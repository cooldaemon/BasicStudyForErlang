-module(kvs_store).
-author('cooldaemon@gmail.com').

-behaviour(gen_server).

-export([start_link/2, stop/0]).
-export([get/1, set/5, add/4, replace/4, delete/1, delete/2]).
-export([
  init/1,
  handle_call/3, handle_cast/2, handle_info/2,
  terminate/2, code_change/3
]).

-include("kvs.hrl").
-include("tcp_server.hrl").

-record(store, {key, flags, expire, unique, value}).

-type(init_args()::{boolean(), self | node()}).
-type(value()::{integer(), integer(), any()}).
-type(expire()::infinity | integer()).
-type(copy_type()::disc_copies | ram_copies).

-spec start_link(boolean(), self | node()) -> {ok, pid()} | ignore | error().
-spec stop()                               -> any().

-spec get(any()) -> {ok, value()} | {error, undefined | bad_record}.
-spec set(
  any(), integer(), integer(), any | integer(), any()
) -> ok | {error, undefined}.
-spec add(any(), integer(), expire(), any())     -> ok | {error, exists}.
-spec replace(any(), integer(), expire(), any()) -> ok | {error, undefined}.
-spec delete(any(), 'now' | integer())           -> ok | {error, undefined}.
  
-spec init(init_args())             -> {ok, {}} | {error, any()}.
-spec handle_call(any(), pid(), {}) -> {reply, ok, {}} | {stop, normal, {}}.
-spec handle_cast(any(), {})        -> {noreply, {}}.
-spec handle_info(any(), {})        -> {noreply, {}}.
-spec terminate(any(), {})          -> ok.
-spec code_change(any(), {}, any()) -> {ok, {}}.

-spec start_mnesia(init_args())                           -> ok.
-spec do_start_mnesia()                                   -> ok.
-spec create_schema()                                     -> ok.
-spec delete_schema()                                     -> ok.
-spec do_delete_schema()                                  -> ok.
-spec ping(node())                                        -> ok.
-spec change_config(node())                               -> ok.
-spec create_table(copy_type())                           -> ok.
-spec change_table_copy_type(schema | store, copy_type()) -> ok.
-spec add_table_copy(copy_type())                         -> ok.
-spec copy_type(boolean())                                -> copy_type().
-spec tab_def(copy_type()) -> [
    {attributes,  [expire | flags | key | unique | value, ...]}
  | {disc_copies, [node()]}, ...
].
-spec copy_tables(copy_type())      -> ok.
-spec change_copy_type(copy_type()) -> ok.

-spec retrieval(any(), read | write)     -> {ok, value()} | {error, undefined}.
-spec micro_expire(infinity | integer()) -> infinity | integer().
-spec microsecs()                        -> integer().
-spec row(any(), integer(), infinity | integer(), integer(), any()) -> #store{}.
-spec transaction(
  fun(() -> ok | {ok, value()} | {error, undefined | exists})
) -> ok | {ok, value()} | {error, undefined | exists | bad_record}.
-spec set_expire(
  any(), infinity | integer(), ok | {error, undefined | exists | bad_record}
) -> ok | {error, undefined | exists | bad_record}.

%% I/F
start_link(PersistentMode, MasterNode) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, {PersistentMode, MasterNode}, []).

stop() -> % it's used when stand alone.
  gen_server:call(?MODULE, stop).

get(Key) ->
  transaction(fun() -> retrieval(Key, read) end).

set(Key, Flags, Expire, any, Value) ->
  set_expire(Key, Expire, transaction(fun() ->
    case retrieval(Key, write) of
      {ok, {_OldFlags, OldUnique, _OldValue}} ->
        mnesia:write(row(Key, Flags, Expire, OldUnique, Value));
      {error, undefined} ->
        mnesia:write(row(Key, Flags, Expire, 0, Value))
    end
  end));
set(Key, Flags, Expire, Unique, Value) ->
  set_expire(Key, Expire, transaction(fun() ->
    case retrieval(Key, write) of
      {ok, {_OldFlags, Unique, _OldValue}} ->
        mnesia:write(row(Key, Flags, Expire, Unique, Value));
      {ok, {_OldFlags, _OldUnique, _OldValue}} ->
        mnesia:abort(exists);
      {error, undefined} ->
        mnesia:abort(undefined)
    end
  end)).

add(Key, Flags, Expire, Value) ->
  set_expire(Key, Expire, transaction(fun() ->
    case retrieval(Key, write) of
      {ok, {_OldFlags, _OldUnique, _OldValue}} ->
        mnesia:abort(exists);
      {error, undefined} ->
        mnesia:write(row(Key, Flags, Expire, 0, Value))
    end
  end)).

replace(Key, Flags, Expire, Value) ->
  set_expire(Key, Expire, transaction(fun() ->
    case retrieval(Key, write) of
      {ok, {_OldFlags, OldUnique, _OldValue}} ->
        mnesia:write(row(Key, Flags, Expire, OldUnique, Value));
      {error, undefined} ->
        mnesia:abort(undefined)
    end
  end)).

delete(Key, now) ->
  transaction(fun() ->
    case retrieval(Key, write) of
      {ok, {_OldFlags, _OldUnique, _OldValue}} ->
        mnesia:delete({store, Key});
      {error, undefined} ->
        mnesia:abort(undefined)
    end
  end);
delete(Key, Expire) ->
  SetExpire = Expire * 1000,
  case erljob:add_job(Key, {?MODULE, delete}, Key, SetExpire, 1) of
    ok     -> ok;
    _Exist -> erljob:set_interval(Key, SetExpire)
  end.

delete(Key) ->
  delete(Key, now).

%% Callbacks
init(Args) ->
  process_flag(trap_exit, true),
  try 
    start_mnesia(Args),
    {ok, {}}
  catch
    throw:Reason -> {error, Reason}
  end.

handle_call(stop, _From, State) ->
  {stop, normal, State};
handle_call(_Message, _From, State) ->
  {reply, ok, State}.

handle_cast(_Message, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  mnesia:stop(),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Internal
start_mnesia({false, self}) ->
  delete_schema(),
  do_start_mnesia(),
  create_table(ram_copies);

start_mnesia({true, self}) ->
  case mnesia:system_info(use_dir) of
    true ->
      do_start_mnesia();
    _False ->
      create_schema(),
      do_start_mnesia(),
      create_table(disc_copies)
  end;

start_mnesia({PersistentMode, Node}) ->
  ping(Node),
  delete_schema(),
  do_start_mnesia(),
  change_config(Node),
  copy_tables(copy_type(PersistentMode)).

do_start_mnesia() ->
  case mnesia:start() of
    ok -> ok;
    {ok, Reason} -> throw(Reason)
  end.

create_schema() ->
  case mnesia:create_schema([node()]) of
    ok              -> ok;
    {error, Reason} -> throw(Reason)
  end.

delete_schema() ->
  case mnesia:system_info(use_dir) of
    true   -> do_delete_schema();
    _False -> ok
  end.

do_delete_schema() ->
  case mnesia:delete_schema([node()]) of
    ok              -> ok;
    {error, Reason} -> throw(Reason)
  end.

ping(Node) ->
  case net_adm:ping(Node) of
    pong -> ok;
    pang -> throw(master_node_not_running)
  end.

change_config(Node) ->
  case mnesia:change_config(extra_db_nodes, [Node]) of
    {ok, _Result}   -> ok;
    {error, Reason} -> throw(Reason)
  end.

create_table(CopyType) ->
  case mnesia:create_table(store, tab_def(CopyType)) of
    {atomic, ok}      -> ok;
    {aborted, Reason} -> throw(Reason)
  end.

change_table_copy_type(Name, CopyType) ->
  case mnesia:change_table_copy_type(Name, node(), CopyType) of
    {atomic, ok}      -> ok;
    {aborted, Reason} -> throw(Reason)
  end.

add_table_copy(CopyType) ->
  case mnesia:add_table_copy(store, node(), CopyType) of
    {atomic, ok}      -> ok;
    {aborted, Reason} -> throw(Reason)
  end.

copy_type(true)   -> disc_copies;
copy_type(_Other) -> ram_copies.

tab_def(disc_copies) ->
  [
    {attributes, record_info(fields, store)},
    {disc_copies, [node()]}
  ];
tab_def(_RamCopies) ->
  [{attributes, record_info(fields, store)}].

copy_tables(CopyType) ->
  case CopyType of
    disc_copies -> change_table_copy_type(schema, CopyType);
    _           -> ok
  end,
  case lists:any(
    fun (store) -> true; (_) -> false end,
    mnesia:system_info(local_tables)
  ) of
    false -> add_table_copy(CopyType);
    _True -> change_copy_type(CopyType)
  end.

change_copy_type(CopyType) ->
  Names = [schema, store],
  SortedNames = case CopyType of
    ram_copies -> lists:reverse(Names);
    _          -> Names
  end,
  lists:foreach(fun (Name) ->
    case
      lists:any(
        fun (X) -> case node() of X -> true; _ -> false end end,
        mnesia:table_info(Name, CopyType)
      )
    of
      true   -> ok;
      _False -> change_table_copy_type(Name, CopyType)
    end
  end, SortedNames),
  ok.

retrieval(Key, LockKind) ->
  case mnesia:read(store, Key, LockKind) of
    [] ->
      {error, undefined};
    [{store, Key, Flags, infinity, Unique, Value}] ->
      {ok, {Flags, Unique, Value}};
    [{store, Key, Flags, Expire, Unique, Value}] ->
      Now = microsecs(),
      if
        Expire > Now ->
          {ok, {Flags, Unique, Value}};
        true ->
          {error, undefined}
      end;
    _Records ->
      mnesia:abort(bad_record)
  end.

micro_expire(infinity) ->
  infinity;
micro_expire(Expire) ->
  microsecs() + Expire * 1000000.

microsecs() ->
  {MegaSecs, Secs, MicroSecs} = now(),
  MegaSecs * 1000000000000 + Secs * 1000000 + MicroSecs.

row(Key, Flags, Expire, Unique, Value) ->
  #store{
    key    = Key,
    flags  = Flags,
    expire = micro_expire(Expire),
    unique = Unique + 1,
    value  = Value
  }.

transaction(Fun) ->
  case mnesia:transaction(Fun) of
    {atomic,  Result} -> Result;
    {aborted, Reason} -> {error, Reason}
  end.

set_expire(_Key, infinity, Result) ->
  Result;
set_expire(Key, Expire, ok) ->
  delete(Key, Expire);
set_expire(_Key, _Expire, Result) ->
  Result.

