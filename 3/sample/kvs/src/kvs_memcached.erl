-module(kvs_memcached).
-author('cooldaemon@gmail.com').

-behaviour(tcp_server).

-export([init/1, handle_call/3]).

-include("kvs.hrl").
-include("tcp_server.hrl").

-define(TIMEOUT, 5000).

-type(kvs_state()::{}).

-spec init(any()) -> {ok, kvs_state()}.
-spec handle_call(port(), packet(), kvs_state()) ->
  {close | reply, packet(), kvs_state()}.

-spec parse(packet()) -> [binary(), ...].
-spec dispatch(port(), [binary(),...], kvs_state()) ->
  {close, any(), kvs_state()} | {noreply, kvs_state()} | {reply, any(), kvs_state()}.

-spec validate(port(), [binary(), ...], binary()) -> {any(), binary()}.
-spec validate(binary(),binary())                 -> {binary(), infinity | integer()}.

-spec validate_keys([binary(), ...]) -> any().
-spec validate_key(binary())         -> binary().
-spec validate_flags(binary())       -> integer().
-spec do_validate_flags(integer())   -> integer().
-spec validate_expire(binary())      -> infinity | integer().
-spec validate_unique(binary())      -> integer().
-spec binary_to_integer(string(), binary()) -> integer().

-spec recv_value(port(), binary())                 -> binary().
-spec setopts(port(), [{packet, line | raw}, ...]) -> ok.
-spec recv(port(), integer())                      ->  binary().

-spec do_get(binary(), boolean(), kvs_state()) -> {reply , any(), kvs_state()}.
-spec make_get_response(binary(), boolean(), {integer(), integer(), binary()}) -> [
  string(), ...
].
-spec do_set({{binary(), integer(), infinity | integer()}, binary()}, kvs_state()) ->
  {reply, binary(), kvs_state()}.
-spec do_add({{binary(), integer(), infinity | integer()}, binary()}, kvs_state()) ->
  {reply, binary(), kvs_state()}.
-spec do_replace(
  {{binary(), integer(), infinity | integer()}, binary()}, kvs_state()
) -> {reply, binary(), kvs_state()}.
-spec do_cas(
  {{{binary(), integer(), infinity | integer()}, integer()}, binary()}, kvs_state()
) -> {reply, binary(), kvs_state()}.
-spec do_delete(
  {binary(), now | integer()}, kvs_state()
) -> {reply, binary(), kvs_state()}.

init(_Args) ->
  {ok, {}}.

handle_call(Socket, Data, State) ->
  try
    dispatch(Socket, parse(Data), State)
  catch
    throw:command_error ->
      {reply, <<"ERROR\r\n">>, State};
    throw:{client_error, Message} ->
      {reply, ["CLIENT_ERROR ", Message, "\r\n"], State};
    throw:{server_error, Message} ->
      error_logger:warning_msg("~p", [Message]),
      {close, ["SERVER_ERROR ", Message, "\r\n"], State}
  end.

parse(Data) ->
  binary:split(Data, [<<" ">>, <<"\r">>, <<"\n">>], [global, trim]).

dispatch(_Socket, [<<"get">>, Key], State) ->
  do_get(validate_key(Key), false, State);
dispatch(_Socket, [<<"gets">>, Key], State) ->
  do_get(validate_key(Key), true,  State);

dispatch(Socket, [<<"set">>, _Key, _Flags, _Expire, Bytes] = Keys, State) ->
  do_set(validate(Socket, Keys, Bytes), State);

dispatch(Socket, [<<"set">>, _Key, _Flags, _Expire, Bytes, <<"noreply">>] = Keys, State) ->
  do_set(validate(Socket, Keys, Bytes), State),
  {noreply, State};

dispatch(Socket, [<<"add">>, _Key, _Flags, _Expire, Bytes] = Keys, State) ->
  do_add(validate(Socket, Keys, Bytes), State);
dispatch(Socket, [<<"add">>, _Key, _Flags, _Expire, Bytes, <<"noreply">>] = Keys, State) ->
  do_add(validate(Socket, Keys, Bytes), State),
  {noreply, State};

dispatch(Socket, [<<"replace">>, _Key, _Flags, _Expire, Bytes] = Keys, State) ->
  do_replace(validate(Socket, Keys, Bytes), State);
dispatch(Socket, [<<"replace">>, _Key, _Flags, _Expire, Bytes, <<"noreply">>] = Keys, State) ->
  do_replace(validate(Socket, Keys, Bytes), State),
  {noreply, State};

dispatch(Socket, [<<"cas">>, _Key, _Flags, _Expire, Bytes, _Unique] = Keys, State) ->
  do_cas(validate(Socket, Keys, Bytes), State);
dispatch(Socket, [<<"cas">>, _Key, _Flags, _Expire, Bytes, _Unique, <<"noreply">>] = Keys, State) ->
  do_cas(validate(Socket, Keys, Bytes), State),
  {noreply, State};

dispatch(_Socket, [<<"delete">>, Key], State) ->
  do_delete({validate_key(Key), now}, State);
dispatch(_Socket, [<<"delete">>, Key, <<"noreply">>], State) ->
  do_delete({validate_key(Key), now}, State),
  {noreply, State};
dispatch(_Socket, [<<"delete">>, Key, Expire], State) ->
  do_delete(validate(Key, Expire), State);
dispatch(_Socket, [<<"delete">>, Key, Expire, <<"noreply">>], State) ->
  do_delete(validate(Key, Expire), State),
  {noreply, State};
  
dispatch(_Socket, [<<"stats">>], State) ->
  {reply, [
    "STAT curr_connections "
      ++ integer_to_list(tcp_server:info(?MODULE, curr_connections))
      ++ "\r\n",
    "END\r\n"
  ], State};

dispatch(_Socket, [<<"version">>], State) ->
  {reply, <<"VERSION 0.0.1\r\n">>, State};

dispatch(_Socket, [<<"quit">>], State) ->
  {close, State};

dispatch(_Socket, _Unknown, _State) ->
  throw(command_error).

validate(Socket, Keys, Bytes) ->
  {
    validate_keys(Keys),
    recv_value(Socket, Bytes)
  }.

validate(Key, Expire) ->
  {
    validate_key(Key),
    validate_expire(Expire)
  }.

validate_keys([<<"cas">>, Key, Flags, Expire, Bytes, Unique, _AsyncFlag]) ->
  validate_keys([<<"cas">>, Key, Flags, Expire, Bytes, Unique]);
validate_keys([<<"cas">>, Key, Flags, Expire, Bytes, Unique]) ->
  {
    validate_keys([<<"cas">>, Key, Flags, Expire, Bytes]),
    validate_unique(Unique)
  };
validate_keys([Command, Key, Flags, Expire, Bytes, _AsyncFlag]) ->
  validate_keys([Command, Key, Flags, Expire, Bytes]);
validate_keys([_Command, Key, Flags, Expire, _Bytes]) ->
  {
    validate_key(Key),
    validate_flags(Flags),
    validate_expire(Expire)
  }.

validate_key(Key) when byte_size(Key) =< 250 ->
  Key;
validate_key(_Key) ->
  throw({client_error, "Invalid value for key"}).

validate_flags(Flags) ->
  do_validate_flags(binary_to_integer("flags", Flags)).

do_validate_flags(Flags) when 0 =< Flags, Flags =< 65535 ->
  Flags;
do_validate_flags(_Flags) ->
  throw({client_error, "Invalid value for flags"}).

validate_expire(<<"0">>) ->
  infinity;
validate_expire(Expire) when byte_size(Expire) =< 10 ->
  binary_to_integer("exptime", Expire);
validate_expire(_Expire) ->
  throw({client_error, "Invalid value for exptime"}).

validate_unique(Unique) ->
  binary_to_integer("cas unique", Unique).
  
binary_to_integer(Name, Binary) ->
  try
    list_to_integer(binary_to_list(Binary))
  catch
    error:badarg -> throw({client_error, "Invalid value for " ++ Name})
  end.

recv_value(Socket, Bytes) ->
  setopts(Socket, [{packet, raw}]),
  RecvData = recv(Socket, binary_to_integer("bytes", Bytes)),
  recv(Socket, 2),
  setopts(Socket, [{packet, line}]),
  RecvData.

setopts(Socket, Options) ->
  case inet:setopts(Socket, Options) of
    ok              -> ok;
    {error, Reason} -> throw({server_error, Reason})
  end.

recv(Socket, Bytes) ->
  case gen_tcp:recv(Socket, Bytes, ?TIMEOUT) of
    {ok, Value}      -> Value;
    {error, _Reason} -> throw({client_error, "Didn't receive data block."})
  end.

do_get(Key, WithUnique, State) ->
  case kvs_store:get(Key) of
    {ok, {_Flags, _Unique, _Value} = Data} ->
      {reply, make_get_response(Key, WithUnique, Data), State};
    {error, undefined} ->
      {reply, <<"END\r\n">>, State};
    _Other ->
      throw({server_error, "Failed to set."})
  end.

make_get_response(Key, WithUnique, {Flags, Unique, Value}) ->
  [
    "VALUE ",
    io_lib:format("~s ~w ~w", [Key, Flags, byte_size(Value)]),
    case WithUnique of
      true -> io_lib:format(" ~w", [Unique]);
      _    -> []
    end,
    "\r\n",
    Value,
    "\r\nEND\r\n"
  ].

do_set({{Key, Flags, Expire}, Value}, State) ->
  case kvs_store:set(Key, Flags, Expire, any, Value) of
    ok     -> {reply, <<"STORED\r\n">>, State};
    _Other -> throw({server_error, "Failed to set."})
  end.

do_add({{Key, Flags, Expire}, Value}, State) ->
  case kvs_store:add(Key, Flags, Expire, Value) of
    ok              -> {reply, <<"STORED\r\n">>, State};
    {error, exists} -> {reply, <<"NOT_STORED\r\n">>, State};
    _Other          -> throw({server_error, "Failed to add."})
  end.

do_replace({{Key, Flags, Expire}, Value}, State) ->
  case kvs_store:replace(Key, Flags, Expire, Value) of
    ok                 -> {reply, <<"STORED\r\n">>, State};
    {error, undefined} -> {reply, <<"NOT_STORED\r\n">>, State};
    _Other             -> throw({server_error, "Failed to replace."})
  end.

do_cas({{{Key, Flags, Expire}, Unique}, Value}, State) ->
  case kvs_store:set(Key, Flags, Expire, Unique, Value) of
    ok                 -> {reply, <<"STORED\r\n">>, State};
    {error, exists}    -> {reply, <<"EXISTS\r\n">>, State};
    {error, undefined} -> {reply, <<"NOT_FOUND\r\n">>, State};
    _Other             -> throw({server_error, "Failed to set."})
  end.

do_delete({Key, Expire}, State) ->
  case kvs_store:delete(Key, Expire) of
    ok                 -> {reply, <<"DELETED\r\n">>, State};
    {error, undefined} -> {reply, <<"NOT_FOUND\r\n">>, State};
    _Other             -> throw({server_error, "Failed to delete."})
  end.

