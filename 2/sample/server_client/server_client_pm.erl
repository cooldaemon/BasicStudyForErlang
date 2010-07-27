%% @author Masahito Ikuta <cooldaemon@gmail.com> [http://d.hatena.ne.jp/cooldaemon/]
%% @copyright Masahito Ikuta 2010
%% @doc This Module is a sample for parameterized modules in server/client.

-module(server_client_pm).
-author('cooldaemon@gmail.com').
-twitter('@cooldaemon').

-export([start/0, start/1, stop/1]).
-export([put/3, get/2]).
-export([init/0]).
-export([test/0, test/1]).

-record(?MODULE, {pid}).

% I/F
start() ->
  {ok, #?MODULE{pid = spawn(?MODULE, init, [])}}.

start(Node) ->
  {ok, #?MODULE{pid = spawn(Node, ?MODULE, init, [])}}.

stop(THIS) ->
  THIS#?MODULE.pid ! stop,
  ok.

put(Key, Value, THIS) ->
  THIS#?MODULE.pid ! {put, Key, Value},
  ok.

get(Key, THIS) ->
  Ref = make_ref(),
  THIS#?MODULE.pid ! {get, self(), Ref, Key},
  receive
    {Ref, Value} ->
      {ok, Value}
  after 1000 ->
      {error, timeout}
  end.

% Callback
init() ->
  loop().

% Event Loop
loop() ->
  receive
    {put, Key, Value} ->
      erlang:put(Key, Value),
      loop();
    {get, Pid, Ref, Key} ->
      Pid ! {Ref, erlang:get(Key)},
      loop();
    stop ->
      ok;
    _ ->
      loop()
  end.

% Test
test() ->
  {ok, SC} = ?MODULE:start(),
  run_test(SC).

test(Node) ->
  {ok, SC} = ?MODULE:start(Node),
  run_test(SC).

run_test(SC) ->
  [Put, Stop] = lists:map(
    fun (Fun) -> fun (Args) ->
      Result = apply(?MODULE, Fun, Args ++ [SC]),
      timer:sleep(500),
      Result
    end end,
    [put, stop]
  ),

  {ok, undefined} = SC:get(foo),
  ok = Put([foo, {bar, baz}]),
  {ok, {bar, baz}} = SC:get(foo),
  ok = Stop([]),
  {error, timeout} = SC:get(foo),
  ok.

