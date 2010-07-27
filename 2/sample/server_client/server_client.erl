%% @author Masahito Ikuta <cooldaemon@gmail.com> [http://d.hatena.ne.jp/cooldaemon/]
%% @copyright Masahito Ikuta 2010
%% @doc This Module is a sample for server/client pattern.

-module(server_client).
-author('cooldaemon@gmail.com').
-twitter('@cooldaemon').

-export([start/0, stop/0]).
-export([put/2, get/1]).
-export([init/0]).
-export([test/0]).

% I/F
start() ->
  register(?MODULE, spawn(?MODULE, init, [])),
  ok.

stop() ->
  ?MODULE ! stop,
  ok.

put(Key, Value) ->
  ?MODULE ! {put, Key, Value},
  ok.

get(Key) ->
  Ref = make_ref(),
  ?MODULE ! {get, self(), Ref, Key},
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
  [Put, Stop] = lists:map(
    fun (Fun) -> fun (Args) ->
      Result = apply(?MODULE, Fun, Args),
      timer:sleep(500),
      Result
    end end,
    [put, stop]
  ),

  ok = ?MODULE:start(),
  {ok, undefined} = ?MODULE:get(foo),
  ok = Put([foo, {bar, baz}]),
  {ok, {bar, baz}} = ?MODULE:get(foo),
  ok = Stop([]),
  ok.

