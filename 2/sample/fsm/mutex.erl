%% @author Masahito Ikuta <cooldaemon@gmail.com> [http://d.hatena.ne.jp/cooldaemon/]
%% @copyright Masahito Ikuta 2010
%% @doc This Module is a sample for mutex semaphore.

-module(mutex).
-author('cooldaemon@gmail.com').
-twitter('@cooldaemon').

-export([start/0, start/1, stop/1]).
-export([lock/1, unlock/1]).
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

% Event
lock(THIS) ->
  Ref = make_ref(),
  THIS#?MODULE.pid ! {lock, self(), Ref},
  receive
    {Ref, ok} ->
      ok
  after 1000 ->
      {error, timeout}
  end.

unlock(THIS) ->
  THIS#?MODULE.pid ! unlock,
  ok.

% Callback
init() ->
  free().

% State 
free() ->
  receive
    {lock, Pid, Ref} ->
      Pid ! {Ref, ok},
      busy();
    stop ->
      ok;
    _ ->
      free()
  end.

busy() ->
  receive
    unlock ->
      free();
    stop ->
      ok
  end.

% Test
test() ->
  {ok, MUTEX} = start(),
  run_test(MUTEX).

test(Node) ->
  {ok, MUTEX} = start(Node),
  run_test(MUTEX).

run_test(MUTEX) ->
  Stop = add_sleep(bind_mutex(MUTEX, fun stop/1)),
  Lock = add_print(bind_mutex(MUTEX, fun lock/1), locked),
  Unlock = add_sleep(add_print(bind_mutex(MUTEX, fun unlock/1), unlocked)),

  ok = Lock(),
  ok = Unlock(),

  ok = Lock(),
  spawn(fun () ->
    ok = Lock(),
    ok = Unlock()
  end),
  ok = Unlock(),

  ok = Stop(),
  {error, timeout} = Lock(),
  ok.

bind_mutex(MUTEX, Fun) ->
  fun () -> Fun(MUTEX) end.

add_sleep(Fun) ->
  fun () ->
    Result = Fun(),
    timer:sleep(500),
    Result
  end.

add_print(Fun, Message) ->
  fun () ->
    Result = Fun(),
    io:fwrite("~p:~p~n", [self(), Message]),
    Result
  end.

