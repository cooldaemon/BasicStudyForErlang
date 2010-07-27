%% @author Masahito Ikuta <cooldaemon@gmail.com> [http://d.hatena.ne.jp/cooldaemon/]
%% @copyright Masahito Ikuta 2010
%% @doc This Module is a sample for finite state machine.

-module(finite_state_machine).
-author('cooldaemon@gmail.com').
-twitter('@cooldaemon').

-export([start/0, start/1, stop/1]).
-export([get/1, change/1]).
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
get(THIS) ->
  Ref = make_ref(),
  THIS#?MODULE.pid ! {get, self(), Ref},
  receive
    {Ref, Value} ->
      {ok, Value}
  after 1000 ->
      {error, timeout}
  end.

change(THIS) ->
  THIS#?MODULE.pid ! change,
  ok.

% Callback
init() ->
  state1().

% State 
state1() ->
  receive
    change ->
      state2();
    {get, Pid, Ref} ->
      Pid ! {Ref, state1},
      state1();
    stop ->
      ok;
    _ ->
      state1()
  end.

state2() ->
  receive
    change ->
      state1();
    {get, Pid, Ref} ->
      Pid ! {Ref, state2},
      state2();
    stop ->
      ok;
    _ ->
      state2()
  end.

% Test
test() ->
  {ok, FSM} = ?MODULE:start(),
  run_test(FSM).

test(Node) ->
  {ok, FSM} = ?MODULE:start(Node),
  run_test(FSM).

run_test(SC) ->
  [Change, Stop] = lists:map(
    fun (Fun) -> fun () ->
      Result = apply(?MODULE, Fun, [SC]),
      timer:sleep(500),
      Result
    end end,
    [change, stop]
  ),

  {ok, state1} = SC:get(),
  ok = Change(),
  {ok, state2} = SC:get(),
  ok = Change(),
  {ok, state1} = SC:get(),
  ok = Stop(),
  {error, timeout} = SC:get(),
  ok.

