-module(send_message_vs_call_function).
-author('cooldaemon@gmail.com').

-export([test/1]).
-export([receive_loop/0]).
-export([send_message/2, call_function/1]).

test (Count) ->
  Pid = spawn(?MODULE, receive_loop, []),
  [{MsgTime, ok}, {FunTime, ok}] = lists:map(
    fun ({Name, Args}) -> timer:tc(?MODULE, Name, Args) end,
    [
      {send_message, [Count, Pid]},
      {call_function, [Count]}
    ]
  ),
  {MsgTime, FunTime}.

receive_loop () ->
  receive {Pid, Msg} ->
    io:fwrite("~p~n", [Msg]),
    Pid ! {self(), ok}
  end,
  receive_loop().

send_message (0, _Pid) ->
  ok;
send_message (Count, Pid) ->
  Pid ! {self(), foo},
  receive {Pid, ok} -> ok end,
  send_message(Count - 1, Pid).

call_function (0) ->
  ok;
call_function (Count) ->
  say_message(foo),
  call_function(Count - 1).

say_message (Msg) ->
  io:fwrite("~p~n", [Msg]).

