-module(my_supervisor).
-author('cooldaemon@gmail.com').
-twitter('@cooldaemon').

-export([start/0, stop/0]).
-export([start_worker/3, workers/0]).
-export([init/0]).

% I/F
start() ->
  register(?MODULE, spawn(?MODULE, init, [])),
  ok.

stop() ->
  exit(whereis(?MODULE), kill),
  ok.

start_worker(Mod, Fun, Args) ->
  call_server(start_child, {Mod, Fun, Args}).

workers() ->
  call_server(workers, {}).

call_server(Command, Message) ->
  Ref = make_ref(),
  ?MODULE ! {Command, self(), Ref, Message},
  receive
    {Ref, Value} ->
      Value
  after 1000 ->
      {error, timeout}
  end.

% Callback
init() ->
  process_flag(trap_exit, true),
  loop([]).

% Event Loop
loop(Workers) ->
  receive
    {'EXIT', WorkerPid, normal} ->
      loop(lists:keydelete(WorkerPid, 1, Workers));
    {'EXIT', WorkerPid, {undef, _Args}} ->
      loop(lists:keydelete(WorkerPid, 1, Workers));
    {'EXIT', WorkerPid, _Reason} ->
      loop(restart_child(WorkerPid, Workers));
    {start_child, Pid, Ref, MFA={Mod, Fun, Args}} ->
      WorkerPid = spawn_link(Mod, Fun, Args),
      Pid ! {Ref, ok},
      loop([{WorkerPid, MFA} | Workers]);
    {workers, Pid, Ref, _Message} ->
      Pid ! {Ref, {ok, Workers}},
      loop(Workers);
    _ ->
      loop(Workers)
  end.

restart_child(WorkerPid, Workers) ->
  case lists:keyfind(WorkerPid, 1, Workers) of
    {WorkerPid, MFA={Mod, Fun, Args}} ->
      NewWorkerPid = spawn_link(Mod, Fun, Args),
      [{NewWorkerPid, MFA} | Workers];
    _Other ->
      io:fwrite("Worker(~p) not found.~n", [WorkerPid]),
      Workers
  end.

