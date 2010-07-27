-module(event_manager).
-author('cooldaemon@gmail.com').

-export([behaviour_info/1]).

-export([start/0, start/1, stop/1]).
-export([add/3, delete/2, handlers/1, notify/2]).
-export([loop/1]).

-record(?MODULE, {pid}).

% Behaviour Callbacks
behaviour_info(callbacks) -> [{init, 1}, {handle_event, 2}];
behaviour_info(_Other)    -> undefined.

% I/F
start() ->
  {ok, #?MODULE{pid = spawn(?MODULE, loop, [[]])}}.

start(Node) ->
  {ok, #?MODULE{pid = spawn(Node, ?MODULE, loop, [[]])}}.

stop(THIS) ->
  THIS#?MODULE.pid ! stop,
  ok.

add(Handler, Args, THIS) ->
  THIS#?MODULE.pid ! {add, Handler, Args},
  ok.

delete(Handler, THIS) ->
  THIS#?MODULE.pid ! {delete, Handler},
  ok.

handlers(THIS) ->
  Ref = make_ref(),
  THIS#?MODULE.pid ! {handlers, self(), Ref},
  receive
    {Ref, Handlers} ->
      {ok, Handlers}
  after 1000 ->
      {error, timeout}
  end.

notify(Event, THIS) ->
  THIS#?MODULE.pid ! {notify, Event},
  ok.

% Event Loop
loop(Handlers) ->
  receive
    {add, Handler, Args} ->
      loop(add_handler(Handler, Args, Handlers));
    {delete, Handler} ->
      loop(lists:keydelete(Handler, 1, Handlers));
    {handlers, Pid, Ref} ->
      Pid ! {Ref, Handlers},
      loop(Handlers);
    {notify, Event} ->
      loop(call_event_handlers(Event, Handlers, []));
    stop ->
      ok;
    _Other ->
      loop(Handlers)
  end.

add_handler(Handler, Args, Handlers) ->
  case lists:keyfind(Handler, 1, Handlers) of
    false ->
      case call_handler(Handler, init, [Args]) of
        {ok, State} -> [{Handler, State} | Handlers];
        _Error      -> Handlers
      end;
    _Handler ->
      Handlers
  end.

call_event_handlers(_Event, [], NewHandlers) ->
  NewHandlers;
call_event_handlers(Event, [{Handler, State} | Handlers], NewHandlers) ->
  case call_handler(Handler, handle_event, [State, Event]) of
    {ok, NewState} ->
      call_event_handlers(Event, Handlers, [{Handler, NewState} | NewHandlers]);
    _Error ->
      call_event_handlers(Event, Handlers, NewHandlers)
  end.

call_handler(Module, Function, Args) ->
  try
    apply(Module, Function, Args)
  catch
    Type:Reason ->
      io:fwrite("call(~p) ~p", [{Module, Function, Args}, {Type, Reason}]),
      {Type, Reason}
  end.
 
