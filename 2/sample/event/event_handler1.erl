-module(event_handler1).
-author('cooldaemon@gmail.com').

-behaviour(event_manager).

-export([init/1, handle_event/2]).

init(_Args) ->
  {ok, {}}.

handle_event(State, Event) ->
  io:fwrite("handle event:~p~n", [Event]),
  {ok, State}.

