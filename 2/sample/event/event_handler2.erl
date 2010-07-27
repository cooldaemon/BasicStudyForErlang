-module(event_handler2).
-author('cooldaemon@gmail.com').

-behaviour(event_manager).

-export([init/1, handle_event/2]).

init(Args) when is_integer(Args) ->
  {ok, Args};
init(_Args) ->
  {ok, 0}.

handle_event(10, _Event) ->
  {error, over_count};
handle_event(State, _Event) ->
  {ok, State+1}.

