-module(my_module).
-author('cooldaemon@gmail.com').

-export([greet/0, greet/1]).

greet() ->
  greet("Erlang").

greet(Target) ->
  IO:fwrite("Hello ~s~n", [Target]).

