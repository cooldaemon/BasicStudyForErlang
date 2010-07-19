-module(bench).
-author('cooldaemon@gmail.com').

-export([mark/1]).

mark(F) ->
  lists:foreach(fun statistics/1, [runtime, wall_clock]),
  Result = F(),
  [Runtime, WallClock] = lists:map(
    fun (Type) -> {_, T} = statistics(Type), T end,
    [runtime, wall_clock]
  ),
  {Result, Runtime, WallClock}.

