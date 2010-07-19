-module(prime).
-author('cooldaemon@gmail.com').

-export([prime/1]).

prime(N) ->
  [X || X <- lists:seq(2, N),
    (fun (Y) ->
      case [Z || Z <- lists:seq(1, Y), Y rem Z =:= 0] of
        [1, Y] -> true;
        _Other -> false
      end
    end)(X)
  ].

