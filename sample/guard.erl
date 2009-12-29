-module(guard).
-author('cooldaemon@gmail.com').

-export([abs1/1, abs2/1, dog_or_cat/1]).

abs1(X) when is_integer(X), 0 =< X -> X;
abs1(X) when is_integer(X)         -> -X;
abs1(X)                            -> 0.

abs2(X) ->
  if
    is_intger(X), 0 =<X -> X;
    is_intger(X)        -> -X;
    true                -> 0
  end.

dog_or_cat (X) when X =:= dog; X =:= cat -> true;
dog_or_cat (X)                           -> false.

