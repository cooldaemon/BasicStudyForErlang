-module(pattern_match).
-author('cooldaemon@gmail.com').

-export([dog_or_cat/1, dog_or_cat2/1]).
-export([fst/1, snd/1]).
-export([car/1, cdr/1]).

dog_or_cat(dog)    -> true;
dog_or_cat(cat)    -> true;
dog_or_cat(_Other) -> false.

dog_or_cat2(X) ->
  case X of
    dog    -> true;
    cat    -> true;
    _Other -> false
  end.

fst({X, _Y}) -> X.
snd({_X, Y}) -> Y.

car([X|_XS]) -> X.
cdr([_X|XS]) -> XS.

