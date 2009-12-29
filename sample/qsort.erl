-module(qsort).
-author('cooldaemon@gmail.com').

-export([qsort/1]).

qsort([]) ->
  [];
qsort([X | XS]) ->
  qsort([Y || Y <- XS, Y =< X]) ++ [X] ++ qsort([Z || Z <- XS, Z > X]).

