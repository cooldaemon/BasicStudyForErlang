-module(fold).
-author('cooldaemon@gmail.com').

-export([foldl/3, foldr/3]).

foldl(F, Acc, [X|XS]) -> foldl(F, F(X, Acc), XS).
foldr(F, Acc, [X|XS]) -> F(X, foldr(F, Acc, XS)).

