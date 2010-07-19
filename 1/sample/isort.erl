-module(isort).
-author('cooldaemon@gmail.com').

-export([insert1/2, isort1/1]).
-export([insert2/2, isort2/1]).
-export([isort3/1, isort4/1]).
-export([test/1]).

insert1(X, [])                       -> [X];
insert1(X, [Y | _YS]=XS) when X =< Y -> [X | XS];
insert1(X, [Y | YS])                 -> [Y | insert1(X, YS)].

isort1([])       -> [];
isort1([X | XS]) -> insert1(X, isort1(XS)).

insert2(X, XS) -> insert2(X, XS, []).
insert2(X, [], Acc)                       -> lists:reverse([X | Acc]);
insert2(X, [Y | _YS]=XS, Acc) when X =< Y -> lists:reverse([X | Acc]) ++ XS;
insert2(X, [Y | YS], Acc)                 -> insert2(X, YS, [Y | Acc]).

isort2([])       -> [];
isort2([X | XS]) -> insert2(X, isort2(XS)).

isort3(XS)            -> isort3(XS, []).
isort3([], Acc)       -> Acc;
isort3([X | XS], Acc) -> isort3(XS, insert1(X, Acc)).

isort4(XS)            -> isort4(XS, []).
isort4([], Acc)       -> Acc;
isort4([X | XS], Acc) -> isort4(XS, insert2(X, Acc)).

test(N) ->
  lists:map(
    fun (XS) ->
      lists:map(
        fun (F) ->
          bench:mark(fun () -> F(XS) end)
        end,
        [fun isort1/1, fun isort2/1, fun isort3/1, fun isort4/1]
      )
    end,
    [
      lists:seq(1, N),
      lists:seq(N, 1, -1),
      lists:map(fun (_X) -> random:uniform(N) end, lists:seq(1, N))
    ]
  ).

