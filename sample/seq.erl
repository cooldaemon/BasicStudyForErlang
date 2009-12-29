-module(seq).
-author('cooldaemon@gmail.com').

-export([seq1/2, seq2/2]).
-export([test/1]).

seq1(N, E) when N > E -> [];
seq1(E, E)            -> [E];
seq1(N, E)            -> [N | seq1(N+1, E)].

seq2(N, E) when N > E -> [];
seq2(N, E)            -> seq2(N, E, []).
seq2(N, N, Acc)       -> [N | Acc];
seq2(N, E, Acc)       -> seq2(N, E-1, [E | Acc]).

test(N) ->
  lists:map(
    fun (F) -> bench:mark(fun () -> F(1, N) end) end,
    [fun seq1/2, fun seq2/2]
  ).

