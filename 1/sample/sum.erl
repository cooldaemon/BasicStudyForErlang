-module(sum).
-author('cooldaemon@gmail.com').

-export([sum1/1, sum2/1]).
-export([test/1]).

sum1([])                         -> 0;
sum1([X | XS]) when is_number(X) -> X + sum1(XS);
sum1([_X | XS])                  -> sum1(XS).
 
sum2(XS)                              -> sum2(XS, 0).
sum2([], Acc)                         -> Acc;
sum2([X | XS], Acc) when is_number(X) -> sum2(XS, X + Acc);
sum2([_X | XS], Acc)                  -> sum2(XS, Acc).
 
test(N) ->
  Args = lists:seq(1, N),
  lists:map(
    fun (F) -> bench:mark(fun () -> F(Args) end) end,
    [fun sum1/1, fun sum2/1]
  ).

