-define(_ASSERT(Expr, Expect), 
  case Expr of
    Expect -> ok;
    Other  -> ct:fail(Other)
  end
).

-define(assert(BoolExpr),
  (fun () ->
    ?_ASSERT(BoolExpr, true)
  end)()
).

-define(assertEqual(Expr, Expect), 
  (fun () ->
    ExpectResult = Expect,
    ?_ASSERT(Expr, ExpectResult)
  end)()
).

-define(assertMatch(Expr, Expect), 
  (fun () ->
    ?_ASSERT(Expr, Expect)
  end)()
).

