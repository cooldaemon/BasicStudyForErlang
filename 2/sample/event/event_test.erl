-module(event_test).
-author('cooldaemon@gmail.com').

-export([test/0]).

test() ->
  {ok, EM} = event_manager:start(),

  [Add, Delete] = lists:map(
    fun (Fun) -> fun (Args) ->
      Result = apply(event_manager, Fun, Args ++ [EM]),
      timer:sleep(500),
      Result
    end end,
    [add, delete]
  ),

  {ok, []} = EM:handlers(),
  ok = EM:notify({event, foo}),

  ok = Add([event_handler1, {}]),
  {ok, [{event_handler1, {}}]} = EM:handlers(),
  ok = EM:notify({event, bar}),

  ok = Add([event_handler1, {}]),
  {ok, [{event_handler1, {}}]} = EM:handlers(),

  ok = Add([event_handler2, 5]),
  {ok, [{event_handler2, 5}, {event_handler1, {}}]} = EM:handlers(),
  lists:foreach(fun (_X) -> ok = EM:notify({event, baz}) end, lists:seq(1, 6)),
  timer:sleep(500),
  {ok, [{event_handler1, {}}]} = EM:handlers(),

  ok = Delete([event_handler1]),
  {ok, []} = EM:handlers(),

  ok = Delete([event_handler1]),
  {ok, []} = EM:handlers(),

  ok = EM:stop().

