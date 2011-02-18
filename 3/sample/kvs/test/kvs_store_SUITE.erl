-module(kvs_store_SUITE).

-compile(export_all).

-include("ct.hrl").
-include("../include/kvs_test.hrl").

sequences() -> [{seq, [
  start_ram,
  start_disc,
  get,
  set,
  delete,
  add,
  replace,
  cas,
  expire
]}].

all() -> [{sequence, seq}].

init_per_testcase(expire, Config) ->
  erljob:start(),
  kvs_store:start_link(false, self),
  Config;

init_per_testcase(
  TestCase, Config
) when TestCase =:= start_ram; TestCase =:= start_disc ->
  Config;

init_per_testcase(_TestCase, Config) ->
  kvs_store:start_link(false, self),
  Config.

end_per_testcase(expire, _Config) ->
%  kvs_store:stop(),
  erljob:stop(),
  ok;

end_per_testcase(_TestCase, _Config) ->
%  kvs_store:stop(),
  ok.

%% Test Cases
start_ram(_Config) ->
  do_start(false, self).

start_disc(_Config) ->
  do_start(true, self),
  mnesia:stop(),
  ?assertMatch(mnesia:system_info(use_dir), true),
  ?assertMatch(mnesia:delete_schema([node()]), ok).

get(_Config) ->
  get_undef(foo).

set(_Config) ->
  do_set(foo, 1, bar),
  do_get(foo, 1, bar).

delete(_Config) ->
  do_set(foo, 1, bar),
  do_get(foo, 1, bar),
  do_delete(foo),
  get_undef(foo).

add(_Config) ->
  do_add(foo, 1, bar),
  do_get(foo, 1, bar),
  do_add(foo, 2, baz, {error, exists}).

replace(_Config) ->
  do_replace(foo, 1, bar, {error, undefined}),
  do_set(foo, 1, bar),
  do_replace(foo, 2, baz),
  do_get(foo, 2, baz).

cas(_Config) ->
  do_set(foo, 1, bar),
  {ok, {1, Unique, bar}} = kvs_store:get(foo),
  do_set(foo, 2, Unique, baz),
  do_get(foo, 2, baz),
  do_set(foo, 3, Unique, quu, {error, exists}),
  do_delete(foo),
  do_set(foo, 3, Unique, quu, {error, undefined}).

expire(_Config) ->
  do_set_expire(foo, 1, 1, bar),
  do_get(foo, 1, bar),
  timer:sleep(1500),
  get_undef(foo).

%% Internal Functions
do_start(PersistentMode, MasterNode) ->
  ?assertMatch(kvs_store:start_link(PersistentMode, MasterNode), {ok, _Pid}).

do_set(Key, Flag, Value) ->
  do_set(Key, Flag, any, Value).

do_set(Key, Flag, Unique, Value) ->
  do_set(Key, Flag, Unique, Value, ok).

do_set(Key, Flag, Unique, Value, Match) ->
  ?assertMatch(kvs_store:set(Key, Flag, infinity, Unique, Value), Match).

do_set_expire(Key, Flag, Expire, Value) ->
  ?assertMatch(kvs_store:set(Key, Flag, Expire, any, Value), ok).

do_get(Key, Flag, Value) ->
  ?assertMatch(kvs_store:get(Key), {ok, {Flag, _Unique, Value}}).

get_undef(Key) ->
  ?assertMatch(kvs_store:get(Key), {error, undefined}).

do_delete(Key) ->
  ?assertMatch(kvs_store:delete(Key), ok).

do_add(Key, Flag, Value) ->
  do_add(Key, Flag, Value, ok).

do_add(Key, Flag, Value, Match) ->
  ?assertMatch(kvs_store:add(Key, Flag, infinity, Value), Match).

do_replace(Key, Flag, Value) ->
  do_replace(Key, Flag, Value, ok).

do_replace(Key, Flag, Value, Match) ->
  ?assertMatch(kvs_store:replace(Key, Flag, infinity, Value), Match).


