-module(kvs_app).
-author('cooldaemon@gmail.com').

-behaviour(application).
-export([start/2, stop/1]).

-include("kvs.hrl").
-include("tcp_server.hrl").

-spec start(normal | {takeover, node()} | {failover, node()}, [any(), ...]) ->
  ignore | error() | {ok, pid()}.
-spec stop(any())  -> ok.
-spec get_params() -> kvs_args().

start(_StartType, _StartArgs) ->
  kvs_sup:start_link(get_params()).
 
stop(_State) ->
  ok.

get_params() ->
  list_to_tuple(lists:map(fun ({Key, DefaultValue}) ->
    case kvs_argument:get(Key) of
      {ok, Value} -> Value;
      _Other      -> DefaultValue
    end
  end, [
    {listen_port,     11211},
    {max_connections, 256},
    {persistent_mode, false},
    {master_node,     self}
  ])).
 
