-module(kvs_sup).
-author('cooldaemon@gmail.com').

-export([start_link/1, stop/0]).
-export([init/1]).

-include("kvs.hrl").
-include("tcp_server.hrl").

-spec start_link(kvs_args()) -> result_start_link().
-spec stop()                 -> result().
-spec init(kvs_args())       -> supervisor_spec().

start_link(Args) ->
  sup_utils:start_link(?MODULE, Args).

stop() ->
  sup_utils:stop(?MODULE).

init({ListenPort, MaxConnections, PersistentMode, MasterNode}) ->
  sup_utils:spec([
    {sup, erljob_sup},
    {sup, tcp_server, [
      {local, kvs_memcached},
      kvs_memcached,
      [],
      #tcp_server_option{
        port          = ListenPort,
        max_processes = MaxConnections
      }
    ]},
  {worker, kvs_store, [PersistentMode, MasterNode]}
  ]).

