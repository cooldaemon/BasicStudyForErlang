-module(kvs).
-author('cooldaemon@gmail.com').

-export([start/0, stop/0]).

-include("kvs.hrl").
-include("tcp_server.hrl").

-spec start() -> result().
-spec stop()  -> result().

start() -> application:start(kvs).
stop()  -> application:stop(kvs).
