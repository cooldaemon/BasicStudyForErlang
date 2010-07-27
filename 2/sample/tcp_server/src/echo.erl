-module(echo).
-behaviour(tcp_server).

-export([start_link/0, stop/0]).
-export([init/1, handle_call/3]).

-include("tcp_server.hrl").

%% I/F
start_link() ->
  tcp_server:start_link(
    ?MODULE, [], #tcp_server_option{port = 10000, max_processes = 2}
  ).

stop() ->
  tcp_server:stop().

%% Callbacks
init(_Args) ->
  {ok, {}}.

handle_call(_Socket, <<"bye\r\n">>, State) ->
  {close, <<"cya\r\n">>, State};
handle_call(_Socket, Data, State) ->
  {reply, Data, State}.

