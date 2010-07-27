%% @author Masahito Ikuta <cooldaemon@gmail.com> [http://d.hatena.ne.jp/cooldaemon/]
%% @copyright Masahito Ikuta 2010

-module(tcp_server).
-author('cooldaemon@gmail.com').
-twitter('@cooldaemon').

-export([behaviour_info/1]).
-export([start_link/1, start_link/2, start_link/3, start_link/4]).
-export([stop/0, stop/1]).
-export([info/1, info/2]).

-include("tcp_server.hrl").

%% Behaviour Callbacks
behaviour_info(callbacks) -> [{init, 1}, {handle_call, 3}];
behaviour_info(_Other)    -> undefined.

%% External APIs
start_link(Mod)       -> start_link(Mod, []).
start_link(Mod, Args) -> start_link(Mod, Args, #tcp_server_option{}).
start_link(Mod, Args, Option) ->
    start_link({local, ?MODULE}, Mod, Args, Option).
start_link(Name, Mod, Args, Option) ->
    tcp_server_sup:start_link(Name, Mod, Args, Option).

stop() -> stop(?MODULE).
stop(Name) ->
    tcp_server_sup:stop(Name).

info(Key) -> info(?MODULE, Key).
info(Name, Key) ->
    tcp_server_monitor:info(
        tcp_server_sup:build_monitor_name(Name), Key
    ).
