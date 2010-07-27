%% @author Masahito Ikuta <cooldaemon@gmail.com> [http://d.hatena.ne.jp/cooldaemon/]
%% @copyright Masahito Ikuta 2010

-module(tcp_server_acceptor).
-author('cooldaemon@gmail.com').
-twitter('@cooldaemon').

-export([start_link/6]).
-export([init/6]).

-include("tcp_server.hrl").

%% External APIs
start_link({Dest, Name}, ListenSocket, State, MonitorName, Mod, Option) ->
  {ok, Pid} = proc_lib:start_link(
    ?MODULE, init,
    [self(), ListenSocket, State, MonitorName, Mod, Option]
  ),
  case Dest of
    local   -> register(Name, Pid);
    _Global -> global:register_name(Name, Pid)
  end,
  {ok, Pid}.

%% Callbacks
init(Parent, ListenSocket, State, MonitorName, Mod, Option) ->
  proc_lib:init_ack(Parent, {ok, self()}),
  tcp_server_monitor:register(MonitorName, self()),
  accept(ListenSocket, State, MonitorName, Mod, Option).

%% Internal Functions
accept(ListenSocket, State, MonitorName, Mod, Option) ->
  case gen_tcp:accept(
    ListenSocket, Option#tcp_server_option.accept_timeout
  ) of 
    {ok, Socket} ->
      try
        tcp_server_monitor:increment(MonitorName, self()),
        recv(
          proplists:get_value(
            active, Option#tcp_server_option.listen
          ),
          Socket, State, Mod, Option
        )
      catch
        Type:Reason ->
          ?warning(io_lib:format(
            "accept(~p) ~p", [Mod, {Type, Reason}]
          ))
      after
        tcp_server_monitor:decrement(MonitorName, self()),
        gen_tcp:close(Socket)
      end;
    {error, Reason} ->
      ?warning(io_lib:format(
        "accept(~p) ~p", [Mod, {error, Reason}]
      )),
      timer:sleep(Option#tcp_server_option.accept_error_sleep_time)
  end,
  accept(ListenSocket, State, MonitorName, Mod, Option).

recv(false, Socket, State, Mod, Option) ->
  case gen_tcp:recv(
    Socket,
    Option#tcp_server_option.recv_length,
    Option#tcp_server_option.recv_timeout
  ) of
    {ok, Data} ->
      call_mod(false, Socket, Data, State, Mod, Option);
    {error, closed} ->
      tcp_closed;
    {error, Reason} ->
      ?warning(io_lib:format("recv(~p) ~p", [Mod, {error, Reason}])),
      error
  end;

recv(true, _DummySocket, State, Mod, Option) ->
  receive
    {tcp, Socket, Data} ->
      call_mod(true, Socket, Data, State, Mod, Option);
    {tcp_closed, _Socket} ->
      tcp_closed;
    Error ->
      ?warning(io_lib:format("recv(~p) ~p", [Mod, {error, Error}])),
      error
  after Option#tcp_server_option.recv_timeout ->
    tcp_timeout
  end.
 
call_mod(Active, Socket, Data, State, Mod, Option) ->
  case Mod:handle_call(Socket, Data, State) of
    {reply, DataToSend, State} ->
      gen_tcp:send(Socket, DataToSend),
      recv(Active, Socket, State, Mod, Option);
    {noreply, State} ->
      recv(Active, Socket, State, Mod, Option);
    {close, State} ->
      tcp_closed;
    {close, DataToSend, State} ->
      gen_tcp:send(Socket, DataToSend);
    Other ->
      ?warning(io_lib:format(
        "call_mod(~p) ~p", [Mod, {unexpected_result, Other}]
      ))
  end.

