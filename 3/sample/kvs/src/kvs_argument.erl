-module(kvs_argument).
-author('cooldaemon@gmail.com').

-export([get/1]).

-include("kvs.hrl").
-include("tcp_server.hrl").

-type(command_args()::{ok, integer() | boolean() | node()}).

-spec get(atom())          -> command_args() | error().
-spec get_argument(atom()) -> command_args().

-spec validate_argument(
  {listen_port, string()}
  | {master_node, string()}
  | {max_connections, string()}
  | {persistent_mode, string()}) ->
  command_args().

-spec regex_match(string(), string())          -> ok.
-spec between(integer(), integer(), integer()) -> {ok, integer()}.

get(Key) ->
  try
    validate_argument(get_argument(Key))
  catch
    throw:Reason ->
      {error, Reason};
    error:Reason ->
      io:fwrite("~p: ~p~n", [Key, Reason]),
      error(Reason)
  end.

get_argument(Key) ->
  case init:get_argument(Key) of
    {ok, [[]]}                -> {Key, on};
    {ok, [[Value]]}           -> {Key, Value};
    {ok, [[Value | _Values]]} -> {Key, Value};
    error                     -> throw(undefined);
    _Other                    -> error(badarg)
  end.

validate_argument({listen_port, Port}) ->
  regex_match(Port, "^[0-9]+$"),
  between(list_to_integer(Port), 1024, 65535);

validate_argument({max_connections, Conn}) ->
  regex_match(Conn, "^[0-9]+$"),
  between(list_to_integer(Conn), 1, 1024);

validate_argument({persistent_mode, _On}) ->
  {ok, true};

validate_argument({master_node, Node}) ->
  Atom = list_to_atom(Node),
  case net_adm:ping(Atom) of
    pong -> {ok, Atom};
    _    -> error(badarg)
  end.

regex_match(Value, Regex) ->
  case re:run(Value, Regex) of
    nomatch -> error(badarg);
    _Match  -> ok
  end.

between(Value, Start,  _End) when Value < Start -> error(badarg);
between(Value, _Start,  End) when End   < Value -> error(badarg);
between(Value, _Start, _End)                    -> {ok, Value}.

