-record(tcp_server_option, {
    listen = [{active, false}, binary, {packet, line}, {reuseaddr, true}],
    port                    = 11211,
    max_processes           = 8,
    max_restarts            = 3,
    time                    = 60,
    shutdown                = 2000,
    accept_timeout          = infinity,
    accept_error_sleep_time = 3000,
    recv_length             = 0,
    recv_timeout            = infinity
}).

-define(error  (Data), error_logger:error_msg("~p:~p~n~p", [?FILE, ?LINE, Data])).
-define(warning(Data), error_logger:warning_msg("~p:~p~n~p", [?FILE, ?LINE, Data])).
-define(info   (Data), error_logger:info_msg("~p:~p~n~p", [?FILE, ?LINE, Data])).

