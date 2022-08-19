%%%===================================================================
%%% Macros
%%%===================================================================

-define(SOCKET_TIMEOUT, 60*100).

-define(TIMEOUT_MSG, 
        io_lib:format("Disconnected from server, please reconnect~n", [])).
