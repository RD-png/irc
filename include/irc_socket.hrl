%%%===================================================================
%%% Macros
%%%===================================================================

-define(SOCKET_TIMEOUT, 600*100).
-define(TIMEOUT_MSG,
        io_lib:format("Disconnected from server, please reconnect~n", [])).
-define(QUIT_MSG,
       io_lib:format("Quitting Server~n", [])).
-define(INVALID_COMMAND(Command),
        io_lib:format("Unknown command '~s', type 'help!' to see a valid list~nof commands~n", [Command])).
