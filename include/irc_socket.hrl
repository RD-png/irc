%%%===================================================================
%%% Macros
%%%===================================================================

-define(SOCKET_TIMEOUT, 600*100).
-define(TIMEOUT_MSG,
        io_lib:format("Disconnected from server, please reconnect~n", [])).
-define(INVALID_COMMAND(Command),
        io_lib:format("Unknown command '~p', type 'help!' to see a valid list~nof commands~n", [Command])).
-define(CREATED_CHANNEL(Channel),
       io_lib:format("Channel '~p' has been created~n", [Channel])).
