%%%===================================================================
%%% Macros
%%%===================================================================

-define(SOCKET_TIMEOUT, 600*100).
-define(COMMAND_HELP, io_lib:format("
IRC Commands:
-----------------------------------------------------------------------
create_channel!      $Name        -- Create a new chat channel
close_channel!       $Name        -- Close a channel you own
set_name!            $Name        -- Set your username in chat channels
subscribe_channel!   $Name        -- Subscribe to chat channel
unsubscribe_channel! $Name        -- Unsubscribe from chat channel
msg_channel!         $Name | $Msg -- Message a chat channel
-----------------------------------------------------------------------
", [])).
-define(TIMEOUT_MSG,
        io_lib:format("Disconnected from server, please reconnect~n", [])).
-define(QUIT_MSG,
       io_lib:format("Quitting Server~n", [])).
-define(INVALID_COMMAND(Command),
        io_lib:format("Unknown command '~s', type 'help!' to see a valid list~nof commands~n", [Command])).
