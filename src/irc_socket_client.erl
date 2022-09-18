%%%-------------------------------------------------------------------
%%% @author Ryan User <ryan@nixos-laptop>
%%% @copyright (C) 2022, Ryan User
%%% @doc
%%%
%%% @end
%%% Created : 21 Aug 2022 by Ryan User <ryan@nixos-laptop>
%%%-------------------------------------------------------------------
-module(irc_socket_client).
-author("ryandenby").

-include("irc.hrl").
-include("irc_socket.hrl").

%% API
-export([handle/2]).

%%%===================================================================
%%% API
%%%===================================================================

-spec handle(Packet, State) -> ok | Msg when
    Packet   :: binary(),
    State    :: client(),
    Msg      :: io_lib:chars().
handle(Packet, State) ->
  CleanPacket = re:replace(Packet, "[\r\n]$", "", [global, {return, binary}]),
  do_handle(CleanPacket, State).

%%%===================================================================
%%% Internal functions
%%%===================================================================

do_handle(<<"help!", _Rest/binary>>, _ClientID) ->
  ?COMMAND_HELP;
do_handle(<<"create_channel! ", ChannelName/binary>>, ClientID) ->
  case irc_server:create_channel(ChannelName, ClientID) of
    ok ->
      io_lib:format("Channel '~s' created~n", [ChannelName]);
    channel_already_registered ->
      io_lib:format("Channel '~s' already registered~n", [ChannelName])
  end;
do_handle(<<"close_channel! ", ChannelName/binary>>, ClientID) ->
  case irc_server:close_channel(ChannelName, ClientID) of
    ok ->
      io_lib:format("Channel '~s' closed~n", [ChannelName]);
    channel_not_registered ->
      io_lib:format("Channel '~s' not registered~n", [ChannelName]);
    channel_non_owner ->
      io_lib:format("Not owner of channel '~s'~n", [ChannelName])
  end;
do_handle(<<"set_name! ", Name/binary>>, ClientID) ->
  case length(binary_to_list(Name)) of
    Length when Length < 3 ->
      io_lib:format("Name too short~n", []);
    Length when Length > 12 ->
      io_lib:format("Name too long~n", []);
    _ValidLength ->
      irc_client:set_name(ClientID, Name),
      io_lib:format("Name has been updated to '~s'~n", [Name])
  end;
do_handle(<<"subscribe_channel! ", ChannelName/binary>>, ClientID) ->
  case irc_server:subscribe_channel(ChannelName, ClientID) of
    ok ->
      io_lib:format("Subscribed to channel '~s'~n", [ChannelName]);
    channel_not_registered ->
      io_lib:format("Channel '~s' not registered~n", [ChannelName]);
    client_already_subscribed ->
      io_lib:format("Already subscribed to channel '~s'~n", [ChannelName])
  end;
do_handle(<<"unsubscribe_channel! ", ChannelName/binary>>, ClientID) ->
  case irc_server:unsubscribe_channel(ChannelName, ClientID) of
    ok ->
      io_lib:format("Unsubscribed from channel '~s'~n", [ChannelName]);
    channel_not_registered ->
      io_lib:format("Channel '~s' not registered~n", [ChannelName]);
    client_not_subscribed ->
      io_lib:format("Client '~p' not subscribed~n", [ClientID])
  end;
do_handle(<<"msg_channel! ", Args/binary>>, ClientID) ->
  case parse_args(Args) of
    [ChannelName, Msg] ->
      case irc_server:msg_channel(ChannelName, Msg, ClientID) of
        ok ->
          ok;
        channel_not_registered ->
          io_lib:format("Channel '~s' not registered~n", [ChannelName]);
        client_not_subscribed ->
          io_lib:format("Client '~p' not subscribed~n", [ClientID])
      end;
    _InvalidArgFormat ->
      io_lib:format("Invalid arg format '~s' ~n", [Args])
  end;
do_handle(Packet, _State) ->
  ?INVALID_COMMAND(Packet).

parse_args(Args) ->
  binary:split(Args, <<" | ">>, [global, trim]).
