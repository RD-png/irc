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

-spec handle(Packet, State) -> Response when
    Packet   :: binary(),
    State    :: client(),
    Response :: ok | Msg,
    Msg      :: io_lib:chars().
handle(Packet, State) ->
  CleanPacket = re:replace(Packet, "[\r\n]$", "",
                           [global, {return, binary}]),
  do_handle(CleanPacket, State).

%%%===================================================================
%%% Internal functions
%%%===================================================================

do_handle(<<"create_channel! ", ChannelName/binary>>, ClientID) ->
  case irc_mnesia:create_channel(ChannelName, ClientID) of
    ok ->
      io_lib:format("Channel '~p' created~n", [ChannelName]);
    channel_already_registered ->
      io_lib:format("Channel '~p' already registered~n", [ChannelName]);
    client_not_registered ->
      io_lib:format("Client '~p' not registered~n", [ClientID])
  end;
do_handle(<<"close_channel! ", ChannelName/binary>>, ClientID) ->
  case irc_mnesia:close_channel(ChannelName, ClientID) of
    ok ->
      io_lib:format("Channel '~p' closed~n", [ChannelName]);
    channel_not_registered ->
      io_lib:format("Channel '~p' not registered~n", [ChannelName]);
    channel_non_owner ->
      io_lib:format("Not owner of channel '~p'~n", [ChannelName])
  end;
do_handle(Packet, _State) ->
  ?INVALID_COMMAND(Packet).
