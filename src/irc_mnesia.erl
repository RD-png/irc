%%%-------------------------------------------------------------------
%%% @author Ryan User <ryan@nixos-desktop>
%%% @copyright (C) 2022, Ryan User
%%% Created :  18 Aug 2022 by Ryan User <ryan@nixos-desktop>
%%%-------------------------------------------------------------------
-module(irc_mnesia).
-author("ryandenby").

-include("irc.hrl").

%% API
-export([start/0,
         stop/0,
         create_channel/2,
         close_channel/2,
         subscribe_channel/2,
         msg_channel/3]).

%%%-------------------------------------------------------------------
%% API
%%%-------------------------------------------------------------------

-spec start() -> ok.
start() ->
  ok = irc_client:create_mnesia_table(),
  ok = irc_channel:create_mnesia_table().

-spec stop() -> stopped | {error, Reason} when
    Reason :: term().
stop() ->
  mnesia:stop().

-spec create_channel(ChannelName, ClientID) -> Result when
    ChannelName :: channel_name(),
    ClientID    :: client_id(),
    Result      :: ok
                 | channel_already_registered.
create_channel(ChannelName, ClientID) ->
  Client = irc_client:fetch(ClientID),
  case irc_channel:create(ChannelName, ClientID) of
    ok ->
      irc_client:create_channel(ChannelName, Client);
    channel_already_registered = Err ->
      Err
  end.

-spec close_channel(ChannelName, ClientID) -> Result when
    ChannelName :: channel_name(),
    ClientID    :: client_id(),
    Result      :: ok
                 | channel_non_owner
                 | channel_not_registered.
close_channel(ChannelName, ClientID) ->
  Client = irc_client:fetch(ClientID),
  case irc_channel:fetch(ChannelName) of
    #channel{owner = OwnerID} = Channel when ClientID =:= OwnerID ->
      irc_client:close_channel(ChannelName, Client),
      irc_channel:unregister(Channel);
    #channel{} = _Channel ->
      channel_non_owner;
    _ChannelNotRegistered ->
      channel_not_registered
  end.

-spec subscribe_channel(ChannelName, ClientID) -> Result when
    ChannelName :: channel_name(),
    ClientID    :: client_id(),
    Result      :: ok | channel_not_registered | client_already_subscribed.
subscribe_channel(ChannelName, ClientID) ->
  case irc_channel:fetch(ChannelName) of
    #channel{} = Channel ->
      irc_channel:subscribe(Channel, ClientID);
    _ChannelNotRegistered ->
      channel_not_registered
  end.

-spec msg_channel(ChannelName, Msg, ClientID) -> ok when
    ChannelName :: channel_name(),
    Msg         :: binary(),
    ClientID    :: client_id().
msg_channel(ChannelName, Msg, ClientID) ->
  case irc_channel:fetch(ChannelName) of
    #channel{subscribers = Subscribers} ->
      Client = irc_client:fetch(ClientID),
      dispatch_channel_msg(Subscribers, Msg, Client);
    _ChannelNotRegistered ->
      channel_not_registered
  end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

dispatch_channel_msg(_Subscribers, _Msg, _Client) ->
  ok.
