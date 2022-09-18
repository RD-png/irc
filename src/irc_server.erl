%%%-------------------------------------------------------------------
%%% @author Ryan User <ryan@nixos-desktop>
%%% @copyright (C) 2022, Ryan User
%%% Created :  18 Aug 2022 by Ryan User <ryan@nixos-desktop>
%%%-------------------------------------------------------------------
-module(irc_server).
-author("ryandenby").

-include("irc.hrl").

%% API
-export([start/0,
         stop/0,
         create_channel/2,
         close_channel/2,
         subscribe_channel/2,
         unsubscribe_channel/2,
         msg_channel/3,
         close_client/1]).

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

-spec create_channel(ChannelName, ClientID) ->
        ok | channel_already_registered when
    ChannelName :: channel_name(),
    ClientID    :: client_id().
create_channel(ChannelName, ClientID) ->
  Client = irc_client:fetch(ClientID),
  case irc_channel:create(ChannelName, ClientID) of
    ok ->
      irc_client:create_channel(ChannelName, Client);
    channel_already_registered = Err ->
      Err
  end.

-spec close_channel(ChannelName, ClientID) ->
        ok | channel_non_owner | channel_not_registered when
    ChannelName :: channel_name(),
    ClientID    :: client_id().
close_channel(ChannelName, ClientID) ->
  case irc_channel:fetch(ChannelName) of
    #channel{owner = OwnerID} = Channel when ClientID =:= OwnerID ->
      Client = irc_client:fetch(ClientID),
      irc_client:close_channel(ChannelName, Client),
      irc_channel:unregister(Channel);
    #channel{} = _Channel ->
      channel_non_owner;
    _ChannelNotRegistered ->
      channel_not_registered
  end.

-spec subscribe_channel(ChannelName, ClientID) ->
        ok | channel_not_registered | client_already_subscribed when
    ChannelName :: channel_name(),
    ClientID    :: client_id().
subscribe_channel(ChannelName, ClientID) ->
  case irc_channel:fetch(ChannelName) of
    #channel{} = Channel ->
      irc_channel:subscribe(Channel, ClientID);
    _ChannelNotRegistered ->
      channel_not_registered
  end.

-spec unsubscribe_channel(ChannelName, ClientID) ->
        ok | channel_not_registered | client_not_subscribed when
    ChannelName :: channel_name(),
    ClientID    :: client_id().
unsubscribe_channel(ChannelName, ClientID) ->
  case irc_channel:fetch(ChannelName) of
    #channel{} = Channel ->
      irc_channel:unsubscribe(Channel, ClientID);
    _ChannelNotRegistered ->
      channel_not_registered
  end.

-spec msg_channel(ChannelName, Msg, ClientID) ->
        ok | channel_not_registered | client_not_subscribed when
    ChannelName :: channel_name(),
    Msg         :: binary(),
    ClientID    :: client_id().
msg_channel(ChannelName, Msg, ClientID) ->
  case irc_channel:fetch(ChannelName) of
    #channel{} = Channel ->
      Client = irc_client:fetch(ClientID),
      irc_channel:dispatch_msg(Channel, Client, Msg);
    _ChannelNotRegistered ->
      channel_not_registered
  end.

-spec close_client(ClientID) -> ok when
    ClientID :: client_id().
close_client(ClientID) ->
  #client{owned = Owned} = irc_client:fetch(ClientID),
  CloseFn =
    fun(ChannelName) ->
        close_channel(ChannelName, ClientID)
    end,
  lists:foreach(CloseFn, Owned).
