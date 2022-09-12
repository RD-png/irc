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
         close_channel/2]).

%%%-------------------------------------------------------------------
%% API
%%%-------------------------------------------------------------------

-spec start() -> ok.
start() ->
  ok = irc_client:create_mnesia_table(),
  ok = irc_channel:create_mnesia_table().

-spec stop() -> stopped | {error, term()}.
stop() ->
  mnesia:stop().

-spec create_channel(ChannelName, ClientID) -> Result when
    ChannelName :: channel_name(),
    ClientID    :: client_id(),
    Result      :: ok
                 | channel_already_registered
                 | client_not_registered.
create_channel(ChannelName, ClientID) ->
  case irc_client:fetch(ClientID) of
    #client{owned = Owned, subscribed = Subscribed} = Client ->
      case irc_channel:create(ChannelName, ClientID) of
        ok ->
          UpdatedClient = Client#client{owned      = [ChannelName | Owned],
                                        subscribed = [ChannelName | Subscribed]},
          irc_client:update(UpdatedClient);
        channel_already_registered = Err ->
          Err
      end;
    _ClientNotRegistered ->
      client_not_registered
  end.

-spec close_channel(ChannelName, ClientID) -> Result when
    ChannelName :: channel_name(),
    ClientID    :: client_id(),
    Result      :: ok
                 | client_not_registered
                 | channel_non_owner
                 | channel_not_registered.
close_channel(ChannelName, ClientID) ->
  case irc_channel:fetch(ChannelName) of
    #channel{owner = OwnerID} = Channel when ClientID =:= OwnerID ->
      do_close_channel(Channel, ClientID);
    #channel{} = _Channel ->
      channel_non_owner;
    _ChannelNotRegistered ->
      channel_not_registered
  end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

do_close_channel(#channel{name = ChannelName} = Channel, ClientID) ->
  case irc_client:fetch(ClientID) of
    #client{owned = Owned, subscribed = Subscribed} = Client ->
      UpdatedClient =
        Client#client{owned      = lists:delete(ChannelName, Owned),
                      subscribed = lists:delete(ChannelName, Subscribed)},
      irc_client:update(UpdatedClient),
      irc_channel:unregister(Channel);
    _ClientNotRegistered ->
      client_not_registered
  end.
