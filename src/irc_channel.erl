%%%-------------------------------------------------------------------
%%% @author Ryan User <ryan@nixos-laptop>
%%% @copyright (C) 2022, Ryan User
%%% @doc
%%%
%%% @end
%%% Created : 19 Aug 2022 by Ryan User <ryan@nixos-laptop>
%%%-------------------------------------------------------------------
-module(irc_channel).
-author("ryandenby").

-include("irc.hrl").

%% API
-export([create/2,
         close/2]).

-define(CHANNEL_TABLE, channel).

%%%===================================================================
%%% API
%%%===================================================================

-spec create(ChannelName, OwnerID) -> Result when
    ChannelName :: channel_name(),
    OwnerID     :: client_id(),
    Result      :: ok | channel_already_registered.
create(ChannelName, OwnerID) ->
  case is_registered_channel(ChannelName) of
    false ->
      Channel = #channel{name    = ChannelName,
                         owner   = OwnerID,
                         clients = [OwnerID]},
      mnesia:dirty_write(Channel);
    true ->
      channel_already_registered
  end.

-spec close(ChannelName, OwnerID) -> Result when
    ChannelName :: channel_name(),
    OwnerID     :: client_id(),
    Result      :: ok | non_channel_owner | channel_not_registered.
close(ChannelName, OwnerID) ->
  case fetch_channel(ChannelName) of
    [Channel] when OwnerID =:= Channel#channel.owner ->
      mnesia:dirty_delete_object(Channel);
    [_Channel] ->
      non_channel_owner;
    _NotRegistered ->
      channel_not_registered
  end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

is_registered_channel(ChannelName) ->
  [] /= mnesia:dirty_read({?CHANNEL_TABLE, ChannelName}).

fetch_channel(ChannelName) ->
  mnesia:dirty_read({?CHANNEL_TABLE, ChannelName}).
