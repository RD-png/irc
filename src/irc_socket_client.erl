%%%-------------------------------------------------------------------
%%% @author Ryan User <ryan@nixos-laptop>
%%% @copyright (C) 2022, Ryan User
%%% @doc
%%%
%%% @end
%%% Created : 21 Aug 2022 by Ryan User <ryan@nixos-laptop>
%%%-------------------------------------------------------------------
-module(irc_socket_client).

-include("irc.hrl").

%% API
-export([create_channel/2]).

%%%=============================p======================================
%%% API
%%%===================================================================

-spec create_channel(ChannelName, OwnerID) -> Result when
    ChannelName :: binary(),
    OwnerID     :: client_id(),
    Result      :: channel_id().
create_channel(ChannelName, OwnerID) ->
  case irc_channel:create(ChannelName, OwnerID) of
    {ok, ChannelID} ->
      {ok, ChannelID};
    {error, channel_already_registered} = Err ->
      Err
  end.
