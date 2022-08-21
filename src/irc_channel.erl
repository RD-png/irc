%%%-------------------------------------------------------------------
%%% @author Ryan User <ryan@nixos-laptop>
%%% @copyright (C) 2022, Ryan User
%%% @doc
%%%
%%% @end
%%% Created : 19 Aug 2022 by Ryan User <ryan@nixos-laptop>
%%%-------------------------------------------------------------------
-module(irc_channel).

-include("irc.hrl").

%% API
-export([create/2]).

%%%===================================================================
%%% API
%%%===================================================================

-spec create(ChannelName, OwnerID) -> ChannelID when
    ChannelName :: binary(),
    OwnerID     :: client_id(),
    ChannelID   :: channel_id().
create(ChannelName, OwnerID) ->
  Channel = #channel{id = uuid:uuid1(),
                     name = ChannelName,
                     owner = OwnerID,
                     clients = [OwnerID]},
  mnesia:dirty_write(Channel),
  {ok, Channel#channel.id}.
