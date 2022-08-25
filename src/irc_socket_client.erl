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

%% API
-export([create_channel/2]).

%%%===================================================================
%%% API
%%%===================================================================

-spec create_channel(ChannelName, OwnerID) -> ClientRes when
    ChannelName :: channel_name(),
    OwnerID     :: client_id(),
    ClientRes   :: {ok, io_lib:chars()} | {error, io_lib:chars()}.
create_channel(ChannelName, OwnerID) ->
  case irc_channel:create(ChannelName, OwnerID) of
    ok ->
      Response = io_lib:format("Channel '~p' created~n", [ChannelName]),
      {ok, Response};
    channel_already_registered ->
      ErrResponse = io_lib:format("Channel '~p' already registered~n",
                                  [ChannelName]),
      {error, ErrResponse}
  end.
