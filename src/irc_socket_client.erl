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
    Response :: {ok | Msg, State},
    Msg      :: io_lib:chars().
handle(Packet, State) ->
  do_handle(Packet, State).

%%%===================================================================
%%% Internal functions
%%%===================================================================

do_handle(<<"create_channel! ", ChannelName/binary>>,
          #client{id = OwnerID, channels = Channels} = State) ->
  case irc_channel:create(ChannelName, OwnerID) of
    ok ->
      Response = io_lib:format("Channel '~p' created~n", [ChannelName]),
      NewChannels = [ChannelName | Channels],
      {Response, State#client{channels = NewChannels}};
    channel_already_registered ->
      ErrResponse = io_lib:format("Channel '~p' already registered~n",
                                  [ChannelName]),
      {ErrResponse, State}
  end;
do_handle(<<"close_channel! ", ChannelName/binary>>,
          #client{id = OwnerID, channels = Channels} = State) ->
  case irc_channel:close(ChannelName, OwnerID) of
    ok ->
      Response = io_lib:format("Channel '~p' closed~n", [ChannelName]),
      NewChannels = lists:delete(ChannelName, Channels),
      {Response, State#client{channels = NewChannels}};
    channel_not_registered ->
      ErrResponse = io_lib:format("Channel '~p' not registered~n",
                                  [ChannelName]),
      {ErrResponse, State};
    non_channel_owner ->
      ErrResponse = io_lib:format("Not owner of channel '~p'~n",
                                  [ChannelName]),
      {ErrResponse, State}
  end;
do_handle(Packet, State) ->
  {?INVALID_COMMAND(Packet), State}.
