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
-export([create_mnesia_table/0,
         create/2,
         fetch/1,
         unregister/1,
         subscribe/2,
         unsubscribe/2]).

-define(CHANNEL_TABLE, channel).

%%%===================================================================
%%% API
%%%===================================================================

-spec create_mnesia_table() -> ok.
create_mnesia_table() ->
  {atomic, ok} =
    mnesia:create_table(channel,
                        [{attributes, record_info(fields, channel)},
                         {type, ordered_set}]),
  ok.

-spec create(ChannelName, ClientID) -> Result when
    ChannelName :: channel_name(),
    ClientID    :: client_id(),
    Result      :: ok | channel_already_registered.
create(ChannelName, ClientID) ->
  case is_registered(ChannelName) of
    false ->
      Channel = #channel{name        = ChannelName,
                         owner       = ClientID,
                         subscribers = [{ClientID, self()}]},
      mnesia:dirty_write(Channel);
    true ->
      channel_already_registered
  end.

-spec unregister(Channel) -> ok | false when
    Channel :: channel().
unregister(Channel) when is_record(Channel, channel) ->
  mnesia:dirty_delete_object(Channel);
unregister(_Channel) ->
  false.

-spec fetch(ChannelName) -> Result when
    ChannelName :: channel_name(),
    Result      :: Channel | none,
    Channel     :: channel().
fetch(ChannelName) ->
  case mnesia:dirty_read({?CHANNEL_TABLE, ChannelName}) of
    [Channel] ->
      Channel;
    [] ->
      none
  end.

-spec subscribe(Channel, ClientID) -> Result when
    Channel  :: channel(),
    ClientID :: client_id(),
    Result   :: ok | client_already_subscribed.
subscribe(#channel{subscribers = Subscribers} = Channel, ClientID) ->
  case is_subscribed(Subscribers, ClientID) of
    false ->
      UpdatedSubscribers = [{ClientID, self()} | Subscribers],
      UpdatedChannel = Channel#channel{subscribers = UpdatedSubscribers},
      mnesia:dirty_write(UpdatedChannel);
    true ->
      client_already_subscribed
  end.

-spec unsubscribe(Channel, ClientID) -> Result when
    Channel  :: channel(),
    ClientID :: client_id(),
    Result   :: ok | client_not_subscribed.
unsubscribe(#channel{subscribers = Subscribers} = Channel, ClientID) ->
  case is_subscribed(Subscribers, ClientID) of
    true ->
      UpdatedSubscribers = lists:delete({ClientID, self()}, Subscribers),
      UpdatedChannel = Channel#channel{subscribers = UpdatedSubscribers},
      mnesia:dirty_write(UpdatedChannel);
    false ->
      client_not_subscribed
  end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

is_registered(ChannelName) ->
  [] /= mnesia:dirty_read({?CHANNEL_TABLE, ChannelName}).

is_subscribed(Subscribers, ClientID) ->
  lists:member({ClientID, self()}, Subscribers).
