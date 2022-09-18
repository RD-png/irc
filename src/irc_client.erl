%%%-------------------------------------------------------------------
%%% @author Ryan User <ryan@nixos-laptop>
%%% @copyright (C) 2022, Ryan User
%%% @doc
%%%
%%% @end
%%% Created : 18 Aug 2022 by Ryan User <ryan@nixos-laptop>
%%%-------------------------------------------------------------------
-module(irc_client).
-author("ryandenby").

-include("irc.hrl").

%% API
-export([create_mnesia_table/0,
         register/1,
         unregister/1,
         fetch/1,
         create_channel/2,
         close_channel/2,
         set_name/2]).

-define(CLIENT_TABLE, client).

%%%===================================================================
%%% API
%%%===================================================================

-spec create_mnesia_table() -> ok.
create_mnesia_table() ->
  {atomic, ok} =
    mnesia:create_table(client, [{attributes, record_info(fields, client)},
                                 {type, ordered_set}]),
  ok.

-spec register(Protocol) -> Client when
    Protocol :: socket_connection(),
    Client   :: client().
register(Protocol) ->
  Client = #client{id       = uuid:uuid1(),
                   joined   = {time(), date()},
                   protocol = Protocol},
  mnesia:dirty_write(Client),
  Client#client.id.

-spec unregister(ClientID) -> ok when
    ClientID :: client_id().
unregister(ClientID) ->
  mnesia:dirty_delete({?CLIENT_TABLE, ClientID}).

-spec fetch(ClientID) -> Client | none when
    ClientID :: client_id(),
    Client   :: client().
fetch(ClientID) ->
  case mnesia:dirty_read({?CLIENT_TABLE, ClientID}) of
    [Client] ->
      Client;
    [] ->
      none
  end.

-spec create_channel(ChannelName, Client) -> ok when
    ChannelName :: channel_name(),
    Client      :: client().
create_channel(ChannelName, #client{owned      = Owned,
                                    subscribed = Subscribed} = Client) ->
  UpdatedClient = Client#client{owned      = [ChannelName | Owned],
                                subscribed = [ChannelName | Subscribed]},
  mnesia:dirty_write(UpdatedClient).

-spec close_channel(ChannelName, Client) -> ok when
    ChannelName :: channel_name(),
    Client      :: client().
close_channel(ChannelName, #client{owned      = Owned,
                                   subscribed = Subscribed} = Client) ->
  UpdatedClient =
    Client#client{owned      = lists:delete(ChannelName, Owned),
                  subscribed = lists:delete(ChannelName, Subscribed)},
  mnesia:dirty_write(UpdatedClient).

-spec set_name(ClientID, Name) -> ok when
    ClientID :: client_id(),
    Name     :: binary().
set_name(ClientID, Name) ->
  Client = fetch(ClientID),
  UpdatedClient = Client#client{name = Name},
  mnesia:dirty_write(UpdatedClient).
