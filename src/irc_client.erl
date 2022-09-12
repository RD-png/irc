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
         update/1,
         unregister/1,
         fetch/1,
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

-spec update(Client) -> ok when
    Client :: client().
update(Client) when is_record(Client, client) ->
  mnesia:dirty_write(Client).

-spec unregister(ClientID) -> ok when
    ClientID :: client_id().
unregister(ClientID) ->
  mnesia:dirty_delete({?CLIENT_TABLE, ClientID}).

-spec fetch(ClientID) -> Result when
    ClientID :: client_id(),
    Result   :: Client | none,
    Client   :: client().
fetch(ClientID) ->
  case mnesia:dirty_read({?CLIENT_TABLE, ClientID}) of
    [Client] ->
      Client;
    [] ->
      none
  end.

-spec set_name(ClientID, Name) -> ok when
    ClientID :: client_id(),
    Name     :: binary().
set_name(ClientID, Name) ->
  Client = fetch(ClientID),
  UpdatedClient = Client#client{name = Name},
  update(UpdatedClient).
