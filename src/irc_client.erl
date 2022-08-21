%%%-------------------------------------------------------------------
%%% @author Ryan User <ryan@nixos-laptop>
%%% @copyright (C) 2022, Ryan User
%%% @doc
%%%
%%% @end
%%% Created : 18 Aug 2022 by Ryan User <ryan@nixos-laptop>
%%%-------------------------------------------------------------------
-module(irc_client).

-include("irc.hrl").

%% API
-export([register/2,
         unregister/1]).

-define(CLIENT_TABLE, client).

%%%===================================================================
%%% API
%%%===================================================================

-spec register(Name, Protocol) -> Client when
    Name     :: binary(),
    Protocol :: socket_connection(),
    Client   :: client().
register(Name, Protocol) ->
  Client = #client{id       = uuid:uuid1(),
                   name     = Name,
                   joined   = time(),
                   protocol = Protocol},
  mnesia:dirty_write(Client),
  Client.

-spec unregister(ClientID) -> ok when
    ClientID :: client().
unregister(ClientID) ->
  ok = mnesia:dirty_delete({?CLIENT_TABLE, ClientID}).
