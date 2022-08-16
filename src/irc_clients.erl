%%%-------------------------------------------------------------------
%%% @author Ryan User <ryan@nixos-laptop>
%%% @copyright (C) 2022, Ryan User
%%% @doc
%%% User interface to interact with IRC clients ets table
%%% @end
%%% Created : 16 Aug 2022 by Ryan User <ryan@nixos-laptop>
%%%-------------------------------------------------------------------
-module(irc_clients).

-include("include/irc.hrl").

%% API
-export([init/0,
         stop/0,
         register_client/3]).

-define(IRC_ETS_CLIENTS, irc_ets_clients).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Initialize ets table for irc clients.
%% @end
%%--------------------------------------------------------------------
-spec init() -> ok.
init() ->
  EtsOpts = [named_table, public, set, {keypos, #client.id}],
  ?IRC_ETS_CLIENTS = ets:new(?IRC_ETS_CLIENTS, EtsOpts),
  ok.

%%--------------------------------------------------------------------
%% @doc
%% Close ets table for irc clients.
%% @end
%%--------------------------------------------------------------------
-spec stop() -> true.
stop() ->
  ets:delete(?IRC_ETS_CLIENTS).

%%--------------------------------------------------------------------
%% @doc
%% Enter new client record into `irc_ets_clients` if no user currently
%% registered to `ID`.
%% @end
%%--------------------------------------------------------------------
register_client(ID, Name, Protocol) ->
  Client = #client{id = ID,
                   name = Name,
                   joined = time(),
                   protocol = Protocol},
  ets:insert_new(?IRC_ETS_CLIENTS, Client).
  
