%%%-------------------------------------------------------------------
%%% @author Ryan User <ryan@nixos-desktop>
%%% @copyright (C) 2022, Ryan User
%%% @doc
%%% irc interface for udp clients.
%%% @end
%%% Created :  6 Aug 2022 by Ryan User <ryan@nixos-desktop>
%%%-------------------------------------------------------------------
-module(irc_udp_client).

-behaviour(gen_server).

%% API
-export([start_link/3]).

%% gen_server callbacks
-export([init/1, 
         handle_call/3, 
         handle_cast/2,
         handle_info/2,
         terminate/2, 
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {socket  :: inet:socket(),
                address :: inet:ip_address(),
                port    :: port()}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Socket, Address, Port) ->
  gen_server:start_link(?MODULE, [Socket, Address, Port], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Socket, Address, Port]) ->
  Client = #state{socket  = Socket,
                  address = Address,
                  port    = Port},
  {ok, Client}.

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
