%%%-------------------------------------------------------------------
%%% @author Ryan User <ryan@nixos-desktop>
%%% @copyright (C) 2022, Ryan User
%%% @doc
%%%
%%% @end
%%% Created :  6 Aug 2022 by Ryan User <ryan@nixos-desktop>
%%%-------------------------------------------------------------------
-module(irc_udp_manager).
-author("ryandenby").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, 
         handle_call/3, 
         handle_cast/2, 
         handle_info/2,
         terminate/2, 
         code_change/3]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  {ok, _Socket} = start_udp(),
  {ok, []}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({udp, Socket, Address, Port, _Msg} = Packet, State) ->
  {NewState, Client} = case lists:keyfind({Address, Port}, 1, State) of
                         {_Key, ClientPID} ->
                           {State, ClientPID};
                         false ->
                           {ok, ClientPID} = irc_udp_client_sup:create_client(Socket, Address, Port),
                           UpdatedState = register_client({Address, Port}, ClientPID, State),
                           {UpdatedState, ClientPID}
                         end,
  Client ! Packet,
  {noreply, NewState};
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

start_udp() ->
  Port = irc_app:get_env(udp_port),
  gen_udp:open(Port, [{mode, binary}, {active, true}, {reuseaddr, true}]).

register_client(Key, ClientPID, State) ->
  [{Key, ClientPID} | State].
