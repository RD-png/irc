%%%-------------------------------------------------------------------
%%% @author Ryan User <ryan@nixos-desktop>
%%% @copyright (C) 2022, Ryan User
%%% @doc
%%% irc interface for udp clients.
%%% @end
%%% Created :  6 Aug 2022 by Ryan User <ryan@nixos-desktop>
%%%-------------------------------------------------------------------
-module(irc_udp_client).
-author("ryandenby").

-behaviour(gen_server).

-include("irc.hrl").
-include("irc_socket.hrl").

%% API
-export([start_link/3,
         get_spec/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Socket, Host, Port) ->
  gen_server:start_link(?MODULE, [{Socket, Host, Port}], []).

get_spec() ->
  #{id => ?MODULE,
    start => {?MODULE, start_link, []},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [?MODULE]}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Protocol]) ->
  Client = irc_client:register(<<"test2">>, Protocol),
  {ok, Client}.

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({udp, Socket, Host, Port, Packet}, State) ->
  NewState = case handle(Packet, State) of
               {ok, HandleState} ->
                 HandleState;
               {Response, HandleState}->
                 gen_udp:send(Socket, Host, Port, Response),
                 HandleState
             end,
  {noreply, NewState};
handle_info(Info, State) ->
  lager:error("Received unhandled message ~p", [Info]),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

handle(<<"create_channel! ", _ChannelName/binary>>, State) ->
  {"test", State};
handle(Packet, State) ->
  {?INVALID_COMMAND(Packet), State}.
