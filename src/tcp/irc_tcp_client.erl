%%%-------------------------------------------------------------------
%%% @author Ryan User <ryan@nixos-laptop>
%%% @copyright (C) 2022, Ryan User
%%% @doc
%%%
%%% @end
%%% Created : 15 Aug 2022 by Ryan User <ryan@nixos-laptop>
%%%-------------------------------------------------------------------
-module(irc_tcp_client).

-behaviour(gen_server).
-behaviour(ranch_protocol).

-include("irc.hrl").
-include("irc_socket.hrl").

%% API
-export([start_link/3]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {id        :: client_id(),
                transport :: module(),
                protocol  :: tcp_connection()}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Ref, Transport, Opts) ->
  {ok, proc_lib:spawn_link(?MODULE, init, [{Ref, Transport, Opts}])}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init({Ref, Transport, _Opts = []}) ->
  {ok, Socket} = ranch:handshake(Ref),
  ok = Transport:setopts(Socket, [{active, once}]),
  ClientID = irc_client:register(Socket),
  gen_server:enter_loop(?MODULE, [], #state{id        = ClientID,
                                            transport = Transport,
                                            protocol  = Socket}).

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast({subscription_msg, Msg}, #state{transport = Transport,
                                            protocol = Socket} = State) ->
  Transport:send(Socket, Msg),
  {noreply, State};
handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({tcp, _Socket, <<"quit!", _Rest/binary>>}, State) ->
  {stop, normal, State};
handle_info({tcp, Socket, Packet}, #state{id        = ClientID,
                                          transport = Transport} = State) ->
  Transport:setopts(Socket, [{active, once}]),
  case irc_socket_client:handle(Packet, ClientID) of
    ok ->
      ok;
    Response ->
      Transport:send(Socket, Response)
  end,
  {noreply, State, ?SOCKET_TIMEOUT};
handle_info(timeout, #state{transport = Transport,
                            protocol  = Socket} = State) ->
  Transport:send(Socket, ?TIMEOUT_MSG),
  {stop, normal, State};
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
