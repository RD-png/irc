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

-record(state, {id       :: client_id(),
                protocol :: udp_connection()}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Socket, Host, Port) ->
  gen_server:start_link(?MODULE, [{Socket, Host, Port}], []).

get_spec() ->
  #{id => ?MODULE,
    start => {?MODULE, start_link, []},
    restart => temporary,
    shutdown => 5000,
    type => worker,
    modules => [?MODULE]}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Protocol]) ->
  ClientID = irc_client:register(<<"test2">>, Protocol),
  {ok, #state{id = ClientID, protocol = Protocol}}.

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({udp, Socket, Host, Port, Packet}, #state{id = ClientID} = State) ->
  case irc_socket_client:handle(Packet, ClientID) of
    ok ->
      ok;
    Response ->
      gen_udp:send(Socket, Host, Port, Response)
  end,
  {noreply, State, ?SOCKET_TIMEOUT};
handle_info(timeout, #state{protocol = {Socket, Host, Port}} = State) ->
  gen_udp:send(Socket, Host, Port, ?TIMEOUT_MSG),
  {stop, normal, State};
handle_info(Info, State) ->
  lager:error("Received unhandled message ~p", [Info]),
  {noreply, State}.

terminate(_Reason, #state{protocol = {_Socket, Host, Port}} = _State) ->
  irc_udp_manager:unregister({Host, Port}).

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
