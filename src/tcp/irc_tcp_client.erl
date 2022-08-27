%%%-------------------------------------------------------------------
%%% @author Ryan User <ryan@nixos-laptop>
%%% @copyright (C) 2022, Ryan User
%%% @doc
%%%
%%% @end
%%% Created : 15 Aug 2022 by Ryan User <ryan@nixos-laptop>
%%%-------------------------------------------------------------------
-module(irc_tcp_client).

-behaviour(ranch_protocol).

-include("irc.hrl").
-include("irc_socket.hrl").

%% API
-export([start_link/3]).

%% ranch_protocol callbacks
-export([init/3]).

-record(state, {id       :: client_id(),
                protocol :: tcp_connection()}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Ref, Transport, Opts) ->
  Pid = spawn_link(?MODULE, init, [Ref, Transport, Opts]),
  {ok, Pid}.

%%%===================================================================
%%% ranch_protocol callbacks
%%%===================================================================

init(Ref, Transport, _Opts = []) ->
  {ok, Socket} = ranch:handshake(Ref),
  ClientID = irc_client:register(<<"test">>, Socket),
  loop(Socket, Transport, #state{id = ClientID, protocol = Socket}).

%%%===================================================================
%%% Internal functions
%%%===================================================================

loop(Socket, Transport, #state{id = ClientID} = State) ->
  case Transport:recv(Socket, 0, ?SOCKET_TIMEOUT) of
    {ok, Packet} ->
      CleanPacket = re:replace(Packet, "[\r\n]$", "",
                               [global, {return, binary}]),
      case irc_socket_client:handle(CleanPacket, ClientID) of
        ok ->
          ok;
        Response ->
          Transport:send(Socket, Response)
      end,
      loop(Socket, Transport, State);
    {error, timeout} ->
      Transport:send(Socket, ?TIMEOUT_MSG),
      irc_client:unregister(ClientID);
    {error, Reason} ->
      lager:info("TCP Client '~p' exited with reason '~p'", [ClientID, Reason])
  end,
  ok = Transport:close(Socket).
