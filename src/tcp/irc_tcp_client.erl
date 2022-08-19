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
  State = irc_client:register("test", Socket),
  loop(Socket, Transport, State).

%%%===================================================================
%%% Internal functions
%%%===================================================================

loop(Socket, Transport, State) ->
  case Transport:recv(Socket, 0, ?SOCKET_TIMEOUT) of
    {ok, Packet} ->
      Response = handle(Packet),
      Transport:send(Socket, Response),
      loop(Socket, Transport, State);
    {error, timeout} ->
      Transport:send(Socket, ?TIMEOUT_MSG),
      irc_client:unregister(State#client.id);
    {error, Reason} ->
      lager:err("TCP Client '~p' exited with reason '~p'",
                [State#client.protocol, Reason])
  end,
  ok = Transport:close(Socket).

handle(Packet) ->
  Packet.
