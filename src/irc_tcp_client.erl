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
  %% ok = Transport:setopts(Socket, [binary, {active, once}]),
  loop(Socket, Transport).

%%%===================================================================
%%% Internal functions
%%%===================================================================

loop(Socket, Transport) ->
  case Transport:recv(Socket, 0, 5000) of
    {ok, Packet} ->
      Response = handle(Packet),
      Transport:send(Socket, Response),
      loop(Socket, Transport);
    _ ->
      ok = Transport:close(Socket)
  end.

handle(Packet) ->
  Packet.
