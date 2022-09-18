%%%-------------------------------------------------------------------
%%% @author Ryan User <ryan@nixos-desktop>
%%% @copyright (C) 2022, Ryan User
%%% @doc
%%%
%%% @end
%%% Created :  17 Sep 2022 by Ryan User <ryan@nixos-desktop>
%%%-------------------------------------------------------------------
-module(irc_worker).
-author("ryandenby").

-behaviour(gen_server).

-include("irc.hrl").

-define(POOL, irc_workers).

%% API
-export([start_link/1,
         get_spec/0,
         dispatch_msg/2]).

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

start_link(Args) ->
  gen_server:start_link(?MODULE, [], Args).

-spec get_spec() -> ok.
get_spec() ->
  PoolArgs = irc_app:get_env(?POOL),
  poolboy:child_spec(?POOL, PoolArgs).

-spec dispatch_msg(Subscribers, Msg) -> ok when
    Subscribers :: [{client_id(), pid()}],
    Msg         :: io_lib:chars().
dispatch_msg(Subscribers, Msg) ->
  poolboy:transaction(
    ?POOL,
    fun(Worker) ->
        gen_server:cast(Worker, {dispatch_msg, Subscribers, Msg})
    end).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  {ok, []}.

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast({dispatch_msg, Subscribers, Msg}, State) ->
  do_dispatch_msg(Subscribers, Msg),
  {noreply, State};
handle_cast(_Request, State) ->
  {noreply, State}.

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

do_dispatch_msg(Subscribers, Msg) ->
  DispatchFn = fun({_ClientID, ClientPID}) ->
                   gen_server:cast(ClientPID, {subscription_msg, Msg})
               end,
  lists:foreach(DispatchFn, Subscribers).
