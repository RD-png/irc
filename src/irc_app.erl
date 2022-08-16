%%%-------------------------------------------------------------------
%%% @author Ryan User <ryan@nixos-desktop>
%%% @copyright (C) 2022, Ryan User
%%% @doc
%%% irc public API.
%%% @end
%%% Created :  6 Aug 2022 by Ryan User <ryan@nixos-desktop>
%%%-------------------------------------------------------------------
-module(irc_app).
-author("ryandenby").

-behaviour(application).

%% API
-export([start/2,
         stop/1,
         get_env/1]).

-define(APP, irc).

%%%-------------------------------------------------------------------
%% API
%%%-------------------------------------------------------------------

start(_StartType, _StartArgs) ->
  irc_clients:init(),
  irc_sup:start_link().

stop(_State) ->
  ok.

-spec get_env(Key) -> Value | no_return() when
    Key   :: atom(),
    Value :: term().
get_env(Key) when is_atom(Key) ->
  case application:get_env(?APP, Key) of
    {ok, Val} ->
      Val;
    undefined ->
      throw({env_missing_key, Key})
  end.
