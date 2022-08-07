%%%-------------------------------------------------------------------
%%% @author Ryan User <ryan@nixos-desktop>
%%% @copyright (C) 2022, Ryan User
%%% @doc
%%% irc top level supervisor.
%%% @end
%%% Created :  6 Aug 2022 by Ryan User <ryan@nixos-desktop>
%%%-------------------------------------------------------------------
-module(irc_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER,         ?MODULE).
-define(IRC_CLIENT_SUP, irc_client_sup).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
  SupFlags = #{strategy => one_for_all,
               intensity => 0,
               period => 1},
  ChildSpecs = [irc_client_sup_spec()],
  
  {ok, {SupFlags, ChildSpecs}}.

%%%-------------------------------------------------------------------
%% Internal Functions
%%%-------------------------------------------------------------------

irc_client_sup_spec() ->
  #{id => ?IRC_CLIENT_SUP,
    start => {?IRC_CLIENT_SUP, start_link, []},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [?IRC_CLIENT_SUP]}.
