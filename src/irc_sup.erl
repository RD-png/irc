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

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
  SupFlags = #{strategy => one_for_all,
               intensity => 0,
               period => 1},
  ChildSpecs = [irc_udp_manager_spec()],
  
  {ok, {SupFlags, ChildSpecs}}.

%%%-------------------------------------------------------------------
%% Internal Functions
%%%-------------------------------------------------------------------

irc_udp_manager_spec() ->
  #{id => irc_udp_manager,
    start => {irc_udp_manager, start_link, []},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [irc_udp_manager]}.
