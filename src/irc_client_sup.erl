%%%-------------------------------------------------------------------
%%% @author Ryan User <ryan@nixos-desktop>
%%% @copyright (C) 2022, Ryan User
%%% @doc
%%%
%%% @end
%%% Created :  7 Aug 2022 by Ryan User <ryan@nixos-desktop>
%%%-------------------------------------------------------------------
-module(irc_client_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER,             ?MODULE).
-define(IRC_UDP_MANAGER,    irc_udp_manager).
-define(IRC_UDP_CLIENT_SUP, irc_udp_client_sup).
-define(IRC_TCP_CLIENT,     irc_tcp_client).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
  irc_ranch_tcp_start_acceptor_pool(),
  SupFlags = #{strategy => one_for_one,
               intensity => 1,
               period => 5},
  ChildSpec = [irc_udp_manager_spec(),
               irc_udp_client_sup_spec()],

  {ok, {SupFlags, ChildSpec}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

irc_ranch_tcp_start_acceptor_pool() ->
  {ok, _} = ranch:start_listener(irc_tcp_acceptors,
                                 ranch_tcp,
                                 #{num_acceptors => 50,
                                   socket_opts => [{port, 8080}]},
                                 ?IRC_TCP_CLIENT, []).

irc_udp_manager_spec() ->
  #{id => ?IRC_UDP_MANAGER,
    start => {?IRC_UDP_MANAGER, start_link, []},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [?IRC_UDP_MANAGER]}.

irc_udp_client_sup_spec() ->
  #{id => ?IRC_UDP_CLIENT_SUP,
    start => {?IRC_UDP_CLIENT_SUP, start_link, []},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [?IRC_UDP_CLIENT_SUP]}.
