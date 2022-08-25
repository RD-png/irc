%%%-------------------------------------------------------------------
%%% @author Ryan User <ryan@nixos-desktop>
%%% @copyright (C) 2022, Ryan User
%%% @doc
%%%
%%% @end
%%% Created :  7 Aug 2022 by Ryan User <ryan@nixos-desktop>
%%%-------------------------------------------------------------------
-module(irc_client_sup).
-author("ryandenby").

-behaviour(supervisor).

%% API
-export([start_link/0,
         get_spec/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

get_spec() ->
  #{id => ?MODULE,
    start => {?MODULE, start_link, []},
    restart => permanent,
    shutdown => 5000,
    type => supervisor,
    modules => [?MODULE]}.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
  irc_ranch_tcp_start_acceptor_pool(),
  SupFlags = #{strategy => one_for_one,
               intensity => 1,
               period => 5},
  ChildSpec = [irc_udp_manager:get_spec(),
               irc_udp_client_sup:get_spec()],

  {ok, {SupFlags, ChildSpec}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

irc_ranch_tcp_start_acceptor_pool() ->
  TransportOpts = #{num_acceptors => 50,
                    socket_opts => [{port, 8080}]},
  {ok, _} = ranch:start_listener(irc_tcp_acceptors,
                                 ranch_tcp,
                                 TransportOpts,
                                 irc_tcp_client, []).
