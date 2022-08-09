%%%-------------------------------------------------------------------
%%% @author Ryan User <ryan@nixos-laptop>
%%% @copyright (C) 2022, Ryan User
%%% @doc
%%%
%%% @end
%%% Created :  9 Aug 2022 by Ryan User <ryan@nixos-laptop>
%%%-------------------------------------------------------------------
-module(irc_udp_client_sup).
-author("ryandenby").

-behaviour(supervisor).

%% API
-export([start_link/0,
         create_client/3]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER,         ?MODULE).
-define(IRC_UDP_CLIENT, irc_udp_client).


%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%% @end
%%--------------------------------------------------------------------
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

create_client(Socket, Address, Port) ->
  supervisor:start_child(?MODULE, [Socket, Address, Port]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
  SupFlags = #{strategy => simple_one_for_one,
               intensity => 1,
               period => 5},
  ChildSpec = [#{id => ?IRC_UDP_CLIENT,
                 start => {?IRC_UDP_CLIENT, start_link, []},
                 restart => permanent,
                 shutdown => 5000,
                 type => worker,
                 modules => [?IRC_UDP_CLIENT]}],

  {ok, {SupFlags, ChildSpec}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
