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
         get_spec/0,
         create_client/3]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

get_spec() ->
  #{id => ?MODULE,
    start => {?MODULE, start_link, []},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [?MODULE]}.

%%--------------------------------------------------------------------
%% @doc
%% Dynamically spawn irc_udp_client process.
%% @end
%%--------------------------------------------------------------------
create_client(Socket, Host, Port) when is_port(Port) ->
  supervisor:start_child(?MODULE, [Socket, Host, Port]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
  SupFlags = #{strategy => simple_one_for_one,
               intensity => 1,
               period => 5},
  ChildSpec = [irc_udp_client:get_spec()],

  {ok, {SupFlags, ChildSpec}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
