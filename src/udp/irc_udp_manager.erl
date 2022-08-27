%%%-------------------------------------------------------------------
%%% @author Ryan User <ryan@nixos-desktop>
%%% @copyright (C) 2022, Ryan User
%%% @doc
%%%
%%% @end
%%% Created :  6 Aug 2022 by Ryan User <ryan@nixos-desktop>
%%%-------------------------------------------------------------------
-module(irc_udp_manager).
-author("ryandenby").

-behaviour(gen_server).

%% API
-export([start_link/0,
         get_spec/0,
         unregister/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get_spec() ->
  #{id => ?MODULE,
    start => {?MODULE, start_link, []},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [?MODULE]}.

-spec unregister(ClientPID) -> ok when
    ClientPID :: pid().
unregister(ClientPID) ->
  gen_server:cast(?MODULE, {unregister, ClientPID}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  {ok, _Socket} = start_udp(),
  {ok, []}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({unregister, ClientPID}, State) ->
  UpdatedState = do_unregister(ClientPID, State),
  {noreply, UpdatedState};
handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({udp, Socket, Host, Port, _Msg} = Packet, State) ->
  {NewState, Client} = case is_registered_client({Host, Port}, State) of
                         {_Key, ClientPID}->
                           {State, ClientPID};
                         false ->
                           {ok, ClientPID} = irc_udp_client_sup:create_client(Socket, Host, Port),
                           UpdatedState = register({Host, Port}, ClientPID, State),
                           {UpdatedState, ClientPID}
                         end,
  Client ! Packet,
  {noreply, NewState};
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

start_udp() ->
  Port = irc_app:get_env(udp_port),
  gen_udp:open(Port, [{mode, binary}, {reuseaddr, true}]).

register(Key, ClientPID, State) ->
  link(ClientPID),
  [{Key, ClientPID} | State].

is_registered_client(Key, State) ->
  lists:keyfind(Key, 1, State).

do_unregister(ClientPID, State) ->
  lists:delete(ClientPID, State).
