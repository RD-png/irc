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

-type connection_id() :: {inet:hostname() | inet:ip_address(),
                          inet:port_number()}.

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

-spec unregister(ConnectionKey) -> ok when
    ConnectionKey :: connection_id().
unregister(ConnectionKey) ->
  gen_server:cast(?MODULE, {unregister, ConnectionKey}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  {ok, _Socket} = start_udp(),
  {ok, []}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({unregister, ConnectionKey}, State) ->
  UpdatedState = do_unregister(ConnectionKey, State),
  {noreply, UpdatedState};
handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({udp, Socket, Host, Port, _Msg} = Packet, State) ->
  {NewState, Client} = case is_registered({Host, Port}, State) of
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
%%% Internal functions
%%%===================================================================

start_udp() ->
  Port = irc_app:get_env(udp_port),
  gen_udp:open(Port, [{mode, binary}, {reuseaddr, true}]).

register(Key, ClientPID, State) ->
  [{Key, ClientPID} | State].

is_registered(Key, State) ->
  lists:keyfind(Key, 1, State).

do_unregister(ConnectionKey, State) ->
  lists:keydelete(ConnectionKey, 1, State).
