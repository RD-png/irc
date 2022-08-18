%%%-------------------------------------------------------------------
%%% @author Ryan User <ryan@nixos-laptop>
%%% @copyright (C) 2022, Ryan User
%%% @doc
%%%
%%% @end
%%% Created : 18 Aug 2022 by Ryan User <ryan@nixos-laptop>
%%%-------------------------------------------------------------------
-module(irc_client).

-include("irc.hrl").

%% API
-export([register_client/2]).

%%%===================================================================
%%% API
%%%===================================================================

-spec register_client(Name, Protocol) -> ok when
    Name     :: string(),
    Protocol :: socket_connection().
register_client(Name, Protocol) ->  
  Register = fun() ->
                 Client = #client{id       = uuid:uuid1(),
                                  name     = Name,
                                  joined   = time(),
                                  protocol = Protocol},
                 mnesia:write(Client)
             end,
  mnesia:activity(transaction, Register).
