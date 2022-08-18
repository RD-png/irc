%%%-------------------------------------------------------------------
%%% @author Ryan User <ryan@nixos-desktop>
%%% @copyright (C) 2022, Ryan User
%%% Created :  18 Aug 2022 by Ryan User <ryan@nixos-desktop>
%%%-------------------------------------------------------------------
-module(irc_mnesia).
-author("ryandenby").

-include("irc.hrl").

%% API
-export([start/0,
         stop/0]).

-define(MNESIA_GENERIC_OPTS, [{type, ordered_set}]).

%%%-------------------------------------------------------------------
%% API
%%%-------------------------------------------------------------------

start() ->
  create_tables().

stop() ->
  mnesia:stop().

%%%-------------------------------------------------------------------
%% Internal functions
%%%-------------------------------------------------------------------

create_tables() ->
  mnesia_create_client_table(),
  mnesia_create_channel_table().

mnesia_create_client_table() ->
   {atomic, ok} = mnesia:create_table(
                   client,
                   [{attributes, record_info(fields, client)}]
                   ++ ?MNESIA_GENERIC_OPTS
                  ).


mnesia_create_channel_table() ->
   {atomic, ok} = mnesia:create_table(
                   channel,
                   [{attributes, record_info(fields, channel)}]
                   ++ ?MNESIA_GENERIC_OPTS
                  ).
