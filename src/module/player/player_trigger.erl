%%%-------------------------------------------------------------------
%%% @doc
%%% module trigger
%%% @end
%%%-------------------------------------------------------------------
-module(player_trigger).
%% API
-export([fire/2]).
-include("common.hrl").
-include("trigger.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%% @doc trigger fire
fire(User, Event) ->
    do_fire(User, Event).

%%%===================================================================
%%% Internal functions
%%%===================================================================
do_fire([Event | T], User) ->
    NewUser = do_fire(Event, User),
    do_fire(T, NewUser);
do_fire(#event_enter{}, User) ->

    User.