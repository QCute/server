%%%-------------------------------------------------------------------
%%% @doc
%%% module trigger
%%% @end
%%%-------------------------------------------------------------------
-module(player_event).
%% API
-export([handle/2]).
-include("common.hrl").
-include("event.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%% @doc handle event
handle(User, Event) ->
    do_handle(User, Event).

%%%===================================================================
%%% Internal functions
%%%===================================================================
do_handle([Event | T], User) ->
    NewUser = do_handle(Event, User),
    do_handle(T, NewUser);
do_handle(#event_enter{}, User) ->
    User;
do_handle(_, User) ->
    User.