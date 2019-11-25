%%%------------------------------------------------------------------
%%% @doc
%%% module role event
%%% @end
%%%------------------------------------------------------------------
-module(user_event).
%% API
-export([handle/2]).
%% Includes
-include("common.hrl").
-include("user.hrl").
-include("event.hrl").

%%%==================================================================
%%% API functions
%%%==================================================================
%% @doc handle event
-spec handle(User :: #user{}, Event :: tuple() | [tuple()]) -> NewUser :: #user{}.
handle(User, Events) when is_list(Events) ->
    %% multi event
    handle_loop(User, Events);
handle(User, Event) ->
    %% single event
    handle_loop(User, [Event]).

%%%==================================================================
%%% Internal functions
%%%==================================================================
handle_loop([], User) ->
    User;
handle_loop([Event | T], User) ->
    NewUser = do_handle(Event, User),
    handle_loop(T, NewUser).

%% handle specific event
do_handle(Event = #event_level_upgrade{}, User) ->
    quest_update:update(User, Event);
do_handle(Event = #event_guild_join{}, User) ->
    quest_update:update(User, Event);
do_handle(Event = #event_kill_monster{}, User) ->
    quest_update:update(User, Event);
do_handle(Event = #event_pass_dungeon{}, User) ->
    quest_update:update(User, Event);
do_handle(Event = #event_shop_buy{}, User) ->
    quest_update:update(User, Event);
do_handle(_, User) ->
    User.
