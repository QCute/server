%%%-------------------------------------------------------------------
%%% @doc
%%% location 
%%% @end
%%%-------------------------------------------------------------------
-module(location).
%% API
-export([on_load/1, on_save/1]).
%% Includes
-include("user.hrl").
-include("map.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc on load
-spec on_load(User :: #user{}) -> NewUser :: #user{}.
on_load(User = #user{role_id = RoleId}) ->
    case location_sql:select(RoleId) of
        [Location] ->
            Location;
        [] ->
            Location = #location{
                role_id = RoleId,
                map_id = map_data:city()
            }
    end,
    User#user{location = Location}.

%% @doc on save
-spec on_save(User :: #user{}) -> NewUser :: #user{}.
on_save(User = #user{role_id = RoleId, location = Location = #location{role_id = 0}}) ->
    NewLocation = Location#location{role_id = RoleId},
    %% insert new
    location_sql:insert(NewLocation),
    User#user{location = NewLocation};
on_save(User = #user{location = Location}) ->
    location_sql:update(Location),
    User.

%%%===================================================================
%%% Internal functions
%%%===================================================================
