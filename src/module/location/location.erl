%%%-------------------------------------------------------------------
%%% @doc
%%% location 
%%% @end
%%%-------------------------------------------------------------------
-module(location).
%% API
-export([load/1, save/1]).
%% Includes
-include("common.hrl").
-include("user.hrl").
-include("map.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc load
-spec load(User :: #user{}) -> NewUser :: #user{}.
load(User = #user{role_id = RoleId}) ->
    case location_sql:select(RoleId) of
        [Location] ->
            Location;
        [] ->
            Location = #location{
                role_id = RoleId,
                map_id = 100000
            }
    end,
    User#user{location = Location}.

%% @doc save
-spec save(User :: #user{}) -> NewUser :: #user{}.
save(User = #user{role_id = RoleId, location = Location = #location{role_id = 0}}) ->
    NewLocation = Location#location{role_id = RoleId},
    %% insert new
    location_sql:insert(NewLocation),
    User#user{location = NewLocation};
save(User = #user{location = Location}) ->
    location_sql:update(Location),
    User.

%%%===================================================================
%%% Internal functions
%%%===================================================================
