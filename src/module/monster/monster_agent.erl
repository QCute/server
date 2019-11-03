%%%-------------------------------------------------------------------
%%% @doc
%%% module monsters agent
%%% @end
%%%-------------------------------------------------------------------
-module(monster_agent).
%% API
-export([select_enemy/2, get_slice_enemy/3, get_slice_roles/3, get_slice_monsters/3]).
-export([find_path/3]).
%% Includes
-include("map.hrl").
-include("monster.hrl").
-include("attribute.hrl").
%%%===================================================================
%%% API
%%%===================================================================
%% type   : fix move active passive
%% act_script : enemy role monster {monster, id} location
select_enemy(#map_state{}, Fighter = #fighter{act_script = [enemy | _], hatred = [H | T]}) ->
    {Fighter#fighter{hatred = T}, H}.

%% @doc get radius all enemy
-spec get_slice_enemy(State :: #map_state{}, Radius :: #slice{}, Camp :: non_neg_integer()) -> list().
get_slice_enemy(#map_state{fighters = Fighters}, Radius, Camp) ->
    %% alive, diff camp and in slice all roles and monsters
    [{Id, Type} || #fighter{id = Id, type = Type, attribute = #attribute{hp = Hp}, x = X, y = Y, camp = C} <- Fighters, Hp > 0 andalso Camp =/= C andalso map:is_in_slice(X, Y, Radius)].

-spec get_slice_roles(State :: #map_state{}, Radius :: #slice{}, Camp :: non_neg_integer()) -> list().
get_slice_roles(#map_state{fighters = Fighters}, Radius, Camp) ->
    %% alive, diff camp and in slice all roles
    [{Id, Type} || #fighter{id = Id, type = Type = ?MAP_OBJECT_ROLE, attribute = #attribute{hp = Hp}, x = X, y = Y, camp = C} <- Fighters, Hp > 0 andalso Camp =/= C andalso map:is_in_slice(X, Y, Radius)].

-spec get_slice_monsters(State :: #map_state{}, Radius :: #slice{}, Camp :: non_neg_integer()) -> list().
get_slice_monsters(#map_state{fighters = Fighters}, Radius, Camp) ->
    %% alive, diff camp and in slice all monsters
    [{Id, Type} || #fighter{id = Id, type = Type = ?MAP_OBJECT_MONSTER, attribute = #attribute{hp = Hp}, x = X, y = Y, camp = C} <- Fighters, Hp > 0 andalso Camp =/= C andalso map:is_in_slice(X, Y, Radius)].

%% @doc make path
find_path(#map_state{map_id = MapId}, #fighter{id = Id, x = X, y = Y}, #fighter{x = TargetX, y = TargetY}) ->
    path_finder:find(Id, MapId, {X, Y}, {TargetX, TargetY});
find_path(_, _, _) ->
    [].

%%%===================================================================
%%% Internal functions
%%%===================================================================
