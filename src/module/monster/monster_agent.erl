%%%------------------------------------------------------------------
%%% @doc
%%% module monsters agent
%%% @end
%%%------------------------------------------------------------------
-module(monster_agent).
%% API
-export([select_enemy/2, get_slice_enemy/4, get_slice_roles/4, get_slice_monsters/4]).
%% Includes
-include("map.hrl").
-include("monster.hrl").
-include("attribute.hrl").
%%%==================================================================
%%% API functions
%%%==================================================================
%% type   : fix move active passive
%% act_script : enemy role monster {monster, id} location
-spec select_enemy(#map_state{}, #fighter{}) -> #fighter{}.
select_enemy(#map_state{map_id = MapId, fighters = Fighters}, Fighter = #fighter{id = Id, x = X, y = Y, hatreds = Hatreds}) ->
    NewHatreds = select_enemy_loop(Hatreds, MapId, Fighters, Id, X, Y, []),
    Fighter#fighter{hatreds = NewHatreds}.

select_enemy_loop([], _, _, _, _, _, List) ->
    List;
select_enemy_loop([Hatred = #hatred{id = ThisId} | T], MapId, Fighters, Id, X, Y, List) ->
    case lists:keyfind(ThisId, #fighter.id, Fighters) of
        false ->
            select_enemy_loop(T, MapId, Fighters, Id, X, Y, List);
        #fighter{attribute = #attribute{hp = 0}} ->
            select_enemy_loop(T, MapId, Fighters, Id, X, Y, List);
        #fighter{x = TargetX, y = TargetY} ->
            path_finder:find(Id, MapId, {X, Y}, {TargetX, TargetY}),
            lists:reverse([Hatred | List], T)
    end.

%% @doc get radius all enemy
-spec get_slice_enemy(State :: #map_state{}, #fighter{}, Radius :: #slice{}, Camp :: non_neg_integer()) -> list().
get_slice_enemy(#map_state{fighters = Fighters}, #fighter{id = Id, x = X, y = Y}, Radius, Camp) ->
    %% alive, diff camp and in slice all roles and monsters
    get_slice_object(Fighters, Id, 0, Radius, Camp, X, Y, []).

-spec get_slice_roles(State :: #map_state{}, #fighter{}, Radius :: #slice{}, Camp :: non_neg_integer()) -> list().
get_slice_roles(#map_state{fighters = Fighters}, #fighter{id = Id, x = X, y = Y}, Radius, Camp) ->
    %% alive, diff camp and in slice all roles
    get_slice_object(Fighters, Id, ?MAP_OBJECT_ROLE, Radius, Camp, X, Y, []).

-spec get_slice_monsters(State :: #map_state{}, #fighter{}, Radius :: #slice{}, Camp :: non_neg_integer()) -> list().
get_slice_monsters(#map_state{fighters = Fighters}, #fighter{id = Id, x = X, y = Y}, Radius, Camp) ->
    %% alive, diff camp and in slice all monsters
    get_slice_object(Fighters, Id, ?MAP_OBJECT_MONSTER, Radius, Camp, X, Y, []).

get_slice_object([], _, _, _, _, _, _, List) ->
    List;
get_slice_object([#fighter{id = Id} | T], Id, Type, Radius, Camp, X, Y, List) ->
    get_slice_object(T, Id, Type, Radius, Camp, X, Y, List);
get_slice_object([#fighter{camp = Camp} | T], Id, Type, Radius, Camp, X, Y, List) ->
    get_slice_object(T, Id, Type, Radius, Camp, X, Y, List);
get_slice_object([#fighter{attribute = #attribute{hp = 0}} | T], Id, Type, Radius, Camp, X, Y, List) ->
    get_slice_object(T, Id, Type, Radius, Camp, X, Y, List);
get_slice_object([#fighter{id = ThisId, group_id = GroupId, x = ThisX, y = ThisY} | T], Id, 0, Radius, Camp, X, Y, List) ->
    case map:is_in_distance(ThisX, ThisY, X, Y, Radius) of
        true ->
            Hatred = #hatred{id = ThisId, type = 0, group_id = GroupId},
            get_slice_object(T, Id, 0, Radius, Camp, X, Y, [Hatred | List]);
        false ->
            get_slice_object(T, Id, 0, Radius, Camp, X, Y, List)
    end;
get_slice_object([#fighter{id = ThisId, type = Type, group_id = GroupId, x = ThisX, y = ThisY} | T], Id, Type, Radius, Camp, X, Y, List) ->
    case map:is_in_distance(ThisX, ThisY, X, Y, Radius) of
        true ->
            Hatred = #hatred{id = ThisId, type = Type, group_id = GroupId},
            get_slice_object(T, Id, Type, Radius, Camp, X, Y, [Hatred | List]);
        false ->
            get_slice_object(T, Id, Type, Radius, Camp, X, Y, List)
    end;
get_slice_object([_ | T], Id, Type, Radius, Camp, X, Y, List) ->
    get_slice_object(T, Id, Type, Radius, Camp, X, Y, List).


%%%==================================================================
%%% Internal functions
%%%==================================================================
