%%%-------------------------------------------------------------------
%%% @doc
%%% module monster
%%% @end
%%%-------------------------------------------------------------------
-module(monster).
%% API
-export([create/1]).
-export([select_enemy/2, search_enemy/2]).
-export([get_slice_enemy/2, get_slice_roles/2, get_slice_monsters/2]).
%% Includes
-include("map.hrl").
-include("attribute.hrl").
-include("monster.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc create
-spec create([non_neg_integer() | {non_neg_integer(), non_neg_integer()}]) -> [#fighter{}].
create(List) ->
    create_loop(List, []).

create_loop([], List) ->
    List;
%% id, number
create_loop([{_, 0} | MonsterIdList], List) ->
    create_loop(MonsterIdList, List);
%% id, number
create_loop([{MonsterId, Number} | MonsterIdList], List) ->
    NewList = create_loop([MonsterId], List),
    create_loop([{MonsterId, Number - 1} | MonsterIdList], NewList);
%% id only
create_loop([MonsterId | MonsterIdList], List) ->
    case monster_data:get(MonsterId) of
        #monster_data{type = Type, hp = Hp, skills = Skills, act_type = ActType, act_script = ActScript, camp = Camp, range = Range, distance = Distance, born_points = Points} ->
            {X, Y} = listing:random(Points),
            Fighter = #fighter{
                id = erlang:unique_integer([positive, monotonic]),
                type = ?MAP_OBJECT_MONSTER,
                monster_id = MonsterId,
                monster_type = Type,
                attribute = #attribute{hp = Hp, fc = Hp},
                skill = skill:to_battle_skill(Skills),
                act_type = ActType,
                act_script = ActScript,
                camp = Camp,
                range = Range,
                distance = Distance,
                x = X,
                y = Y
            },
            create_loop(MonsterIdList, [Fighter | List]);
        _ ->
            create_loop(MonsterIdList, List)
    end.

%% @doc select enemy and find path
-spec select_enemy(#map_state{}, #fighter{}) -> #fighter{}.
select_enemy(#map_state{map_id = MapId, fighter = FighterList}, Fighter = #fighter{hatreds = Hatreds}) ->
    select_enemy_loop(Hatreds, MapId, FighterList, Fighter, []).

select_enemy_loop([], _, _, Fighter, List) ->
    Fighter#fighter{hatreds = List};
select_enemy_loop([Hatred = #hatred{id = ThisId} | T], MapId, FighterList, Fighter = #fighter{id = Id, x = X, y = Y, range = Range}, List) ->
    case lists:keyfind(ThisId, #fighter.id, FighterList) of
        false ->
            select_enemy_loop(T, MapId, FighterList, Fighter, List);
        #fighter{attribute = #attribute{hp = 0}} ->
            select_enemy_loop(T, MapId, FighterList, Fighter, List);
        #fighter{x = TargetX, y = TargetY} ->
            case map:is_in_distance({X, Y}, {TargetX, TargetY}, Range) of
                true ->
                    Fighter#fighter{state = fight, hatreds = lists:reverse([Hatred | T], List)};
                false ->
                    path_finder:find(Id, MapId, {X, Y}, {TargetX, TargetY}),
                    Fighter
            end
    end.

%% type: fix move active passive
%% act_script : enemy role monster {monster, subtype} location
%% @doc search enemy
-spec search_enemy(#map_state{}, #fighter{}) -> #fighter{}.
search_enemy(#map_state{fighter = FighterList}, Fighter = #fighter{monster_id = MonsterId, act_script = []}) ->
    %% reset act script
    #monster_data{act_script = ActScript} = monster_data:get(MonsterId),
    %% search an enemy
    {NewActScript, Enemy} = search_enemy_loop(ActScript, Fighter#fighter{act_script = ActScript}, FighterList),
    Fighter#fighter{act_script = NewActScript, hatreds = Enemy};
search_enemy(#map_state{fighter = FighterList}, Fighter = #fighter{act_script = ActScript}) ->
    %% search an enemy
    {NewActScript, Enemy} = search_enemy_loop(ActScript, Fighter#fighter{act_script = ActScript}, FighterList),
    Fighter#fighter{act_script = NewActScript, hatreds = Enemy}.

%% enemy preference
search_enemy_loop([enemy | T], Fighter, FighterList) ->
    {T, search_slice_object(FighterList, Fighter, ?MAP_OBJECT_ANY, 0)};
search_enemy_loop([role | T], Fighter, FighterList) ->
    {T, search_slice_object(FighterList, Fighter, ?MAP_OBJECT_ROLE, 0)};
search_enemy_loop([monster | T], Fighter, FighterList) ->
    {T, search_slice_object(FighterList, Fighter, ?MAP_OBJECT_MONSTER, 0)};
search_enemy_loop([{monster, Subtype} | T], Fighter, FighterList) ->
    {T, search_slice_object(FighterList, Fighter, ?MAP_OBJECT_MONSTER, Subtype)};
search_enemy_loop([_ | T], _, _) ->
    {T, []}.

search_slice_object([], _, _, _) ->
    [];
search_slice_object([#fighter{id = Id} | T], Fighter = #fighter{id = Id}, Type, Subtype) ->
    %% is self
    search_slice_object(T, Fighter, Type, Subtype);
search_slice_object([#fighter{camp = Camp} | T], Fighter = #fighter{camp = Camp}, Type, Subtype) ->
    %% same camp
    search_slice_object(T, Fighter, Type, Subtype);
search_slice_object([#fighter{attribute = #attribute{hp = 0}} | T], Fighter, Type, Subtype) ->
    %% dead
    search_slice_object(T, Fighter, Type, Subtype);
search_slice_object([#fighter{id = ThisId, type = Type, monster_type = MonsterType, x = ThisX, y = ThisY} | T], Fighter = #fighter{x = X, y = Y, distance = Distance}, ?MAP_OBJECT_ANY, MonsterType) ->
    %% all object
    case map:is_in_distance(ThisX, ThisY, X, Y, Distance) of
        true ->
            [#hatred{id = ThisId, type = Type, subtype = MonsterType}];
        false ->
            search_slice_object(T, Fighter, ?MAP_OBJECT_ANY, MonsterType)
    end;
search_slice_object([#fighter{id = ThisId, type = Type, monster_type = Subtype, x = ThisX, y = ThisY} | T], Fighter = #fighter{x = X, y = Y, distance = Distance}, Type, 0) ->
    %% all object
    case map:is_in_distance(ThisX, ThisY, X, Y, Distance) of
        true ->
            [#hatred{id = ThisId, type = 0, subtype = Subtype}];
        false ->
            search_slice_object(T, Fighter, 0, Subtype)
    end;
search_slice_object([#fighter{id = ThisId, type = Type, monster_type = MonsterType, x = ThisX, y = ThisY} | T], Fighter = #fighter{x = X, y = Y, distance = Distance}, Type, MonsterType) ->
    %% this type object
    case map:is_in_distance(ThisX, ThisY, X, Y, Distance) of
        true ->
            [#hatred{id = ThisId, type = Type, subtype = MonsterType}];
        false ->
            search_slice_object(T, Fighter, Type, MonsterType)
    end;
search_slice_object([_ | T], Fighter, Type, MonsterType) ->
    %% other object
    search_slice_object(T, Fighter, Type, MonsterType).

%% @doc get range all enemy
-spec get_slice_enemy(State :: #map_state{}, #fighter{}) -> list().
get_slice_enemy(#map_state{fighter = FighterList}, Fighter = #fighter{}) ->
    %% alive, diff camp and in the slice all roles and monsters
    get_slice_object(FighterList, Fighter, 0, []).

%% @doc get range roles
-spec get_slice_roles(State :: #map_state{}, #fighter{}) -> list().
get_slice_roles(#map_state{fighter = FighterList}, Fighter = #fighter{}) ->
    %% alive, diff camp and in the slice all roles
    get_slice_object(FighterList, Fighter, ?MAP_OBJECT_ROLE, []).

%% @doc get range monsters
-spec get_slice_monsters(State :: #map_state{}, #fighter{}) -> list().
get_slice_monsters(#map_state{fighter = FighterList}, Fighter = #fighter{}) ->
    %% alive, diff camp and in the slice all monsters
    get_slice_object(FighterList, Fighter, ?MAP_OBJECT_MONSTER, []).

get_slice_object([], _, _, List) ->
    List;
get_slice_object([#fighter{id = Id} | T], Fighter = #fighter{id = Id}, Type, List) ->
    %% is self
    get_slice_object(T, Fighter, Type, List);
get_slice_object([#fighter{camp = Camp} | T], Fighter = #fighter{camp = Camp}, Type, List) ->
    %% same camp
    get_slice_object(T, Fighter, Type, List);
get_slice_object([#fighter{attribute = #attribute{hp = 0}} | T], Fighter, Type, List) ->
    %% dead
    get_slice_object(T, Fighter, Type, List);
get_slice_object([#fighter{id = ThisId, type = Type, monster_type = MonsterType, x = ThisX, y = ThisY} | T], Fighter = #fighter{x = X, y = Y, distance = Distance}, ?MAP_OBJECT_ANY, List) ->
    %% all object
    case map:is_in_distance(ThisX, ThisY, X, Y, Distance) of
        true ->
            Hatred = #hatred{id = ThisId, type = Type, subtype = MonsterType},
            get_slice_object(T, Fighter, ?MAP_OBJECT_ANY, [Hatred | List]);
        false ->
            get_slice_object(T, Fighter, ?MAP_OBJECT_ANY, List)
    end;
get_slice_object([#fighter{id = ThisId, type = Type, monster_type = MonsterType, x = ThisX, y = ThisY} | T], Fighter = #fighter{x = X, y = Y, distance = Distance}, Type, List) ->
    %% this type object
    case map:is_in_distance(ThisX, ThisY, X, Y, Distance) of
        true ->
            Hatred = #hatred{id = ThisId, type = Type, subtype = MonsterType},
            get_slice_object(T, Fighter, Type, [Hatred | List]);
        false ->
            get_slice_object(T, Fighter, Type, List)
    end;
get_slice_object([_ | T], Fighter, Type, List) ->
    %% other object
    get_slice_object(T, Fighter, Type, List).
