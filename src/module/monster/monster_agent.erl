%%%-------------------------------------------------------------------
%%% @doc
%%% module monsters agent
%%% @end
%%%-------------------------------------------------------------------
-module(monster_agent).
%% API
-export([select_enemy/2, get_slice_enemy/3, get_slice_fighters/3, get_slice_monsters/3]).
-export([find_path/3]).
%% Includes
-include("monster.hrl").
-include("map.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%% type   : fix move active passive
%% act_script : enemy fighter monster {monster, id} location
select_enemy(#map_state{fighters = Fighters, monsters = Monsters}, Monster = #monster{act_script = [enemy | _], hatred = Hatred}) ->
    case Hatred of
        [{monster, Id} | T] ->
            case lists:keyfind(Id, #monster.id, Monsters) of
                Enemy = #monster{hp = Hp} when 0 =< Hp ->
                    {Monster, Enemy};
                _ ->
                    {Monster#monster{hatred = T}, []}
            end;
        [{fighter, Id} | T] ->
            case lists:keyfind(Id, #fighter.id, Fighters) of
                Enemy = #fighter{hp = Hp} when 0 =< Hp ->
                    {Monster, Enemy};
                _ ->
                    {Monster#monster{hatred = T}, []}
            end;
        [] ->
            {Monster, []}
    end;
select_enemy(#map_state{fighters = Fighters}, Monster = #monster{act_script = [fighter | _], hatred = Hatred}) ->
    case lists:keytake(fighter, 1, Hatred) of
        {value, {fighter, Id}, T} ->
            case lists:keyfind(Id, #fighter.id, Fighters) of
                Enemy = #fighter{hp = Hp} when 0 =< Hp ->
                    {Monster#monster{hatred = [{fighter, Id} | T]}, Enemy};
                _ ->
                    {Monster#monster{hatred = T}, []}
            end;
        _ ->
            {Monster, []}
    end;
select_enemy(#map_state{monsters = Monsters}, Monster = #monster{act_script = [monsters | _], hatred = Hatred}) ->
    case Hatred of
        [{monster, Id} | T] ->
            case lists:keyfind(Id, #monster.id, Monsters) of
                Enemy = #monster{hp = Hp} when 0 =< Hp ->
                    {Monster, Enemy};
                _ ->
                    {Monster#monster{hatred = T}, []}
            end;
        _ ->
            {Monster, []}
    end;
select_enemy(#map_state{monsters = Monsters}, Monster = #monster{act_script = [{monsters, GroupId} | _], hatred = Hatred}) ->
    case Hatred of
        [{monster, Id} | T] ->
            case lists:keyfind(Id, #monster.id, Monsters) of
                Enemy = #monster{hp = Hp, group_id = GroupId} when 0 < Hp ->
                    {Monster, Enemy};
                #monster{hp = Hp} when 0 =< Hp ->
                    %% non preference group monster
                    {Monster#monster{hatred = T}, []};
                _ ->
                    {Monster#monster{hatred = T}, []}
            end;
        _ ->
            {Monster, []}
    end.

%% @doc get radius all enemy
-spec get_slice_enemy(State :: #map_state{}, Radius :: #slice{}, Camp :: non_neg_integer()) -> list().
get_slice_enemy(#map_state{fighters = Fighters, monsters = Monsters}, Radius, Camp) ->
    %% alive, diff camp and in slice all fighters and monsters
    F = fun
        (#monster{hp = Hp, x = X, y = Y, camp = C}) ->
            Hp > 0 andalso Camp =/= C andalso map:is_in_slice(X, Y, Radius);
        (#fighter{hp = Hp, x = X, y = Y, camp = C}) ->
            Hp > 0 andalso Camp =/= C andalso map:is_in_slice(X, Y, Radius)
    end,
    lists:filter(F, Fighters ++ Monsters).

-spec get_slice_fighters(State :: #map_state{}, Radius :: #slice{}, Camp :: non_neg_integer()) -> list().
get_slice_fighters(#map_state{fighters = Fighters}, Radius, Camp) ->
    %% alive, diff camp and in slice all fighters and monsters
    F = fun
        (#fighter{hp = Hp, x = X, y = Y, camp = C}) ->
            Hp > 0 andalso (Camp == 0 orelse Camp =/= C) andalso map:is_in_slice(X, Y, Radius)
    end,
    lists:filter(F, Fighters).

-spec get_slice_monsters(State :: #map_state{}, Radius :: #slice{}, Camp :: non_neg_integer()) -> list().
get_slice_monsters(#map_state{monsters = Monsters}, Radius, Camp) ->
    %% alive, diff camp and in slice all fighters and monsters
    F = fun
        (#monster{hp = Hp, x = X, y = Y, camp = C}) ->
            Hp > 0 andalso Camp =/= C andalso map:is_in_slice(X, Y, Radius)
    end,
    lists:filter(F, Monsters).

%% @doc make path
find_path(#map_state{map_id = MapId}, #monster{id = Id, x = X, y = Y}, #monster{x = TargetX, y = TargetY}) ->
    path_finder:find(Id, MapId, {X, Y}, {TargetX, TargetY});
find_path(#map_state{map_id = MapId}, #monster{id = Id, x = X, y = Y}, #fighter{x = TargetX, y = TargetY}) ->
    path_finder:find(Id, MapId, {X, Y}, {TargetX, TargetY});
find_path(_, _, _) ->
    [].
%%%===================================================================
%%% Internal functions
%%%===================================================================
