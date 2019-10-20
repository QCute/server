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
select_enemy(#map_state{roles = Roles, monsters = Monsters}, Monster = #fighter{act_script = [enemy | _], hatred = Hatred}) ->
    case Hatred of
        [{monster, Id} | T] ->
            case lists:keyfind(Id, #fighter.id, Monsters) of
                Enemy = #fighter{attribute = #attribute{hp = Hp}} when 0 =< Hp ->
                    {Monster, Enemy};
                _ ->
                    {Monster#fighter{hatred = T}, []}
            end;
        [{role, Id} | T] ->
            case lists:keyfind(Id, #fighter.id, Roles) of
                Enemy = #fighter{attribute = #attribute{hp = Hp}} when 0 =< Hp ->
                    {Monster, Enemy};
                _ ->
                    {Monster#fighter{hatred = T}, []}
            end;
        [] ->
            {Monster, []}
    end;
select_enemy(#map_state{roles = Roles}, Monster = #fighter{act_script = [role | _], hatred = Hatred}) ->
    case lists:keytake(role, 1, Hatred) of
        {value, {role, Id}, T} ->
            case lists:keyfind(Id, #fighter.id, Roles) of
                Enemy = #fighter{attribute = #attribute{hp = Hp}} when 0 =< Hp ->
                    {Monster#fighter{hatred = [{role, Id} | T]}, Enemy};
                _ ->
                    {Monster#fighter{hatred = T}, []}
            end;
        _ ->
            {Monster, []}
    end;
select_enemy(#map_state{monsters = Monsters}, Monster = #fighter{act_script = [monster | _], hatred = Hatred}) ->
    case Hatred of
        [{monster, Id} | T] ->
            case lists:keyfind(Id, #fighter.id, Monsters) of
                Enemy = #fighter{attribute = #attribute{hp = Hp}} when 0 =< Hp ->
                    {Monster, Enemy};
                _ ->
                    {Monster#fighter{hatred = T}, []}
            end;
        _ ->
            {Monster, []}
    end;
select_enemy(#map_state{monsters = Monsters}, Monster = #fighter{act_script = [{monster, GroupId} | _], hatred = Hatred}) ->
    case Hatred of
        [{monster, Id} | T] ->
            case lists:keyfind(Id, #fighter.id, Monsters) of
                Enemy = #fighter{attribute = #attribute{hp = Hp}, group_id = GroupId} when 0 < Hp ->
                    {Monster, Enemy};
                #fighter{attribute = #attribute{hp = Hp}} when 0 =< Hp ->
                    %% non preference group monster
                    {Monster#fighter{hatred = T}, []};
                _ ->
                    {Monster#fighter{hatred = T}, []}
            end;
        _ ->
            {Monster, []}
    end.

%% @doc get radius all enemy
-spec get_slice_enemy(State :: #map_state{}, Radius :: #slice{}, Camp :: non_neg_integer()) -> list().
get_slice_enemy(#map_state{roles = Roles, monsters = Monsters}, Radius, Camp) ->
    %% alive, diff camp and in slice all roles and monsters
    F = fun
        (#fighter{attribute = #attribute{hp = Hp}, x = X, y = Y, camp = C}) ->
            Hp > 0 andalso Camp =/= C andalso map:is_in_slice(X, Y, Radius)
    end,
    lists:filter(F, Roles ++ Monsters).

-spec get_slice_roles(State :: #map_state{}, Radius :: #slice{}, Camp :: non_neg_integer()) -> list().
get_slice_roles(#map_state{roles = Roles}, Radius, Camp) ->
    %% alive, diff camp and in slice all roles and monsters
    F = fun
        (#fighter{attribute = #attribute{hp = Hp}, x = X, y = Y, camp = C}) ->
            Hp > 0 andalso (Camp == 0 orelse Camp =/= C) andalso map:is_in_slice(X, Y, Radius)
    end,
    lists:filter(F, Roles).

-spec get_slice_monsters(State :: #map_state{}, Radius :: #slice{}, Camp :: non_neg_integer()) -> list().
get_slice_monsters(#map_state{monsters = Monsters}, Radius, Camp) ->
    %% alive, diff camp and in slice all roles and monsters
    F = fun
        (#fighter{attribute = #attribute{hp = Hp}, x = X, y = Y, camp = C}) ->
            Hp > 0 andalso Camp =/= C andalso map:is_in_slice(X, Y, Radius)
    end,
    lists:filter(F, Monsters).

%% @doc make path
find_path(#map_state{map_id = MapId}, #fighter{id = Id, x = X, y = Y}, #fighter{x = TargetX, y = TargetY}) ->
    path_finder:find(Id, MapId, {X, Y}, {TargetX, TargetY});
find_path(_, _, _) ->
    [].
%%%===================================================================
%%% Internal functions
%%%===================================================================
