%%%-------------------------------------------------------------------
%%% @doc
%%% module monster act
%%% @end
%%%-------------------------------------------------------------------
-module(monster_act).
%% API
-export([loop/1, act/2]).
%% Includes
-include("monster.hrl").
-include("map.hrl").
-include("protocol.hrl").
%%%===================================================================
%%% API
%%%===================================================================
%% @doc loop all monster
loop(State = #map_state{monsters = Monsters}) ->
    loop(State, Monsters, []).

loop(State, [], List) ->
    State#map_state{monsters = List};
loop(State, [H | T], List) ->
    case act(State, H) of
        {ok, NewState = #map_state{}} ->
            loop(NewState, T, [H | List]);
        {ok, NewMonster = #monster{}} ->
            loop(State, T, [NewMonster | List]);
        _ ->
            loop(State, T, [H | List])
    end.

%% @doc act action
act(State, Monster = #monster{state = MonsterState, act_type = Type}) ->
    case MonsterState of
        guard when Type == active orelse Type ==  passive orelse Type ==  movable ->
            %% try move
            move(State, Monster);
        move when Type == active orelse Type ==  passive orelse Type ==  movable ->
            move(State, Monster);
        fight ->
            fight(State, Monster);
        boom ->
            boom(State, Monster);
        trace ->
            ok;
        die ->
            ok;
        relive ->
            ok;
        _ ->
            ok
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
move(State, Monster = #monster{path = [], hatred = [_ | _]}) ->
    {NewMonster, Enemy} = monster_agent:select_enemy(State, Monster),
    monster_agent:find_path(State, NewMonster, Enemy),
    {ok, Monster#monster{state = move}};
move(State, Monster = #monster{id = Id, x = OldX, y = OldY, path = [{NewX, NewY} | T]}) ->
    NewMonster = Monster#monster{x = NewX, y = NewY, path = T},
    {ok, Binary} = map_protocol:write(?PROTOCOL_MAP_MONSTER, [NewX, NewY]),
    map:move(State, Id, OldX, OldY, NewX, NewY, Binary),
    {ok, NewMonster};
move(State, Monster = #monster{id = Id, x = OldX, y = OldY, path = [[NewX, NewY] | T]}) ->
    NewMonster = Monster#monster{x = NewX, y = NewY, path = T},
    {ok, Binary} = map_protocol:write(?PROTOCOL_MAP_MONSTER, [NewX, NewY]),
    map:move(State, Id, OldX, OldY, NewX, NewY, Binary),
    {ok, NewMonster};
move(State, Monster = #monster{act_type = active, camp = Camp}) ->
    monster_agent:get_slice_enemy(State, 10, Camp),
    E = [{fighter, Id} || #fighter{id = Id} <- State#map_state.fighters],
    {ok, Monster#monster{state = move, hatred = E}};
move(_State, Monster) ->
    {ok, Monster#monster{state = guard}}.

fight(State, Monster = #monster{hatred = [H | T]}) ->
    %% first hatred list object
    battle_fighter:attack(State, Monster#monster{hatred = T}, H);
fight(State = #map_state{monsters = List}, Monster = #monster{id = Id}) ->
    %% no hatred, guard
    NewMonster = Monster#monster{state = guard},
    NewMonsterList = lists:keystore(Id, #monster.id, List, NewMonster),
    {ok, State#map_state{monsters = NewMonsterList}}.

boom(State, Monster = #monster{monster_id = MonsterId, x = X, y = Y, camp = Camp}) ->
    %% range 500 all enemy
    %% this can supported by data config
    #data_monster{attack_distance = Range} = monster_data:get(MonsterId),
    Radius = #slice{left = X - Range, bottom = Y - Range, right = X + Range, top = Y + Range},
    Enemy = monster_agent:get_slice_enemy(State, Radius, Camp),
    battle_fighter:attack(State, Monster, Enemy).