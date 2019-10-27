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
        {ok, NewMonster = #fighter{}} ->
            loop(State, T, [NewMonster | List]);
        _ ->
            loop(State, T, [H | List])
    end.

%% @doc act action
act(State, Monster = #fighter{state = MonsterState, act_type = Type}) ->
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
move(State, Monster = #fighter{path = [], hatred = [_ | _]}) ->
    {NewMonster, Enemy} = monster_agent:select_enemy(State, Monster),
    monster_agent:find_path(State, NewMonster, Enemy),
    {ok, Monster#fighter{state = move}};
move(State, Monster = #fighter{id = Id, x = OldX, y = OldY, path = [{NewX, NewY} | T]}) ->
    NewMonster = Monster#fighter{x = NewX, y = NewY, path = T},
    {ok, Binary} = map_protocol:write(?PROTOCOL_MAP_MONSTER_MOVE, [NewX, NewY]),
    map:move(State, Id, OldX, OldY, NewX, NewY, Binary),
    {ok, NewMonster};
move(State, Monster = #fighter{id = Id, x = OldX, y = OldY, path = [[NewX, NewY] | T]}) ->
    NewMonster = Monster#fighter{x = NewX, y = NewY, path = T},
    {ok, Binary} = map_protocol:write(?PROTOCOL_MAP_MONSTER_MOVE, [NewX, NewY]),
    map:move(State, Id, OldX, OldY, NewX, NewY, Binary),
    {ok, NewMonster};
move(State, Monster = #fighter{act_type = active, camp = Camp}) ->
    monster_agent:get_slice_enemy(State, 10, Camp),
    E = [{fighter, Id} || #fighter{id = Id} <- State#map_state.roles],
    {ok, Monster#fighter{state = move, hatred = E}};
move(_State, Monster) ->
    {ok, Monster#fighter{state = guard}}.

fight(State, Monster = #fighter{hatred = [H | T]}) ->
    %% first hatred list object
    battle_monster:attack(State, Monster#fighter{hatred = T}, H);
fight(State = #map_state{monsters = List}, Monster = #fighter{id = Id}) ->
    %% no hatred, guard
    NewMonster = Monster#fighter{state = guard},
    NewMonsterList = lists:keystore(Id, #fighter.id, List, NewMonster),
    {ok, State#map_state{monsters = NewMonsterList}}.

boom(State, Monster = #fighter{monster_id = MonsterId, x = X, y = Y, camp = Camp}) ->
    %% range 500 all enemy
    %% this can supported by data config
    #monster_data{range = Range} = monster_data:get(MonsterId),
    Radius = #slice{left = X - Range, bottom = Y - Range, right = X + Range, top = Y + Range},
    Enemy = monster_agent:get_slice_enemy(State, Radius, Camp),
    battle_monster:attack(State, Monster, Enemy).