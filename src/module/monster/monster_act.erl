%%%-------------------------------------------------------------------
%%% @doc
%%% monster act
%%% @end
%%%-------------------------------------------------------------------
-module(monster_act).
%% API
-export([loop/1, act/2]).
%% Includes
-include("common.hrl").
-include("map.hrl").
-include("skill.hrl").
-include("monster.hrl").
-include("protocol.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc monster loop
-spec loop(State :: #map{}) -> NewState :: #map{}.
loop(State = #map{fighter = FighterList}) ->
    loop(State, FighterList).

loop(State, []) ->
    State;
loop(State = #map{fighter = FighterList}, [H = #fighter{type = ?MAP_OBJECT_MONSTER} | T]) ->
    case act(State, H) of
        {ok, NewFighter = #fighter{id = Id}} ->
            NewFighterList = lists:keyreplace(Id, #fighter.id, FighterList, NewFighter),
            loop(State#map{fighter = NewFighterList}, T);
        {ok, NewState = #map{}} ->
            loop(NewState, T);
        _ ->
            loop(State, T)
    end;
loop(State, [_ | T]) ->
    loop(State, T).

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% act action
act(State, Fighter = #fighter{data = #fighter_monster{state = FighterState, act_type = Type}}) ->
    case FighterState of
        guard when Type == active orelse Type == passive orelse Type == movable ->
            %% try move
            move(State, Fighter);
        move when Type == active orelse Type == passive orelse Type == movable ->
            move(State, Fighter);
        fight ->
            fight(State, Fighter);
        boom ->
            boom(State, Fighter);
        trace ->
            ok;
        die ->
            ok;
        relive ->
            ok;
        _ ->
            ok
    end.

%% move
move(State, Fighter = #fighter{data = #fighter_monster{hatreds = [_ | _], path = []}}) ->
    %% find path
    NewFighter = monster:select_enemy(State, Fighter),
    {ok, NewFighter};
move(State, Fighter = #fighter{x = OldX, y = OldY, data = FighterMonster = #fighter_monster{path = [{NewX, NewY} | T]}}) ->
    %% move
    NewFighter = Fighter#fighter{x = NewX, y = NewY, data = FighterMonster#fighter_monster{path = T}},
    map:move(State, OldX, OldY, NewFighter),
    {ok, NewFighter};
move(State, Fighter = #fighter{data = #fighter_monster{act_type = active}}) ->
    %% search enemy
    NewFighter = monster:search_enemy(State, Fighter),
    {ok, NewFighter};
move(_State, Fighter = #fighter{data = FighterMonster}) ->
    {ok, Fighter#fighter{data = FighterMonster#fighter_monster{state = move}}}.

%% fight
fight(State, Fighter = #fighter{skill = Skills, data = FighterMonster = #fighter_monster{hatreds = Hatred = [_ | _]}}) ->
    %% first hatred list object
    Skill = #battle_skill{skill_id = SkillId} = listing:random(Skills),
    Enemy = lists:sublist(Hatred, (skill_data:get(SkillId))#skill_data.number),
    case battle_monster:attack(State, Fighter, Skill, Enemy) of
        {ok, NewState} ->
            {ok, NewState};
        {error, skill_cd} ->
            {ok, State};
        {error, _} ->
            {ok, Fighter#fighter{data = FighterMonster#fighter_monster{state = move}}}
    end;
fight(_State, Fighter = #fighter{data = FighterMonster}) ->
    %% no hatred, guard
    {ok, Fighter#fighter{data = FighterMonster#fighter_monster{state = move}}}.

%% boom
boom(State, Fighter = #fighter{skill = Skills}) ->
    %% chose skill
    Skill = listing:random(Skills),
    %% range all enemy
    Enemy = monster:get_slice_enemy(State, Fighter),
    %% start fight
    battle_monster:attack(State, Fighter, Skill, Enemy).
