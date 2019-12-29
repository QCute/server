%%%------------------------------------------------------------------
%%% @doc
%%% module Fighter act
%%% @end
%%%------------------------------------------------------------------
-module(monster_act).
%% API
-export([loop/1, act/2]).
%% Includes
-include("common.hrl").
-include("map.hrl").
-include("skill.hrl").
-include("monster.hrl").
-include("protocol.hrl").
%%%==================================================================
%%% API functions
%%%==================================================================
%% @doc loop all Fighter
loop(State = #map_state{fighters = Fighters}) ->
    loop(State, Fighters, []).

loop(State, [], List) ->
    State#map_state{fighters = List};
loop(State, [H | T], List) ->
    case act(State, H) of
        {ok, NewState = #map_state{}} ->
            loop(NewState, T, [H | List]);
        {ok, NewFighter = #fighter{}} ->
            loop(State, T, [NewFighter | List]);
        _ ->
            loop(State, T, [H | List])
    end.

%% @doc act action
act(State, Fighter = #fighter{state = FighterState, act_type = Type}) ->
    case FighterState of
        guard when Type == active orelse Type ==  passive orelse Type ==  movable ->
            %% try move
            move(State, Fighter);
        move when Type == active orelse Type ==  passive orelse Type ==  movable ->
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

%%%==================================================================
%%% Internal functions
%%%==================================================================
%% move
move(State, Fighter = #fighter{hatreds = [_ | _], path = []}) ->
    %% find path
    NewFighter  = monster_agent:select_enemy(State, Fighter),
    {ok, NewFighter#fighter{state = move}};
move(State, Fighter = #fighter{x = OldX, y = OldY, path = [{NewX, NewY} | T]}) ->
    %% move
    NewFighter = Fighter#fighter{x = NewX, y = NewY, path = T},
    map:move(State, NewFighter, OldX, OldY, NewX, NewY),
    {ok, NewFighter};
move(State, Fighter = #fighter{monster_id = MonsterId, act_type = active, camp = Camp}) ->
    %% find hatred
    #monster_data{range = Range} = monster_data:get(MonsterId),
    Enemy = monster_agent:get_slice_enemy(State, Fighter, Range, Camp),
    {ok, Fighter#fighter{state = move, hatreds = Enemy}};
move(_State, Fighter) ->
    {ok, Fighter#fighter{state = move}}.

%% fight
fight(State, Fighter = #fighter{skills = Skills, hatreds = Hatred}) ->
    %% first hatred list object
    SkillId = listing:random(Skills),
    {Enemy, Remain} = lists:split((skill_data:get(listing:random(SkillId)))#skill_data.number, Hatred),
    NewFighter = Fighter#fighter{hatreds = Remain},
    case battle_monster:attack(State, NewFighter, SkillId, Enemy) of
        {ok, NewState} ->
            {ok, NewState};
        {error, _} ->
            {ok, NewFighter#fighter{state = move}}
    end;
fight(State = #map_state{fighters = Fighters}, Fighter = #fighter{id = Id}) ->
    %% no hatred, guard
    NewFighters = lists:keystore(Id, #fighter.id, Fighters, Fighter#fighter{state = guard}),
    {ok, State#map_state{fighters = NewFighters}}.

%% boom
boom(State, Fighter = #fighter{monster_id = MonsterId, x = X, y = Y, camp = Camp, skills = Skills}) ->
    Skill = listing:random(Skills),
    %% range all enemy
    #monster_data{range = Range} = monster_data:get(MonsterId),
    Radius = #slice{left = X - Range, bottom = Y - Range, right = X + Range, top = Y + Range},
    Enemy = monster_agent:get_slice_enemy(State, Fighter, Radius, Camp),
    battle_monster:attack(State, Fighter, Skill, Enemy).
