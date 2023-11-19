%%%-------------------------------------------------------------------
%%% @doc
%%% skill
%%% @end
%%%-------------------------------------------------------------------
-module(skill).
%% API
-export([on_load/1, on_save/1]).
-export([query/1]).
-export([learn/2]).
-export([to_battle_skill/1]).
%% Includes
-include("common.hrl").
-include("user.hrl").
-include("map.hrl").
-include("skill.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc on load
-spec on_load(User :: #user{}) -> NewUser :: #user{}.
on_load(User = #user{role_id = RoleId}) ->
    Skill = skill_sql:select(RoleId),
    User#user{skill = Skill}.

%% @doc on save
-spec on_save(User :: #user{}) -> NewUser :: #user{}.
on_save(User = #user{skill = Skill}) ->
    NewSkill = skill_sql:save(Skill),
    User#user{skill = NewSkill}.

%% @doc query
-spec query(User :: #user{}) -> ok().
query(#user{skill = Skill}) ->
    {ok, Skill}.

%% @doc learn
-spec learn(User :: #user{}, SkillId :: non_neg_integer()) -> ok() | error().
learn(User = #user{role_id = RoleId, skill = SkillList}, SkillId) ->
    Skill = listing:key_find(SkillId, #skill.skill_id, SkillList, #skill{role_id = RoleId, skill_id = SkillId}),
    case skill_data:get(SkillId) of
        SkillData = #skill_data{} ->
            check_condition(User, Skill, SkillData);
        _ ->
            {error, configure_not_found}
    end.

check_condition(User, Skill, SkillData = #skill_data{condition = Condition}) ->
    case user_checker:check(User, Condition) of
        ok ->
            check_cost(User, Skill, SkillData);
        _ ->
            {error, condition_not_met}
    end.

check_cost(User, Skill, SkillData = #skill_data{cost = Cost}) ->
    case item:cost(User, Cost, skill) of
        {ok, NewUser} ->
            upgrade_level(NewUser, Skill, SkillData);
        _ ->
            {error, item_not_enough}
    end.

upgrade_level(User = #user{skill = SkillList}, Skill = #skill{skill_id = SkillId, level = Level}, #skill_data{attribute = Attribute}) ->
    NewSkill = Skill#skill{level = Level + 1, flag = 1},
    NewSkillList = lists:keystore(SkillId, #skill.skill_id, SkillList, NewSkill),
    NewUser = attribute:recalculate(User, {?MODULE, SkillId}, Attribute),
    {ok, ok, NewUser#user{skill = NewSkillList}}.

%% @doc convert skill id/skill to battle skill
-spec to_battle_skill([non_neg_integer() | #skill{}]) -> [#battle_skill{}].
to_battle_skill(List) ->
    to_battle_skill_loop(List, []).

to_battle_skill_loop([], List) ->
    List;
to_battle_skill_loop([#skill{skill_id = SkillId, level = Level} | T], List)  ->
    #skill_data{type = Type, cd = Cd, distance = Distance, number = Number, effect = Effect} = skill_data:get(SkillId),
    to_battle_skill_loop(T, [#battle_skill{skill_id = SkillId, level = Level, type = Type, cd = Cd, distance = Distance, number = Number, effect = Effect} | List]);
to_battle_skill_loop([SkillId | T], List)  ->
    #skill_data{type = Type, cd = Cd, distance = Distance, number = Number, effect = Effect} = skill_data:get(SkillId),
    to_battle_skill_loop(T, [#battle_skill{skill_id = SkillId, type = Type, cd = Cd, distance = Distance, number = Number, effect = Effect} | List]).

%%%===================================================================
%%% Internal functions
%%%===================================================================
