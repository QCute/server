%%%-------------------------------------------------------------------
%%% @doc
%%% module skill
%%% @end
%%%-------------------------------------------------------------------
-module(skill).
%% API
-export([load/1, save/1]).
-export([query/1]).
-export([learn/2]).
%% Includes
-include("common.hrl").
-include("user.hrl").
-include("skill.hrl").
-include("protocol.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%% @doc load
-spec load(User :: #user{}) -> NewUser :: #user{}.
load(User = #user{role_id = RoleId}) ->
    Skill =  parser:convert(skill_sql:select(RoleId), ?MODULE),
    User#user{skill = Skill}.

%% @doc save
-spec save(User :: #user{}) -> NewUser :: #user{}.
save(User = #user{skill = Skill}) ->
    NewSkill = skill_sql:insert_update(Skill),
    User#user{skill = NewSkill}.

%% @doc query
-spec query(User :: #user{}) -> ok().
query(#user{skill = Skill}) ->
    {ok, [Skill]}.

%% @doc learn
-spec learn(User :: #user{}, SkillId :: non_neg_integer()) -> ok() | error().
learn(User = #user{role_id = RoleId, skill = SkillList}, SkillId) ->
    Skill = listing:key_find(SkillId, #skill.skill_id, SkillList, #skill{role_id = RoleId, skill_id = SkillId}),
    case skill_data:get(SkillId) of
        SkillData = #skill_data{} ->
            check_condition(User, Skill, SkillData);
        _ ->
            {error, 2}
    end.
    
check_condition(User, Skill, #skill_data{condition = Condition, stuff = Stuff}) ->
    case user_checker:check(User, Condition) of
        {ok, _} ->
            case item:check(User, Stuff) of
                {ok, ItemList} ->
                    {ok, NewUser} = item:reduce(User, ItemList, skill),
                    upgrade_level(NewUser, Skill);
                _ ->
                    {error, 4}
            end;
        _ ->
            {error, 3}
    end.

upgrade_level(User = #user{skill = SkillList}, Skill = #skill{skill_id = SkillId, level = Level}) ->
    NewSkill = Skill#skill{level = Level + 1, flag = 1},
    NewSkillList = lists:keystore(SkillId, #skill.skill_id, SkillList, NewSkill),
    {ok, 1, User#user{skill = NewSkillList}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================