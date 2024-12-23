%%%-------------------------------------------------------------------
%%% @doc
%%% achievement
%%% @end
%%%-------------------------------------------------------------------
-module(achievement).
%% API
-export([on_load/1, on_save/1]).
-export([query_count/1, query/1]).
-export([award/2]).
%% Includes
-include("common.hrl").
-include("user.hrl").
-include("achievement.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc on load
-spec on_load(User :: #user{}) -> NewUser :: #user{}.
on_load(User = #user{role_id = RoleId}) ->
    Achievement = achievement_sql:select(RoleId),
    User#user{achievement = Achievement}.

%% @doc on save
-spec on_save(User :: #user{}) -> NewUser :: #user{}.
on_save(User = #user{achievement = Achievement}) ->
    NewAchievement = achievement_sql:save(Achievement),
    User#user{achievement = NewAchievement}.

%% @doc query
-spec query_count(User :: #user{}) -> ok().
query_count(#user{count = Count}) ->
    {ok, Count}.

%% @doc query
-spec query(User :: #user{}) -> ok().
query(#user{achievement = Achievement}) ->
    {ok, Achievement}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% @doc award
-spec award(User :: #user{}, AchievementId :: non_neg_integer()) -> ok() | error().
award(User, AchievementId) ->
    case achievement_data:get(AchievementId) of
        AchievementData = #achievement_data{} ->
            receive_award(User, AchievementData);
        _ ->
            {error, configure_not_found}
    end.

receive_award(User = #user{role_id = RoleId, achievement = AchievementList}, #achievement_data{pre_id = AchievementId, type = Type, count_type = CountType, number = NeedNumber, award = Award}) ->
    Number = count:get_total(User, CountType),
    case lists:keyfind(Type, #achievement.type, AchievementList) of
        Achievement = #achievement{achievement_id = AchievementId} when Number >= NeedNumber ->
            %% award
            {ok, AwardUser} = item:add(User, Award, ?MODULE),
            %% update achievement
            NewAchievement = Achievement#achievement{achievement_id = AchievementId, flag = 1},
            NewAchievementList = lists:keystore(Type, #achievement.type, AchievementList, NewAchievement),
            %% log
            log:achievement_log(RoleId, AchievementId, time:now()),
            {ok, ok, AwardUser#user{achievement = NewAchievementList}};
        #achievement{} when Number < NeedNumber ->
            %% number great then zero
            {error, achievement_not_completed};
        #achievement{} ->
            %% previous award not received
            {error, award_pre_not_received};
        _ ->
            {error, achievement_not_found}
    end.
