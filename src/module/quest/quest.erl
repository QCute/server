%%%------------------------------------------------------------------
%%% @doc
%%% module quest
%%% @end
%%%------------------------------------------------------------------
-module(quest).
%% API
-export([load/1, save/1]).
-export([query/1]).
-export([accept/2, submit/2]).
%% Includes
-include("common.hrl").
-include("user.hrl").
-include("protocol.hrl").
-include("quest.hrl").
%%%==================================================================
%%% API functions
%%%==================================================================
%% @doc load
-spec load(User :: #user{}) -> NewUser :: #user{}.
load(User = #user{role_id = RoleId}) ->
    Quest = parser:convert(quest_sql:select(RoleId), ?MODULE),
    User#user{quest = Quest}.

%% @doc save
-spec save(User :: #user{}) -> NewUser :: #user{}.
save(User = #user{quest = Quest}) ->
    NewQuest = quest_sql:insert_update(Quest),
    User#user{quest = NewQuest}.

%% @doc query
-spec query(User :: #user{}) -> ok().
query(#user{quest = Quest}) ->
    {ok, Quest}.

%% @doc accept
-spec accept(User :: #user{}, QuestId :: non_neg_integer()) -> ok() | error().
accept(User, QuestId) ->
    case quest_data:get(QuestId) of
        QuestData = #quest_data{} ->
            check_pre(User, QuestData);
        _ ->
            {error, configure_not_found}
    end.
check_pre(User = #user{quest = Quest}, QuestData = #quest_data{group_id = GroupId, pre_id = PreQuestId}) ->
    case lists:keyfind(GroupId, #quest.group_id, Quest) of
        false when PreQuestId =:= 0 ->
            check_cost(User, QuestData);
        #quest{number = 0, quest_id = PreQuestId} ->
            check_cost(User, QuestData);
        #quest{number = Number} when 0 < Number ->
            {error, pre_quest_not_complete};
        #quest{quest_id = QuestId} when QuestId =/= PreQuestId ->
            {error, not_next_quest};
        _ ->
            {error, no_such_quest}
    end.
check_cost(User, QuestData = #quest_data{condition = Condition}) ->
    case user_checker:check(User, Condition) of
        {ok, Cost} ->
            accept_update(User, QuestData, Cost);
        _ ->
            {error, condition_not_enough}
    end.
accept_update(User = #user{role_id = RoleId, quest = QuestList}, #quest_data{quest_id = QuestId, group_id = GroupId, event = Event, target = Target, number = Number, compare = Compare}, Cost) ->
    Quest = #quest{role_id = RoleId, quest_id = QuestId, group_id = GroupId, event = Event, target = Target, number = Number, compare = Compare, flag = insert},
    {[NewQuest], _} = quest_update:update_quest(User, [], [Quest]),
    NewQuestList = lists:keystore(GroupId, #quest.group_id, QuestList, NewQuest),
    NewUser = User#user{quest = NewQuestList},
    {ok, CostUser} = asset:cost(NewUser, Cost),
    %% update quest list
    user_sender:send(CostUser, ?PROTOCOL_QUEST, [NewQuest]),
    {ok, ok, CostUser}.

%% @doc submit
-spec submit(User :: #user{}, QuestId :: non_neg_integer()) -> ok() | error().
submit(User = #user{quest = QuestList}, QuestId) ->
    case lists:keyfind(QuestId, #quest.quest_id, QuestList) of
        Quest = #quest{number = 0, award = 0} ->
            award(User, Quest);
        #quest{award = 1} ->
            %% award received
            {error, quest_already_submit};
        #quest{} ->
            %% number great then zero
            {error, quest_not_complete};
        _ ->
            {error, no_such_quest}
    end.
award(User = #user{role_id = RoleId, quest = QuestList}, Quest = #quest{quest_id = QuestId}) ->
    case quest_data:get(QuestId) of
        #quest_data{award = Award} ->
            {ok, AwardUser} = item:add(User, Award, ?MODULE),
            NewQuest = Quest#quest{award = 1, flag = update},
            NewQuestList = lists:keystore(QuestId, #quest.quest_id, QuestList, NewQuest),
            %% log
            log:quest_log(RoleId, QuestId, time:ts()),
            {ok, ok, AwardUser#user{quest = NewQuestList}};
        _ ->

            {error, configure_not_found}
    end.
