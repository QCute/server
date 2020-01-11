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
-include("protocol.hrl").
-include("user.hrl").
-include("event.hrl").
-include("quest.hrl").
%%%==================================================================
%%% API functions
%%%==================================================================
%% @doc load
-spec load(User :: #user{}) -> NewUser :: #user{}.
load(User = #user{role_id = RoleId}) ->
    Quest = quest_sql:select(RoleId),
    NewUser = user_event:add(User, [#trigger{name = Event, module = quest_update, function = update} || #quest{event = Event} <- Quest]),
    NewUser#user{quest = Quest}.

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
            check_condition(User, QuestData);
        #quest{number = 0, quest_id = PreQuestId} ->
            check_condition(User, QuestData);
        #quest{number = Number} when 0 < Number ->
            {error, pre_quest_not_complete};
        #quest{quest_id = QuestId} when QuestId =/= PreQuestId ->
            {error, not_next_quest};
        _ ->
            {error, no_such_quest}
    end.

check_condition(User, QuestData = #quest_data{condition = Condition}) ->
    case user_checker:check(User, Condition) of
        {ok, _} ->
            accept_cost(User, QuestData);
        _ ->
            {error, condition_not_met}
    end.

accept_cost(User, QuestData = #quest_data{cost = Cost}) ->
    case asset:cost(User, Cost, quest) of
        {ok, NewUser} ->
            accept_update(NewUser, QuestData);
        _ ->
            {error, asset_not_enough}
    end.

accept_update(User = #user{role_id = RoleId, quest = QuestList}, QuestData = #quest_data{quest_id = QuestId, group_id = GroupId, event = Event, target = Target, number = Number, compare = Compare}) ->
    Quest = #quest{role_id = RoleId, quest_id = QuestId, group_id = GroupId, event = Event, target = Target, number = Number, compare = Compare, flag = 1},
    %% check it finished when accept
    {NewUser, NewQuest} = quest_update:check(User, Quest, QuestData),
    NewQuestList = lists:keystore(GroupId, #quest.group_id, QuestList, NewQuest),
    %% cost asset
    %% {ok, CostUser} = asset:cost(NewUser#user{quest = NewQuestList}, Cost, ?MODULE),
    %% update quest list
    user_sender:send(NewUser, ?PROTOCOL_QUEST, [NewQuest]),
    {ok, ok, NewUser#user{quest = NewQuestList}}.

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
            NewQuest = Quest#quest{award = 1, flag = 1},
            NewQuestList = lists:keystore(QuestId, #quest.quest_id, QuestList, NewQuest),
            %% log
            log:quest_log(RoleId, QuestId, time:ts()),
            {ok, ok, AwardUser#user{quest = NewQuestList}};
        _ ->

            {error, configure_not_found}
    end.
