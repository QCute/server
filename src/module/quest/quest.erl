%%%-------------------------------------------------------------------
%%% @doc
%%% module quest
%%% @end
%%%-------------------------------------------------------------------
-module(quest).
%% API
-export([load/1, save/1]).
-export([accept/2, submit/2]).
%% Includes
-include("common.hrl").
-include("user.hrl").
-include("quest.hrl").
%%%===================================================================
%%% API
%%%===================================================================
%% @doc load
-spec load(User :: #user{}) -> NewUser :: #user{}.
load(User = #user{role_id = RoleId}) ->
    RawData = quest_sql:select(RoleId),
    Handle = fun(Quest = #quest{progress = Progress}) -> Quest#quest{progress = parser:string_to_term(Progress)} end,
    Data = parser:convert(RawData, quest, Handle),
    User#user{quest = Data}.

%% @doc save
-spec save(User :: #user{}) -> NewUser :: #user{}.
save(User = #user{quest = Quest}) ->
    NewQuest = quest_sql:update_into(Quest),
    User#user{quest = NewQuest}.

%% @doc accept
-spec accept(User :: #user{}, QuestId :: non_neg_integer()) -> {ok, NewQuest :: #quest{}, NewUser :: #user{}} | {error, Code :: non_neg_integer()}.
accept(User, QuestId) ->
    case quest_data:get(QuestId) of
        DataQuest = #quest_data{} ->
            check_pre(User, DataQuest);
        _ ->
            {error, 2}
    end.
check_pre(User = #user{quest = Quest}, DataQuest = #quest_data{group_id = GroupId, pre_id = PreQuestId}) ->
    case lists:keyfind(GroupId, #quest.group_id, Quest) of
        false when PreQuestId =:= 0 ->
            check_cost(User, DataQuest);
        #quest{amount = 0, quest_id = PreQuestId} ->
            check_cost(User, DataQuest);
        #quest{amount = Amount} when 0 < Amount ->
            {error, 3};
        #quest{quest_id = QuestId} when QuestId =/= PreQuestId ->
            {error, 4};
        _ ->
            {error, 5}
    end.
check_cost(User, DataQuest = #quest_data{condition = Condition}) ->
    case user_checker:check(User, Condition) of
        ok ->
            accept_update(User, DataQuest);
        Error ->
            Error
    end.
accept_update(User = #user{role_id = RoleId, quest = QuestList}, #quest_data{quest_id = QuestId, group_id = GroupId, event = Event, target = Target, amount = Amount, compare = Compare, condition = Condition}) ->
    Quest = #quest{role_id = RoleId, quest_id = QuestId, group_id = GroupId, event = Event, target = Target, amount = Amount, compare = Compare, flag = insert},
    {[NewQuest], _} = quest_update:update_quest(User, [], [Quest]),
    NewQuestList = lists:keystore(GroupId, #quest.group_id, QuestList, NewQuest),
    NewUser = User#user{quest = NewQuestList},
    {ok, CostUser} = asset:cost(NewUser, Condition),
    {reply, NewQuest, CostUser}.

%% @doc submit
-spec submit(User :: #user{}, QuestId :: non_neg_integer()) -> {ok, NewUser :: #user{}} | {error, Code :: non_neg_integer()}.
submit(User = #user{quest = QuestList}, QuestId) ->
    case lists:keyfind(QuestId, #quest.quest_id, QuestList) of
        Quest = #quest{amount = 0, award = 0} ->
            award(User, Quest);
        #quest{award = 1} ->
            %% award received
            {error, 3};
        #quest{} ->
            %% amount great then zero
            {error, 2};
        _ ->
            {error, 4}
    end.
award(User = #user{role_id = RoleId, quest = QuestList}, Quest = #quest{quest_id = QuestId}) ->
    case quest_data:get(QuestId) of
        #quest_data{award = Award} ->
            {ok, AwardUser} = item:add(User, Award, ?MODULE),
            NewQuest = Quest#quest{award = 1, flag = update},
            NewQuestList = lists:keystore(QuestId, #quest.quest_id, QuestList, NewQuest),
            %% log
            log:quest_log(RoleId, QuestId, time:ts()),
            {ok, AwardUser#user{quest = NewQuestList}};
        _ ->
            {error, 5}
    end.
