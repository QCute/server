%%%-------------------------------------------------------------------
%%% @doc
%%% module quest
%%% @end
%%%-------------------------------------------------------------------
-module(quest).
%% API
-export([load/1, save/1]).
-export([query/1]).
-export([accept/2, submit/2]).
-export([check/3]).
-export([apply_check/5, apply_check/7]).
-export([update/2]).
%% Includes
-include("common.hrl").
-include("protocol.hrl").
-include("user.hrl").
-include("event.hrl").
-include("quest.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc load
-spec load(User :: #user{}) -> NewUser :: #user{}.
load(User = #user{role_id = RoleId, pid = Pid}) ->
    QuestList = quest_sql:select(RoleId),
    NewUser = user_event:add(User, [begin user_server:apply_cast(Pid, ?MODULE, check, [Quest, quest_data:get(QuestId)]), #trigger{name = Event, module = ?MODULE, function = update} end || Quest = #quest{quest_id = QuestId, event = Event} <- QuestList]),
    NewUser#user{quest = QuestList}.

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

check_pre(User = #user{quest = Quest}, QuestData = #quest_data{type = Type, pre_id = PreQuestId}) ->
    case lists:keyfind(Type, #quest.type, Quest) of
        false when PreQuestId == 0 ->
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
        ok ->
            accept_cost(User, QuestData);
        _ ->
            {error, condition_not_met}
    end.

accept_cost(User, QuestData = #quest_data{cost = Cost}) ->
    case item:cost(User, Cost, quest) of
        {ok, NewUser} ->
            accept_update(NewUser, QuestData);
        _ ->
            {error, asset_not_enough}
    end.

accept_update(User = #user{role_id = RoleId, quest = QuestList}, QuestData = #quest_data{quest_id = QuestId, type = Type, event = Event, target = Target, number = Number, compare = Compare}) ->
    Quest = #quest{role_id = RoleId, quest_id = QuestId, type = Type, event = Event, target = Target, number = Number, compare = Compare, flag = 1},
    %% check it finished when accept
    {NewUser, NewQuest} = check(User, Quest, QuestData),
    NewQuestList = lists:keystore(Type, #quest.type, QuestList, NewQuest),
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

%% @doc update quest when accept
-spec check(User :: #user{}, Quest :: #quest{}, QuestData :: #quest_data{}) -> {NewUser :: #user{}, NewQuest :: #quest{}}.
check(User, Quest, #quest_data{module = []}) ->
    {User, Quest};
check(User, Quest, #quest_data{function = []}) ->
    {User, Quest};
check(User, Quest = #quest{event = Event, compare = Compare, target = Target, number = Number}, #quest_data{module = Module, function = Function}) ->
    case erlang:apply(Module, Function, [User, Event]) of
        #event_checker{data = Integer} when is_integer(Integer) ->
            NewNumber = apply_check(Integer, Event, Compare, Target, Number),
            NewUser = check_add_event(User, Event, NewNumber),
            {NewUser, Quest#quest{number = NewNumber}};
        #event_checker{data = List, key = KeyIndex, value = ValueIndex} when is_list(List) ->
            NewNumber = apply_check(List, KeyIndex, ValueIndex, Event, Compare, Target, Number),
            NewUser = check_add_event(User, Event, NewNumber),
            {NewUser, Quest#quest{number = NewNumber}};
        What ->
            ?PRINT("~w:~w check quest unknown return: ~w", [Module, Function, What]),
            NewUser = check_add_event(User, Event, Number),
            {NewUser, Quest}
    end.

check_add_event(User, _, 0) ->
    User;
check_add_event(User, Event, _) ->
    user_event:add(User, #trigger{name = Event, module = ?MODULE, function = update}).

%% @doc quest apply check
-spec apply_check(Integer :: non_neg_integer(), Event :: atom(), Compare :: atom(), Target :: non_neg_integer(), Number :: non_neg_integer()) -> non_neg_integer().
apply_check(Integer, _, Compare, Target, Number) ->
    update_number(Number, Target, Compare, Integer, 1).

%% @doc quest apply check
-spec apply_check(list(), KeyIndex :: pos_integer(), ValueIndex :: pos_integer(), Event :: atom(), Compare :: atom(), Target :: non_neg_integer(), Number :: non_neg_integer()) -> non_neg_integer().
apply_check(List, _, _, _, nc, _, Number) ->
    {ok, max(Number - length(List), 0)};
apply_check(List, _, _, _, _, 0, Number) ->
    {ok, max(Number - length(List), 0)};
apply_check(List, KeyIndex, ValueIndex, _, Compare, Target, Number) ->
    case lists:keyfind(Target, KeyIndex, List) of
        false ->
            Number;
        Element ->
            NewNumber = erlang:element(ValueIndex, Element),
            update_number(Number, Target, Compare, Target, NewNumber)
    end.

%% @doc update quest when event happen
-spec update(User :: #user{}, Event :: tuple()) -> {ok | remove, NewUser :: #user{}}.
update(User = #user{quest = Quest}, Event) ->
    {NewUser, NewQuest, UpdateQuest, Result} = update_quest_loop(User, Event, Quest, [], [], remove),
    _ = UpdateQuest =/= [] andalso user_sender:send(User, ?PROTOCOL_QUEST, UpdateQuest) == ok,
    {Result, NewUser#user{quest = NewQuest}}.

%% update per quest
update_quest_loop(User, _, [], List, Update, Result) ->
    {User, List, Update, Result};
update_quest_loop(User, Event, [Quest | T], List, Update, Result) ->
    case do_update_quest(User, Quest, Event) of
        NewQuest = #quest{number = 0} ->
            %% 任务完成, 可删除触发器(默认)
            update_quest_loop(User, Event, T, [NewQuest | List], [NewQuest | Update], Result);
        NewQuest = #quest{} ->
            %% 任务还未完成, 需保留触发器
            update_quest_loop(User, Event, T, [NewQuest | List], [NewQuest | Update], ok);
        _ ->
            update_quest_loop(User, Event, T, [Quest | List], Update, Result)
    end.

%% update quest detail
%% 新增具体更新任务放在这下面


%% 新增具体更新任务放在这上面
do_update_quest(_User, Quest = #quest{event = Event, compare = Compare, target = QuestTarget, number = QuestNumber}, #event{name = Event, target = Target, number = Number}) ->
    NewNumber = update_number(QuestNumber, QuestTarget, Compare, Target, Number),
    Quest#quest{number = NewNumber, flag = 1};

do_update_quest(_User, _Quest, _Event) ->
    error.

%% update number with compare mode
update_number(OldNumber, _, nc, _, NewNumber) ->
    %% 不比较
    max(OldNumber - NewNumber, 0);
update_number(OldNumber, Target, eq, Target, NewNumber) ->
    %% 等于
    max(OldNumber - NewNumber, 0);
update_number(OldNumber, Target, gt, ThisTarget, NewNumber) when Target < ThisTarget ->
    %% 大于
    max(OldNumber - NewNumber, 0);
update_number(OldNumber, Target, ge, ThisTarget, NewNumber) when Target =< ThisTarget ->
    %% 大于等于
    max(OldNumber - NewNumber, 0);
update_number(OldNumber, Target, le, ThisTarget, NewNumber) when Target > ThisTarget ->
    %% 小于
    max(OldNumber - NewNumber, 0);
update_number(OldNumber, Target, le, ThisTarget, NewNumber) when Target >= ThisTarget ->
    %% 小于等于
    max(OldNumber - NewNumber, 0);
update_number(OldNumber, _, _, _, _) ->
    OldNumber.
