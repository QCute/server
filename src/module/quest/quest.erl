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
    NewUser = user_event:add(User, [begin QuestData = #quest_data{event = Event} = quest_data:get(QuestId), user_server:apply_cast(Pid, ?MODULE, check, [Quest, QuestData]), #trigger{name = Event, module = ?MODULE, function = update} end || Quest = #quest{quest_id = QuestId} <- QuestList]),
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

accept_update(User = #user{role_id = RoleId, quest = QuestList}, QuestData = #quest_data{quest_id = QuestId, type = Type, target = Target, number = Number, compare = Compare}) ->
    Quest = #quest{role_id = RoleId, quest_id = QuestId, type = Type, target = Target, number = Number, compare = Compare, flag = 1},
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
check(User, Quest = #quest{compare = Compare, target = Target, number = Number}, QuestData = #quest_data{event = Event}) ->
    case check_module(QuestData) of
        [] ->
            %% do not check after add
            {User, Quest};
        Module ->
            CurrentNumber = erlang:apply(Module, check_quest, [User, Event, Target]),
            NewNumber = update_number(Number, Target, Compare, Target, CurrentNumber),
            case NewNumber of
                0 ->
                    {User, Quest#quest{number = NewNumber}};
                _ ->
                    {user_event:add(User, #trigger{name = Event, module = ?MODULE, function = update}), Quest#quest{number = NewNumber}}
            end
    end.

%% quest check module map @here
check_module(#quest_data{event = event_level_upgrade}) ->
    role;
check_module(#quest_data{event = event_guild_join}) ->
    role;
check_module(#quest_data{event = event_dungeon_passed}) ->
    dungeon;
check_module(#quest_data{event = event_friend_add}) ->
    friend;
check_module(#quest_data{event = event_shop_buy}) ->
    shop;
check_module(_) ->
    [].

%% @doc update quest when event happen
-spec update(User :: #user{}, Event :: tuple()) -> {ok | remove, NewUser :: #user{}}.
update(User = #user{quest = Quest}, Event) ->
    {NewUser, NewQuest, UpdateQuest, Result} = update_quest_loop(User, Event, Quest, [], [], remove),
    _ = UpdateQuest =/= [] andalso user_sender:send(User, ?PROTOCOL_QUEST, UpdateQuest) == ok,
    {Result, NewUser#user{quest = NewQuest}}.

%% update per quest
update_quest_loop(User, _, [], List, Update, Result) ->
    {User, List, Update, Result};
update_quest_loop(User, Event, [Quest = #quest{quest_id = QuestId} | T], List, Update, Result) ->
    case do_update_quest(User, Quest, quest_data:get(QuestId), Event) of
        NewQuest = #quest{number = 0} ->
            %% 任务完成, 可删除触发器(默认)
            update_quest_loop(User, Event, T, [NewQuest | List], [NewQuest | Update], Result);
        NewQuest = #quest{} ->
            %% 任务还未完成, 需保留触发器
            update_quest_loop(User, Event, T, [NewQuest | List], [NewQuest | Update], ok);
        _ ->
            update_quest_loop(User, Event, T, [Quest | List], Update, Result)
    end.

%% update quest detail @here if the default update handler not satisfy



do_update_quest(_User, Quest = #quest{compare = Compare, target = QuestTarget, number = QuestNumber}, #quest_data{event = Event}, #event{name = Event, target = Target, number = Number}) ->
    NewNumber = update_number(QuestNumber, QuestTarget, Compare, Target, Number),
    Quest#quest{number = NewNumber, flag = 1};

do_update_quest(_User, _Quest, _QuestData, _Event) ->
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
