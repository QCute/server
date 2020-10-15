%%%-------------------------------------------------------------------
%%% @doc
%%% quest
%%% @end
%%%-------------------------------------------------------------------
-module(quest).
%% API
-export([load/1, save/1]).
-export([query/1]).
-export([accept/2, submit/2]).
-export([update/2]).
%% Includes
-include("common.hrl").
-include("protocol.hrl").
-include("event.hrl").
-include("user.hrl").
-include("dungeon.hrl").
-include("quest.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc load
-spec load(User :: #user{}) -> NewUser :: #user{}.
load(User = #user{role_id = RoleId}) ->
    QuestList = quest_sql:select(RoleId),
    lists:foldl(fun(Quest = #quest{quest_id = QuestId}, AccUser = #user{quest = AccQuestList}) -> {NewUser, NewQuest} = check(AccUser, Quest, quest_data:get(QuestId)), NewUser#user{quest = [NewQuest | AccQuestList]} end, User, QuestList).

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
    user_sender:send(NewUser, ?PROTOCOL_QUEST_QUERY, [NewQuest]),
    {ok, ok, NewUser#user{quest = NewQuestList}}.

%% update quest when accept
check(User, Quest = #quest{compare = Compare, target = Target, number = Number}, QuestData = #quest_data{event = Event}) ->
    %% check current target and number
    {CheckTarget, CheckNumber} = handle_check(User, QuestData),
    %% update target number
    NewNumber = update_number(Number, Target, Compare, CheckTarget, CheckNumber),
    case NewNumber of
        0 ->
            {User, Quest#quest{number = NewNumber}};
        _ ->
            {user_event:add_trigger(User, #trigger{name = Event, module = ?MODULE, function = update}), Quest#quest{number = NewNumber}}
    end.

%% quest check module map @here

%% check role level
handle_check(User, #quest_data{event = event_level_upgrade}) ->
    {role:level(User), 1};

%% check role guild id
handle_check(User, #quest_data{event = event_guild_join}) ->
    {role:guild_id(User), 1};

%% check passed any dungeon
handle_check(User, #quest_data{event = event_dungeon_passed}) ->
    {0, dungeon:get_number(User)};

%% check passed exp dungeon
handle_check(User, #quest_data{event = event_dungeon_exp_passed}) ->
    {dungeon:get_current(User, ?DUNGEON_TYPE_EXP), 1};

%% check passed copper dungeon
handle_check(User, #quest_data{event = event_dungeon_copper_passed}) ->
    {dungeon:get_current(User, ?DUNGEON_TYPE_COPPER), 1};

%% check add number of friend
handle_check(User, #quest_data{event = event_friend_add}) ->
    {0, friend:get_number(User)};

%% check buy any number of shop
handle_check(User, #quest_data{event = event_shop_buy, target = 0}) ->
    {0, shop:get_number(User)};

%% check buy this target number of shop
handle_check(User, #quest_data{event = event_shop_buy, target = Target}) ->
    {Target, shop:get_number(User, Target)};

%% none of all
handle_check(_, _) ->
    {0, 0}.

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
            log:quest_log(RoleId, QuestId, time:now()),
            {ok, ok, AwardUser#user{quest = NewQuestList}};
        _ ->

            {error, configure_not_found}
    end.

%% @doc update quest when event happen
-spec update(User :: #user{}, Event :: tuple()) -> {ok | remove, NewUser :: #user{}}.
update(User = #user{quest = Quest}, Event) ->
    {NewUser, NewQuest, UpdateQuest, Result} = update_quest_loop(User, Event, Quest, [], [], remove),
    _ = UpdateQuest =/= [] andalso user_sender:send(User, ?PROTOCOL_QUEST_QUERY, UpdateQuest) == ok,
    {Result, NewUser#user{quest = NewQuest}}.

%% update per quest
update_quest_loop(User, _, [], List, Update, Result) ->
    {User, List, Update, Result};
update_quest_loop(User, Event, [Quest = #quest{quest_id = QuestId} | T], List, Update, Result) ->
    case do_update_quest(User, Quest, quest_data:get(QuestId), Event) of
        NewQuest = #quest{number = 0} ->
            %% quest finished, remove event trigger(by default)
            update_quest_loop(User, Event, T, [NewQuest | List], [NewQuest | Update], Result);
        NewQuest = #quest{} ->
            %% quest not finish, keep it
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

%% do not compare check target and current target
update_number(OldNumber, _, nc, _, NewNumber) ->
    max(OldNumber - NewNumber, 0);

%% check target equal then current target
update_number(OldNumber, Target, eq, Target, NewNumber) ->
    max(OldNumber - NewNumber, 0);

%% check target great then current target
update_number(OldNumber, Target, gt, ThisTarget, NewNumber) when Target < ThisTarget ->
    max(OldNumber - NewNumber, 0);

%% check target great then equal current target
update_number(OldNumber, Target, ge, ThisTarget, NewNumber) when Target =< ThisTarget ->
    max(OldNumber - NewNumber, 0);

%% check target less then current target
update_number(OldNumber, Target, le, ThisTarget, NewNumber) when Target > ThisTarget ->
    max(OldNumber - NewNumber, 0);

%% check target less then equal current target
update_number(OldNumber, Target, le, ThisTarget, NewNumber) when Target >= ThisTarget ->
    max(OldNumber - NewNumber, 0);

%% none of all
update_number(OldNumber, _, _, _, _) ->
    OldNumber.

%%%===================================================================
%%% Internal functions
%%%===================================================================
