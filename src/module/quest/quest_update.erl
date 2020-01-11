%%%------------------------------------------------------------------
%%% @doc
%%% module quest update
%%% @end
%%%------------------------------------------------------------------
-module(quest_update).
%% API
-export([check/3]).
-export([apply_check/5, apply_check/7]).
-export([update/2]).
%% Includes
-include("common.hrl").
-include("protocol.hrl").
-include("user.hrl").
-include("event.hrl").
-include("quest.hrl").
%%%==================================================================
%%% API functions
%%%==================================================================
%% @doc update quest when accept
-spec check(User :: #user{}, Event :: tuple(), Quest :: [#quest{}]) -> {NewQuest :: [#quest{}], UpdateQuest :: [#quest{}]}.
check(User, Quest, #quest_data{module = []}) ->
    {User, Quest};
check(User, Quest, #quest_data{function = []}) ->
    {User, Quest};
check(User, Quest = #quest{event = Event, compare = Compare, target = Target, number = Number}, #quest_data{module = Module, function = Function}) ->
    case catch Module:Function(User, Event) of
        #event_checker{data = Integer} when is_integer(Integer) ->
            NewNumber = apply_check(Integer, Event, Compare, Target, Number),
            NewUser = check_add_event(User, Event, NewNumber),
            {NewUser, Quest#quest{number = NewNumber}};
        #event_checker{data = List, key = KeyIndex, value = ValueIndex} when is_list(List) ->
            NewNumber = apply_check(List, KeyIndex, ValueIndex, Event, Compare, Target, Number),
            NewUser = check_add_event(User, Event, NewNumber),
            {NewUser, Quest#quest{number = NewNumber}};
        {'EXIT', {What, _}} ->
            ?PRINT("~w:~w check quest exit with: ~w", [Module, Function, What]),
            NewUser = check_add_event(User, Event, Number),
            {NewUser, Quest};
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
-spec update(User :: #user{}, Event :: tuple()) -> NewUser :: #user{}.
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
            %% 任务完成, 可删除触发器
            update_quest_loop(User, Event, T, [NewQuest | List], [NewQuest | Update], remove);
        NewQuest = #quest{} ->
            %% 任务还未完成, 需保留触发器
            update_quest_loop(User, Event, T, [NewQuest | List], [NewQuest | Update], remove);
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
    max(OldNumber - NewNumber, 0).
