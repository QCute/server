%%%------------------------------------------------------------------
%%% @doc
%%% module quest update
%%% @end
%%%------------------------------------------------------------------
-module(quest_update).
%% API
-export([update/2, update_quest/3]).
%% Includes
-include("common.hrl").
-include("protocol.hrl").
-include("user.hrl").
-include("event.hrl").
-include("quest.hrl").
%%%==================================================================
%%% API functions
%%%==================================================================
%% @doc update all quest
-spec update(User :: #user{}, Event :: tuple()) -> NewUser :: #user{}.
update(User = #user{quest = Quest}, Event) ->
    {NewUser, NewQuest, UpdateQuest, Result} = update_quest(User, Event, Quest),
    _ = UpdateQuest =/= [] andalso user_sender:send(User, ?PROTOCOL_QUEST, UpdateQuest) == ok,
    {Result, NewUser#user{quest = NewQuest}}.

%% @doc update specified quest
-spec update_quest(User :: #user{}, Event :: tuple(), Quest :: [#quest{}]) -> {NewQuest :: [#quest{}], UpdateQuest :: [#quest{}]}.
update_quest(User, Event, Quest) ->
    %% 如无触发任务, 默认删除触发器
    update_quest_loop(User, Event, Quest, [], [], remove).

%% update per quest
update_quest_loop(User, _, [], List, Update, Result) ->
    {User, List, Update, Result};
update_quest_loop(User, Event, [Quest | T], List, Update, Result) ->
    case do_update_quest(User, Quest, Event) of
        {ok, NewQuest = #quest{number = 0}, NewUser = #user{}} ->
            %% 任务完成, 可删除触发器
            update_quest_loop(NewUser, Event, T, [NewQuest | List], [NewQuest | Update], remove);
        {ok, NewQuest = #quest{}, NewUser = #user{}} ->
            %% 任务还未完成, 需保留触发器
            update_quest_loop(NewUser, Event, T, [NewQuest | List], [NewQuest | Update], ok);
        {ok, NewQuest = #quest{number = 0}} ->
            %% 任务完成, 可删除触发器
            update_quest_loop(User, Event, T, [NewQuest | List], [NewQuest | Update], remove);
        {ok, NewQuest = #quest{}} ->
            %% 任务还未完成, 需保留触发器
            update_quest_loop(User, Event, T, [NewQuest | List], [NewQuest | Update], remove);
        _ ->
            update_quest_loop(User, Event, T, [Quest | List], Update, Result)
    end.

%% update quest detail
%% 新增具体更新任务放在这下面


%% 新增具体更新任务放在这上面
do_update_quest(_User, Quest = #quest{event = Event}, #event{name = Event, target = Target, number = Number}) ->
    update_number(Quest, Target, Number);

do_update_quest(_User, _Quest, _Event) ->
    error.

%% update number with compare mode
update_number(Quest = #quest{number = OldNumber, compare = nc}, _, NewNumber) ->
    %% 不比较
    Quest#quest{number = max(NewNumber - OldNumber, 0), flag = update};
update_number(Quest = #quest{number = OldNumber, target = Target, compare = eq}, Target, NewNumber) ->
    %% 等于
    Quest#quest{number = max(NewNumber - OldNumber, 0), flag = update};
update_number(Quest = #quest{number = OldNumber, target = Target, compare = gt}, ThisTarget, NewNumber) when Target < ThisTarget ->
    %% 大于
    Quest#quest{number = max(NewNumber - OldNumber, 0), flag = update};
update_number(Quest = #quest{number = OldNumber, target = Target, compare = gte}, ThisTarget, NewNumber) when Target =< ThisTarget ->
    %% 大于等于
    Quest#quest{number = max(NewNumber - OldNumber, 0), flag = update};
update_number(Quest = #quest{number = OldNumber, target = Target, compare = le}, ThisTarget, NewNumber) when Target > ThisTarget ->
    %% 小于
    Quest#quest{number = max(NewNumber - OldNumber, 0), flag = update};
update_number(Quest = #quest{number = OldNumber, target = Target, compare = lte}, ThisTarget, NewNumber) when Target >= ThisTarget ->
    %% 小于等于
    Quest#quest{number = max(NewNumber - OldNumber, 0), flag = update}.
