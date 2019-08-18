%%%-------------------------------------------------------------------
%%% @doc
%%% module quest update
%%% @end
%%%-------------------------------------------------------------------
-module(quest_update).
%% API
-export([update/2, update_quest/3]).
%% Includes
-include("common.hrl").
-include("user.hrl").
-include("quest.hrl").
-include("event.hrl").
-include("protocol.hrl").
%%%===================================================================
%%% API
%%%===================================================================
%% @doc update all quest
-spec update(User :: #user{}, Event :: tuple()) -> NewUser :: #user{}.
update(User = #user{quest = Quest}, Event) ->
    {NewQuest, UpdateQuest} = update_quest(User, Event, Quest),
    user_sender:send(User, ?PROTOCOL_QUEST, UpdateQuest),
    User#user{quest = NewQuest}.

%% @doc update specified quest
-spec update_quest(User :: #user{}, Event :: tuple(), Quest :: [#quest{}]) -> {NewQuest :: [#quest{}], UpdateQuest :: [#quest{}]}.
update_quest(User, Event, Quest) ->
    update_quest_loop(User, Event, Quest, [], []).

%% update per quest
update_quest_loop(_, _, [], List, Update) ->
    {List, Update};
update_quest_loop(User, Event, [Quest | T], List, Update) ->
    case do_update_quest(User, Quest, Event) of
        {ok, NewQuest = #quest{}, NewUser = #user{}} ->
            update_quest_loop(NewUser, Event, T, [NewQuest | List], [NewQuest | Update]);
        {ok, NewQuest = #quest{}} ->
            update_quest_loop(User, Event, T, [NewQuest | List], [NewQuest | Update]);
        _ ->
            update_quest_loop(User, Event, T, [Quest | List], Update)
    end.

%% update amount with compare mode
update_amount(Quest = #quest{amount = OldAmount, compare = nc}, _, NewAmount) ->
    %% 不比较
    Quest#quest{amount = max(NewAmount - OldAmount, 0), flag = update};
update_amount(Quest = #quest{amount = OldAmount, target = Target, compare = eq}, Target, NewAmount) ->
    %% 等于
    Quest#quest{amount = max(NewAmount - OldAmount, 0), flag = update};
update_amount(Quest = #quest{amount = OldAmount, target = Target, compare = gt}, ThisTarget, NewAmount) when Target < ThisTarget ->
    %% 大于
    Quest#quest{amount = max(NewAmount - OldAmount, 0), flag = update};
update_amount(Quest = #quest{amount = OldAmount, target = Target, compare = gte}, ThisTarget, NewAmount) when Target =< ThisTarget ->
    %% 大于等于
    Quest#quest{amount = max(NewAmount - OldAmount, 0), flag = update};
update_amount(Quest = #quest{amount = OldAmount, target = Target, compare = le}, ThisTarget, NewAmount) when Target > ThisTarget ->
    %% 小于
    Quest#quest{amount = max(NewAmount - OldAmount, 0), flag = update};
update_amount(Quest = #quest{amount = OldAmount, target = Target, compare = lte}, ThisTarget, NewAmount) when Target >= ThisTarget ->
    %% 小于等于
    Quest#quest{amount = max(NewAmount - OldAmount, 0), flag = update}.

%% 具体更新任务放在下面
%% update quest detail
do_update_quest(_User, Quest = #quest{event = event_kill_monster}, #event_kill_monster{monster_id = MonsterId, amount = Amount}) ->
    %% 杀怪
    update_amount(Quest, MonsterId, Amount);
do_update_quest(_User, Quest = #quest{event = event_guild_join}, #event_guild_join{}) ->
    %% 加入公会
    update_amount(Quest, 0, 1);
do_update_quest(_User, Quest = #quest{event = event_pass_dungeon}, #event_pass_dungeon{dungeon_id = DungeonId})  ->
    %% 通关副本,向下兼容(通关后面的,前面的也算通关)
    update_amount(Quest, DungeonId, 1);
do_update_quest(_User, Quest = #quest{event = event_level_upgrade}, #event_level_upgrade{level = Level})  ->
    %% 升级,向下兼容(升到更高级,低级的也算通过)
    update_amount(Quest, Level, 1);

do_update_quest(_User, _Event, _Quest) ->
    error.
