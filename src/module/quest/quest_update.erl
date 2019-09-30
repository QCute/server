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

%% 具体更新任务放在下面
%% update quest detail
do_update_quest(_User, Quest = #quest{event = event_kill_monster}, #event_kill_monster{monster_id = MonsterId, number = Number}) ->
    %% 杀怪
    update_number(Quest, MonsterId, Number);
do_update_quest(_User, Quest = #quest{event = event_guild_join}, #event_guild_join{}) ->
    %% 加入公会
    update_number(Quest, 0, 1);
do_update_quest(_User, Quest = #quest{event = event_pass_dungeon}, #event_pass_dungeon{dungeon_id = DungeonId})  ->
    %% 通关副本,向下兼容(通关后面的,前面的也算通关)
    update_number(Quest, DungeonId, 1);
do_update_quest(_User, Quest = #quest{event = event_level_upgrade}, #event_level_upgrade{level = Level})  ->
    %% 升级,向下兼容(升到更高级,低级的也算通过)
    update_number(Quest, Level, 1);

do_update_quest(_User, _Event, _Quest) ->
    error.
