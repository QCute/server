%%%-------------------------------------------------------------------
%%% @doc
%%% module quest update
%%% @end
%%%-------------------------------------------------------------------
-module(quest_update).
%% API
-export([update/2, update_quest/3, update_progress/3]).
%% Includes
-include("common.hrl").
-include("user.hrl").
-include("player.hrl").
-include("quest.hrl").
-include("event.hrl").
%%%===================================================================
%%% API
%%%===================================================================
%% @doc update all quest
-spec update(User :: #user{}, Event :: tuple()) -> NewUser :: #user{}.
update(User = #user{quest = Quest}, Event) ->
    {NewQuest, UpdateQuest} = update_quest(User, Event, Quest),
    {ok, Binary} = player_route:write(45678, UpdateQuest),
    player_sender:send(User, Binary),
    User#user{quest = NewQuest}.

%% @doc update specified quest
-spec update_quest(User :: #user{}, Event :: tuple(), Quest :: [#quest{}]) -> {NewQuest :: [#quest{}], UpdateQuest :: [#quest{}]}.
update_quest(User, Event, Quest) ->
    update_quest(User, Event, Quest, [], []).
%% update per quest
update_quest(_, _, [], List, Update) ->
    {List, Update};
update_quest(User, Event, [Quest = #quest{progress = Progress} | T], List, Update) ->
    case update_progress(User, Event, Progress) of
        {true, NewProgress} ->
            update_quest(User, Event, T, [Quest#quest{progress = NewProgress, extra = update} | List], [NewProgress | Update]);
        _ ->
            update_quest(User, Event, T, [Quest | List], Update)
    end.

%% @doc update specified quest progress
-spec update_progress(User :: #user{}, Event :: tuple(), Progress :: [#quest_progress{}]) -> {UpdateFlag :: boolean(), NewProgress :: [#quest_progress{}]}.
update_progress(User, Event, Progress) ->
    update_progress(User, Event, Progress, [], false).
%% update per progress
update_progress(_, _, [], List, UpdateFlag) ->
    {UpdateFlag, List};

update_progress(User, Event = #event_kill_monster{amount = Amount}, [Progress = #quest_progress{type = event_kill_monster, value = Value} | T], List, _) when Amount < Value ->
    update_progress(User, Event, T, [Progress#quest_progress{value = Value - Amount} | List], true);

update_progress(User, Event = #event_kill_monster{amount = Amount}, [#quest_progress{type = event_kill_monster, value = Value} | T], List, _) when Value =< Amount ->
    update_progress(User, Event, T, List, true);

update_progress(User, Event, [H | T], List, UpdateFlag) ->
    update_progress(User, Event, T, [H | List], UpdateFlag).

