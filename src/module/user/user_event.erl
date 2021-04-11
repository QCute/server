%%%-------------------------------------------------------------------
%%% @doc
%%% user event
%%% @end
%%%-------------------------------------------------------------------
-module(user_event).
%% API
-export([add_trigger/2, remove_trigger/2]).
-export([trigger/2, trigger_static/2]).
%% Includes
-include("common.hrl").
-include("event.hrl").
-include("user.hrl").
-include("rank.hrl").
-include("notice.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc add event trigger
-spec add_trigger(User :: #user{}, AddTrigger :: #trigger{} | [#trigger{}]) -> #user{}.
add_trigger(User = #user{trigger = Trigger}, AddTrigger) when is_list(AddTrigger) ->
    User#user{trigger = add_loop(AddTrigger, Trigger)};
add_trigger(User = #user{trigger = Trigger}, AddTrigger) ->
    User#user{trigger = add_loop([AddTrigger], Trigger)}.

add_loop([], TriggerList) ->
    TriggerList;
add_loop([Trigger = #trigger{name = Name} | T], TriggerList) ->
    case lists:keyfind(Name, 1, TriggerList) of
        false ->
            add_loop(T, [{Name, [Trigger]} | TriggerList]);
        {_, List} ->
            NewTriggerList = lists:keyreplace(Name, 1, TriggerList, {Name, [Trigger | List]}),
            add_loop(T, NewTriggerList)
    end.

%% @doc remove event trigger
-spec remove_trigger(User :: #user{},  RemoveTrigger :: #trigger{} | term() | [#trigger{}] | [term()]) -> #user{}.
remove_trigger(User = #user{trigger = Trigger}, RemoveTrigger) when is_list(RemoveTrigger) ->
    User#user{trigger = remove_loop(RemoveTrigger, Trigger)};
remove_trigger(User = #user{trigger = Trigger}, RemoveTrigger) ->
    User#user{trigger = remove_loop([RemoveTrigger], Trigger)}.

remove_loop([], TriggerList) ->
    TriggerList;
remove_loop([#trigger{name = Name} | T], TriggerList) ->
    remove_loop([Name | T], TriggerList);
remove_loop([Name | T], TriggerList) ->
    case lists:keyfind(Name, 1, TriggerList) of
        false ->
            remove_loop(T, TriggerList);
        {_, List} ->
            case lists:keydelete(Name, 1, List) of
                [] ->
                    NewTriggerList = lists:keydelete(Name, 1, TriggerList),
                    remove_loop(T, NewTriggerList);
                NewList ->
                    %% store it
                    NewTriggerList = lists:keyreplace(Name, 1, TriggerList, {Name, NewList}),
                    remove_loop(T, NewTriggerList)
            end
    end.

%% @doc trigger event
-spec trigger(User :: #user{}, Event :: tuple() | [tuple()]) -> NewUser :: #user{}.
trigger(User = #user{}, Event) when is_list(Event) ->
    %% multi event
    trigger_loop(Event, User);
trigger(User = #user{}, Event) ->
    %% single event
    trigger_loop([Event], User);
trigger(RoleId, Event) when is_integer(RoleId) ->
    user_server:apply_cast(RoleId, ?MODULE, ?FUNCTION_NAME, [Event]);
trigger(RolePid, Event) when is_pid(RolePid) ->
    user_server:apply_cast(RolePid, ?MODULE, ?FUNCTION_NAME, [Event]).

trigger_loop([], User) ->
    User;
trigger_loop([Event | T], User = #user{trigger = TriggerList}) ->
    %% local and dynamic static event
    NewUser = trigger_static(User, Event),
    %% event name as this event key
    case lists:keyfind(element(2, Event), 1, TriggerList) of
        false ->
            trigger_loop(T, NewUser);
        {Name, List} ->
            case apply_loop(List, NewUser, Event, []) of
                {NewestUser, []} ->
                    NewTriggerList = lists:keydelete(Name, 1, TriggerList),
                    trigger_loop(T, NewestUser#user{trigger = NewTriggerList});
                {NewestUser, NewList} ->
                    NewTriggerList = lists:keyreplace(Name, 1, TriggerList, {Name, NewList}),
                    trigger_loop(T, NewestUser#user{trigger = NewTriggerList})
            end
    end.

%% trigger specific event
apply_loop([], User, _, List) ->
    {User, List};
apply_loop([Trigger = #trigger{module = undefined, pure = false, function = Function, args = Args} | T], User, Event, List) ->
    case erlang:apply(Function, [User, Event | Args]) of
        ok ->
            apply_loop(T, User, Event, [Trigger | List]);
        {ok, NewUser = #user{}} ->
            apply_loop(T, NewUser, Event, [Trigger | List]);
        remove ->
            apply_loop(T, User, Event, List);
        {remove, NewUser = #user{}} ->
            apply_loop(T, NewUser, Event, List)
    end;
apply_loop([Trigger = #trigger{module = Module, pure = false, function = Function, args = Args} | T], User, Event, List) ->
    case erlang:apply(Module, Function, [User, Event | Args]) of
        ok ->
            apply_loop(T, User, Event, [Trigger | List]);
        {ok, NewUser = #user{}} ->
            apply_loop(T, NewUser, Event, [Trigger | List]);
        remove ->
            apply_loop(T, User, Event, List);
        {remove, NewUser = #user{}} ->
            apply_loop(T, NewUser, Event, List)
    end;
apply_loop([Trigger = #trigger{module = undefined, pure = true, function = Function, args = Args} | T], User, Event, List) ->
    case erlang:apply(Function, [Event | Args]) of
        ok ->
            apply_loop(T, User, Event, [Trigger | List]);
        remove ->
            apply_loop(T, User, Event, List)
    end;
apply_loop([Trigger = #trigger{module = Module, pure = true, function = Function, args = Args} | T], User, Event, List) ->
    case erlang:apply(Module, Function, [Event | Args]) of
        ok ->
            apply_loop(T, User, Event, [Trigger | List]);
        remove ->
            apply_loop(T, User, Event, List)
    end.

%%%===================================================================
%%% trigger static event
%%%===================================================================
%% @doc trigger static event
-spec trigger_static(User :: #user{}, Event :: #event{}) -> NewUser :: #user{}.
%% trigger static event @here
%% auto generate, do not edit this code

trigger_static(User, Event = #event{name = event_recharge}) ->
    CountUser = count:handle_event_recharge(User, Event),
    vip:handle_event_recharge(CountUser, Event);
trigger_static(User, _Event = #event{name = event_exp_add}) ->
    role:handle_event_exp_add(User);
trigger_static(User, Event = #event{name = event_shop_buy}) ->
    count:handle_event_shop_buy(User, Event);
trigger_static(User, Event = #event{name = event_gold_cost}) ->
    count:handle_event_gold_cost(User, Event);
trigger_static(User, _) ->
    User.

%%%===================================================================
%%% Internal functions
%%%===================================================================
