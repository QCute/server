%%%-------------------------------------------------------------------
%%% @doc
%%% battle event
%%% @end
%%%-------------------------------------------------------------------
-module(battle_event).
%% API
-export([add_trigger/2, remove_trigger/2]).
-export([trigger/2]).
%% Includes
-include("common.hrl").
-include("event.hrl").
-include("map.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc add event trigger
-spec add_trigger(State :: #map{}, AddTrigger :: #trigger{} | [#trigger{}]) -> #map{}.
add_trigger(State = #map{trigger = Trigger}, AddTrigger) when is_list(AddTrigger) ->
    State#map{trigger = add_loop(AddTrigger, Trigger)};
add_trigger(State = #map{trigger = Trigger}, AddTrigger) ->
    State#map{trigger = add_loop([AddTrigger], Trigger)}.

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
-spec remove_trigger(State :: #map{},  RemoveTrigger :: #trigger{} | term() | [#trigger{}] | [term()]) -> #map{}.
remove_trigger(State = #map{trigger = Trigger}, RemoveTrigger) when is_list(RemoveTrigger) ->
    State#map{trigger = remove_loop(RemoveTrigger, Trigger)};
remove_trigger(State = #map{trigger = Trigger}, RemoveTrigger) ->
    State#map{trigger = remove_loop([RemoveTrigger], Trigger)}.

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
-spec trigger(State :: #map{}, Event :: tuple() | [tuple()]) -> NewState :: #map{}.
trigger(State, Event) when is_list(Event) ->
    %% multi event
    trigger_loop(Event, State);
trigger(State, Event) ->
    %% single event
    trigger_loop([Event], State).

trigger_loop([], State) ->
    State;
trigger_loop([Event | T], State = #map{trigger = TriggerList}) ->
    %% event name as this event key
    case lists:keyfind(element(2, Event), 1, TriggerList) of
        false ->
            trigger_loop(T, State);
        {Name, List} ->
            case apply_loop(List, State, Event, []) of
                {NewState, []} ->
                    NewTriggerList = lists:keydelete(Name, 1, TriggerList),
                    trigger_loop(T, NewState#map{trigger = NewTriggerList});
                {NewState, NewList} ->
                    NewTriggerList = lists:keyreplace(Name, 1, TriggerList, {Name, NewList}),
                    trigger_loop(T, NewState#map{trigger = NewTriggerList})
            end
    end.

%% trigger specific event
apply_loop([], State, _, List) ->
    {State, List};
apply_loop([Trigger = #trigger{module = undefined, pure = false, function = Function, args = Args} | T], State, Event, List) ->
    case erlang:apply(Function, [State, Event | Args]) of
        ok ->
            apply_loop(T, State, Event, [Trigger | List]);
        {ok, NewState = #map{}} ->
            apply_loop(T, NewState, Event, [Trigger | List]);
        remove ->
            apply_loop(T, State, Event, List);
        {remove, NewState = #map{}} ->
            apply_loop(T, NewState, Event, List)
    end;
apply_loop([Trigger = #trigger{module = Module, pure = false, function = Function, args = Args} | T], State, Event, List) ->
    case erlang:apply(Module, Function, [State, Event | Args]) of
        ok ->
            apply_loop(T, State, Event, [Trigger | List]);
        {ok, NewState = #map{}} ->
            apply_loop(T, NewState, Event, [Trigger | List]);
        remove ->
            apply_loop(T, State, Event, List);
        {remove, NewState = #map{}} ->
            apply_loop(T, NewState, Event, List)
    end;
apply_loop([Trigger = #trigger{module = undefined, pure = true, function = Function, args = Args} | T], State, Event, List) ->
    case erlang:apply(Function, Args) of
        ok ->
            apply_loop(T, State, Event, [Trigger | List]);
        remove ->
            apply_loop(T, State, Event, List)
    end;
apply_loop([Trigger = #trigger{module = Module, pure = true, function = Function, args = Args} | T], State, Event, List) ->
    case erlang:apply(Module, Function, Args) of
        ok ->
            apply_loop(T, State, Event, [Trigger | List]);
        remove ->
            apply_loop(T, State, Event, List)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
