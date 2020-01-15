%%%------------------------------------------------------------------
%%% @doc
%%% module battle event trigger
%%% @end
%%%------------------------------------------------------------------
-module(battle_event).
%% API
-export([add/2, remove/2]).
-export([handle/2]).
%% Includes
-include("common.hrl").
-include("map.hrl").
-include("event.hrl").
%%%==================================================================
%%% API functions
%%%==================================================================
%% @doc add event trigger
-spec add(State :: #map_state{}, AddTrigger :: #trigger{} | [#trigger{}]) -> #map_state{}.
add(State = #map_state{trigger = Trigger}, AddTrigger) when is_list(AddTrigger) ->
    State#map_state{trigger = add_loop(AddTrigger, Trigger)};
add(State = #map_state{trigger = Trigger}, AddTrigger) ->
    State#map_state{trigger = add_loop([AddTrigger], Trigger)}.

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
-spec remove(State :: #map_state{},  RemoveTrigger :: #trigger{} | term() | [#trigger{}] | [term()]) -> #map_state{}.
remove(State = #map_state{trigger = Trigger}, RemoveTrigger) when is_list(RemoveTrigger) ->
    State#map_state{trigger = remove_loop(RemoveTrigger, Trigger)};
remove(State = #map_state{trigger = Trigger}, RemoveTrigger) ->
    State#map_state{trigger = remove_loop([RemoveTrigger], Trigger)}.

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

%% @doc handle event
-spec handle(State :: #map_state{}, Event :: tuple() | [tuple()]) -> NewState :: #map_state{}.
handle(State, Event) when is_list(Event) ->
    %% multi event
    handle_loop(Event, State);
handle(State, Event) ->
    %% single event
    handle_loop([Event], State).

handle_loop([], State) ->
    State;
handle_loop([Event | T], State = #map_state{trigger = TriggerList}) ->
    %% event name as this event key
    case lists:keyfind(element(2, Event), 1, TriggerList) of
        false ->
            handle_loop(T, State);
        {Name, List} ->
            case apply_loop(List, State, Event, []) of
                {NewState, []} ->
                    NewTriggerList = lists:keydelete(Name, 1, TriggerList),
                    handle_loop(T, NewState#map_state{trigger = NewTriggerList});
                {NewState, NewList} ->
                    NewTriggerList = lists:keyreplace(Name, 1, TriggerList, {Name, NewList}),
                    handle_loop(T, NewState#map_state{trigger = NewTriggerList})
            end
    end.

%% handle specific event
apply_loop([], State, _, List) ->
    {State, List};
apply_loop([Trigger = #trigger{module = undefined, pure = false, function = Function, args = Args} | T], State, Event, List) ->
    case catch erlang:apply(Function, [State, Event | Args]) of
        ok ->
            apply_loop(T, State, Event, [Trigger | List]);
        {ok, NewState = #map_state{}} ->
            apply_loop(T, NewState, Event, [Trigger | List]);
        remove ->
            apply_loop(T, State, Event, List);
        {remove, NewState = #map_state{}} ->
            apply_loop(T, NewState, Event, List);
        What ->
            ?PRINT("event trigger :~w unknown return: ~w", [Trigger, What])
    end;
apply_loop([Trigger = #trigger{module = Module, pure = false, function = Function, args = Args} | T], State, Event, List) ->
    case catch erlang:apply(Module, Function, [State, Event | Args]) of
        ok ->
            apply_loop(T, State, Event, [Trigger | List]);
        {ok, NewState = #map_state{}} ->
            apply_loop(T, NewState, Event, [Trigger | List]);
        remove ->
            apply_loop(T, State, Event, List);
        {remove, NewState = #map_state{}} ->
            apply_loop(T, NewState, Event, List);
        What ->
            ?PRINT("event trigger :~w unknown return: ~w", [Trigger, What])
    end;
apply_loop([Trigger = #trigger{module = undefined, pure = true, function = Function, args = Args} | T], State, Event, List) ->
    case catch erlang:apply(Function, Args) of
        ok ->
            apply_loop(T, State, Event, [Trigger | List]);
        remove ->
            apply_loop(T, State, Event, List);
        What ->
            ?PRINT("event trigger :~w unknown return: ~w", [Trigger, What])
    end;
apply_loop([Trigger = #trigger{module = Module, pure = true, function = Function, args = Args} | T], State, Event, List) ->
    case catch erlang:apply(Module, Function, Args) of
        ok ->
            apply_loop(T, State, Event, [Trigger | List]);
        remove ->
            apply_loop(T, State, Event, List);
        What ->
            ?PRINT("event trigger :~w unknown return: ~w", [Trigger, What])
    end.

%%%==================================================================
%%% Internal functions
%%%==================================================================
