%%%------------------------------------------------------------------
%%% @doc
%%% module role event trigger
%%% @end
%%%------------------------------------------------------------------
-module(user_event).
%% API
-export([add/2, remove/2]).
-export([handle/2]).
%% Includes
-include("common.hrl").
-include("user.hrl").
-include("event.hrl").
%%%==================================================================
%%% API functions
%%%==================================================================
%% @doc add event trigger
-spec add(User :: #user{}, AddTrigger :: #trigger{} | [#trigger{}]) -> #user{}.
add(User = #user{trigger = Trigger}, AddTrigger) when is_list(AddTrigger) ->
    User#user{trigger = add_loop(AddTrigger, Trigger)};
add(User = #user{trigger = Trigger}, AddTrigger) ->
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
-spec remove(User :: #user{},  RemoveTrigger :: #trigger{} | term() | [#trigger{}] | [term()]) -> #user{}.
remove(User = #user{trigger = Trigger}, RemoveTrigger) when is_list(RemoveTrigger) ->
    User#user{trigger = remove_loop(RemoveTrigger, Trigger)};
remove(User = #user{trigger = Trigger}, RemoveTrigger) ->
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

%% @doc handle event
-spec handle(User :: #user{}, Event :: tuple() | [tuple()]) -> NewUser :: #user{}.
handle(User, Event) when is_list(Event) ->
    %% multi event
    handle_loop(User, Event);
handle(User, Event) ->
    %% single event
    handle_loop(User, [Event]).

handle_loop([], User) ->
    User;
handle_loop([Event | T], User = #user{trigger = TriggerList}) ->
    case lists:keyfind(element(1, Event), 1, TriggerList) of
        false ->
            handle_loop(T, User);
        {Name, List} ->
            {NewUser, NewList} = apply_loop(List, User, []),
            NewTriggerList = lists:keyreplace(Name, 1, TriggerList, {Name, NewList}),
            handle_loop(T, NewUser#user{trigger = NewTriggerList})
    end.

%% handle specific event
apply_loop([], User, List) ->
    {User, List};
apply_loop([Trigger = #trigger{module = undefined, pure = false, function = Function, args = Args} | T], User, List) ->
    case catch erlang:apply(Function, [User, Trigger | Args]) of
        ok ->
            apply_loop(T, User, [Trigger | List]);
        {ok, NewUser = #user{}} ->
            apply_loop(T, NewUser, [Trigger | List]);
        remove ->
            apply_loop(T, User, List);
        {remove, NewUser = #user{}} ->
            apply_loop(T, NewUser, List);
        NewUser = #user{} ->
            apply_loop(T, NewUser, List);
        What ->
            ?PRINT("event trigger :~w unknown return: ~w", [Trigger, What])
    end;
apply_loop([Trigger = #trigger{module = Module, pure = false, function = Function, args = Args} | T], User, List) ->
    case catch erlang:apply(Module, Function, [User, Trigger | Args]) of
        ok ->
            apply_loop(T, User, [Trigger | List]);
        {ok, NewUser = #user{}} ->
            apply_loop(T, NewUser, [Trigger | List]);
        remove ->
            apply_loop(T, User, List);
        {remove, NewUser = #user{}} ->
            apply_loop(T, NewUser, List);
        NewUser = #user{} ->
            apply_loop(T, NewUser, List);
        What ->
            ?PRINT("event trigger :~w unknown return: ~w", [Trigger, What])
    end;
apply_loop([Trigger = #trigger{module = undefined, pure = true, function = Function, args = Args} | T], User, List) ->
    case catch erlang:apply(Function, [Trigger | Args]) of
        ok ->
            apply_loop(T, User, [Trigger | List]);
        remove ->
            apply_loop(T, User, List);
        What ->
            ?PRINT("event trigger :~w unknown return: ~w", [Trigger, What])
    end;
apply_loop([Trigger = #trigger{module = Module, pure = true, function = Function, args = Args} | T], User, List) ->
    case catch erlang:apply(Module, Function, [Trigger | Args]) of
        ok ->
            apply_loop(T, User, [Trigger | List]);
        remove ->
            apply_loop(T, User, List);
        What ->
            ?PRINT("event trigger :~w unknown return: ~w", [Trigger, What])
    end.

%%%==================================================================
%%% Internal functions
%%%==================================================================
