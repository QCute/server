%%%-------------------------------------------------------------------
%%% @doc
%%% user effect
%%% @end
%%%-------------------------------------------------------------------
-module(user_effect).
%% API
-export([add/3, remove/3, calculate/6]).
%% Includes
-include("user.hrl").
-include("effect.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc add effect
-spec add(User :: #user{}, Number :: non_neg_integer(), AddEffect :: non_neg_integer() | [non_neg_integer()]) -> #user{}.
add(User = #user{effect = Effect}, Number, AddEffect) when is_list(AddEffect) ->
    User#user{effect = add_loop(AddEffect, Number, Effect)};
add(User = #user{effect = Effect}, Number, AddEffect) ->
    User#user{effect = add_loop([AddEffect], Number, Effect)}.

add_loop([], _, Effect) ->
    Effect;
add_loop([Id | T], Number, Effect) ->
    case effect_data:get(Id) of
        #effect_data{scope = user, operation = Operation, attribute = Attribute, field = Field} ->
            Key = {Operation, Attribute, Field},
            {_, List} = listing:key_find(Key, 1, Effect, {Key, []}),
            NewList = listing:update_count(Id, List, Number),
            NewEffect = lists:keystore(Key, 1, Effect, {Key, NewList}),
            add_loop(T, Number, NewEffect);
        _ ->
            add_loop(T, Number, Effect)
    end.

%% @doc remove effect
-spec remove(User :: #user{}, Number :: non_neg_integer(), RemoveEffect :: non_neg_integer() | [non_neg_integer()]) -> #user{}.
remove(User = #user{effect = Effect}, Number, RemoveEffect) when is_list(RemoveEffect) ->
    User#user{effect = remove_loop(RemoveEffect, Number, Effect)};
remove(User = #user{effect = Effect}, Number, RemoveEffect) ->
    User#user{effect = remove_loop([RemoveEffect], Number, Effect)}.

remove_loop([], _, Effect) ->
    Effect;
remove_loop([Id | T], Number, Effect) ->
    case effect_data:get(Id) of
        #effect_data{scope = user, operation = Operation, attribute = Attribute, field = Field} ->
            Key = {Operation, Attribute, Field},
            {_, List} = listing:key_find(Key, 1, Effect, {Key, []}),
            NewList = listing:update_count(Id, List, -Number),
            NewEffect = lists:keyreplace(Key, 1, Effect, {Key, NewList}),
            remove_loop(T, Number, NewEffect);
        _ ->
            remove_loop(T, Number, Effect)
    end.

%% @doc calculate effect
-spec calculate(User :: #user{}, Operation :: term(), Attribute :: term(), Field :: term(), Value :: non_neg_integer(), From :: term()) -> {NewUser :: #user{}, NewValue :: non_neg_integer()}.
calculate(User = #user{effect = Effect}, Operation, Attribute, Field, Value, From) ->
    case lists:keyfind({Operation, Attribute, Field}, 1, Effect) of
        {_, List} ->
            calculate_loop(User, List, Value, 0, From);
        false ->
            {User, Value}
    end.

%% calculate all effect addition
calculate_loop(User, [], _, Total, _) ->
    {User, Total};
calculate_loop(User, [{Id, Number} | T], Value, Total, From) ->
    {NewUser, NewValue} = execute_script(User, Id, Number * Value, From),
    calculate_loop(NewUser, T, Value, Total + NewValue, From).
%%%===================================================================
%%% Internal functions
%%%===================================================================
%% effect implement
execute_script(User, 9, Value, _) ->
    {User, trunc(Value * 1.5)};
execute_script(User, 10, Value, _) ->
    {User, (Value * 2)};
execute_script(User, _, Value, _) ->
    {User, Value}.
