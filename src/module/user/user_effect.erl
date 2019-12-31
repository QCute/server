%%%------------------------------------------------------------------
%%% @doc
%%% module user effect
%%% @end
%%%------------------------------------------------------------------
-module(user_effect).
%% API
-export([add/3, remove/3, act/6]).
%% Includes
-include("user.hrl").
-include("effect.hrl").
%%%==================================================================
%%% API functions
%%%==================================================================
%% @doc add effect
-spec add(User :: #user{}, Number :: non_neg_integer(), EffectList :: list()) -> #user{}.
add(User = #user{effect = Effect}, Number, EffectIdList) ->
    User#user{effect = add_loop(EffectIdList, Number, Effect)}.

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
-spec remove(User :: #user{}, Number :: non_neg_integer(), EffectList :: list()) -> #user{}.
remove(User = #user{effect = Effect}, Number, EffectIdList) ->
    User#user{effect = remove_loop(EffectIdList, Number, Effect)}.

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

%% @doc execute effect
-spec act(User :: #user{}, Operation :: term(), Attribute :: term(), Field :: term(), Value :: non_neg_integer(), From :: term()) -> {#user{}, non_neg_integer()}.
act(User = #user{effect = Effect}, Operation, Attribute, Field, Value, _) ->
    case lists:keyfind({Operation, Attribute, Field}, 1, Effect) of
        {_, List} ->
            effect:calculate(User, List, Value);
        false ->
            {User, Value}
    end.

%%%==================================================================
%%% Internal functions
%%%==================================================================
