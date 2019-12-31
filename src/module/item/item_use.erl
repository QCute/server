%%%------------------------------------------------------------------
%%% @doc
%%% module item use
%%% @end
%%%------------------------------------------------------------------
-module(item_use).
%% API
-export([use/5]).
%% Includes
-include("common.hrl").
-include("user.hrl").
-include("item.hrl").
%%%==================================================================
%%% API functions
%%%==================================================================
%% @doc use
-spec use(User :: #user{}, UniqueId :: non_neg_integer(), Number :: non_neg_integer(), Type :: non_neg_integer(), Args :: term()) -> ok() | error().
use(User, UniqueId, Number, Type, Args) ->
    case item:validate(User, [{UniqueId, Number, Type}]) of
        {ok, _} ->
            Item = item:find(User, UniqueId, Type),
            check_use_number(User, Item, Number, Args);
        {error, _} ->
            {error, 2}
    end.

check_use_number(User, Item = #item{unique_id = UniqueId, item_id = ItemId, type = Type}, Number, Args) ->
    case item_data:get(ItemId) of
        ItemData = #item_data{use_number = UseNumber} when Number =< UseNumber ->
            {ok, NewUser} = item:reduce(User, [{UniqueId, Number, Type}], use),
            execute_effect(NewUser, Item, ItemData, Number, Args);
        _ ->
            {error, 3}
    end.


%% @doc execute effect after reduce item, ignore unknown use effect
%% @doc add item use effect here

execute_effect(User, _Item, _ItemData = #item_data{use_effect = exp, use_value = UseValue}, Number, _Args) ->
    asset:add(User, [{exp, UseValue * Number}], item);

execute_effect(User, _Item, _ItemData = #item_data{use_effect = copper, use_value = UseValue}, Number, _Args) ->
    asset:add(User, [{copper, UseValue * Number}], item);

execute_effect(User, _Item, _ItemData, _Number, _Args) ->
    {ok, User, 4}.

%%%==================================================================
%%% Internal functions
%%%==================================================================
