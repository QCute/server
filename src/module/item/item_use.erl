%%%-------------------------------------------------------------------
%%% @doc
%%% module item use
%%% @end
%%%-------------------------------------------------------------------
-module(item_use).
%% API
-export([use/4]).
%% Includes
-include("common.hrl").
-include("user.hrl").
-include("item.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc use
-spec use(User :: #user{}, ItemNo :: non_neg_integer(), Number :: non_neg_integer(), Type :: non_neg_integer()) -> ok() | error().
use(User, ItemNo, Number, Type) ->
    case item:validate(User, [{ItemNo, Number, Type}], ?MODULE) of
        {ok, _} ->
            Item = item:find(User, ItemNo, Type),
            check_use_number(User, Item, Number);
        {error, _} ->
            {error, invalid_item}
    end.

check_use_number(User, Item = #item{item_no = ItemNo, item_id = ItemId, type = Type}, Number) ->
    case item_data:get(ItemId) of
        ItemData = #item_data{use_number = UseNumber} when Number =< UseNumber ->
            {ok, NewUser} = item:reduce(User, [{ItemNo, Number, Type}], ?MODULE),
            execute_effect(NewUser, Item, ItemData, Number);
        _ ->
            {error, configure_not_found}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @doc execute effect after reduce item, ignore unknown use effect
%% @doc add item use effect @here

execute_effect(User, _Item, _ItemData = #item_data{use_effect = exp, use_value = UseValue}, Number) ->
    asset:add_and_push(User, [{exp, UseValue * Number}], item);

execute_effect(User, _Item, _ItemData = #item_data{use_effect = copper, use_value = UseValue}, Number) ->
    asset:add_and_push(User, [{copper, UseValue * Number}], item);

execute_effect(_User, _Item, _ItemData, _Number) ->
    {error, item_cannot_use_directly}.

