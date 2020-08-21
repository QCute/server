%%%-------------------------------------------------------------------
%%% @doc
%%% module shop
%%% @end
%%%-------------------------------------------------------------------
-module(shop).
%% API
-export([load/1, save/1, reset/1]).
-export([query/1]).
-export([get_number/1, get_number/2]).
-export([buy/3]).
%% Includes
-include("common.hrl").
-include("protocol.hrl").
-include("user.hrl").
-include("event.hrl").
-include("vip.hrl").
-include("shop.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc load
-spec load(User :: #user{}) -> NewUser :: #user{}.
load(User = #user{role_id = RoleId}) ->
    List = shop_sql:select(RoleId),
    User#user{shop = List}.

%% @doc save
-spec save(User :: #user{}) -> NewUser :: #user{}.
save(User = #user{shop = Shop}) ->
    NewShop = shop_sql:insert_update(Shop),
    User#user{shop = NewShop}.

%% @doc clean
-spec reset(User :: #user{}) -> NewUser :: #user{}.
reset(User) ->
    User.

%% @doc query
-spec query(User :: #user{}) -> ok().
query(#user{shop = Shop}) ->
    {ok, Shop}.

%% @doc get number
-spec get_number(User :: #user{}) -> non_neg_integer().
get_number(#user{shop = Shop}) ->
    length(Shop).

%% @doc get number
-spec get_number(User :: #user{}, ShopId :: non_neg_integer()) -> non_neg_integer().
get_number(#user{shop = Shop}, ShopId) ->
    #shop{number = Number} = listing:key_find(ShopId, #shop.shop_id, Shop, #shop{}),
    Number.

%% @doc buy
-spec buy(User :: #user{}, ShopId :: non_neg_integer(), Number :: non_neg_integer()) -> ok() | error().
buy(User, ShopId, Number) ->
    case shop_data:get(ShopId) of
        ShopData = #shop_data{} ->
            check_level(User, ShopData, Number);
        _ ->
            {error, configure_not_found}
    end.

check_level(User, ShopData = #shop_data{level = Level, vip_level = VipLevel}, Number) ->
    case user_checker:check(User, [{level, Level, level_not_enough}, {vip, VipLevel, vip_level_not_enough}]) of
        ok ->
            check_limit(User, ShopData, Number);
        Error ->
            Error
    end.

check_limit(User = #user{role_id = RoleId, shop = ShopList, vip = #vip{vip_level = VipLevel}}, ShopData = #shop_data{shop_id = ShopId}, Number) ->
    {_, ExtraLimit} = listing:key_find(VipLevel, 1, ShopData#shop_data.vip_limit, {0, 0}),
    Shop = listing:key_find(ShopId, #shop.shop_id, ShopList, #shop{role_id = RoleId, shop_id = ShopId}),
    case Shop#shop.number + Number =< ShopData#shop_data.limit + ExtraLimit of
        true when 0 < Number ->
            buy_cost(User, Shop, ShopData, Number);
        true ->
            {error, number_invalid};
        false ->
            {error, buy_max}
    end.

buy_cost(User, Shop, ShopData = #shop_data{pay_asset = Asset, price = Price}, Number) ->
    case asset:cost(User, [{Asset, Price * Number}], ?MODULE) of
        {ok, CostUser} ->
            buy_final(CostUser, Shop, ShopData, Number);
        Error ->
            Error
    end.

buy_final(User = #user{role_id = RoleId, shop = ShopList}, Shop = #shop{shop_id = ShopId, number = OldNumber}, #shop_data{item_id = ItemId, number = ItemNumber}, Number) ->
    %% update shop
    NewList = lists:keystore(ShopId, #shop.shop_id, ShopList, Shop#shop{number = OldNumber + Number, flag = 1}),
    %% add item
    {ok, NewestUser} = item:add(User, [{ItemId, ItemNumber * Number}], ?MODULE),
    %% log
    log:shop_log(RoleId, ShopId, Number, time:now()),
    %% handle buy event
    FinalUser = user_event:handle(NewestUser, #event{name = event_shop_buy, target = ShopId, number = Number}),
    {ok, ok, FinalUser#user{shop = NewList}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================