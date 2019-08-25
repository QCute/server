%%%-------------------------------------------------------------------
%%% @doc
%%% module shop
%%% @end
%%%-------------------------------------------------------------------
-module(shop).
%% API
-export([load/1, save/1, clean/1]).
-export([buy/3]).
%% Includes
-include("common.hrl").
-include("user.hrl").
-include("role.hrl").
-include("shop.hrl").
-include("vip.hrl").
-include("protocol.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%% @doc load user shop
-spec load(User :: #user{}) -> NewUser :: #user{}.
load(User = #user{role_id = RoleId}) ->
    List = parser:convert(shop_sql:select(RoleId), ?MODULE),
    User#user{shop = List}.

%% @doc save user shop
-spec save(User :: #user{}) -> NewUser :: #user{}.
save(User = #user{shop = Shop}) ->
    NewShop = shop_sql:update_into(Shop),
    User#user{shop = NewShop}.

%% @doc save user shop
-spec clean(User :: #user{}) -> NewUser :: #user{}.
clean(User) ->
    User.

%% @doc buy
-spec buy(User :: #user{}, ShopId :: non_neg_integer(), Amount :: non_neg_integer()) -> {ok, #user{}} | {error, non_neg_integer()}.
buy(User = #user{role_id = RoleId, shop = ShopList}, ShopId, Amount) ->
    case check_amount(User, ShopId, Amount) of
        {ok, NewShop, Items, Cost} ->
            NewList = lists:keystore(ShopId, #shop.shop_id, ShopList, NewShop),
            {ok, NewUser} = asset:cost(User, Cost),
            %% log
            log:shop_log(RoleId, ShopId, Amount, time:ts()),
            %% add item
            item:add(NewUser#user{shop = NewList}, Items, ?MODULE);
        Error ->
            Error
    end.

check_amount(User, ShopId, Amount) ->
    case 0 < Amount of
        true ->
            check_id(User, ShopId, Amount);
        _ ->
            {error, 2}
    end.
check_id(User, ShopId, Amount) ->
    case shop_data:get(ShopId) of
        ShopData = #shop_data{} ->
            check_level(User, ShopData, Amount);
        _ ->
            {error, 3}
    end.
check_level(User, ShopData = #shop_data{level = Level, vip_level = VipLevel}, Amount) ->
    case user_checker:check(User, [{level, Level, 4}, {vip, VipLevel, 5}]) of
        ok ->
            check_limit(User, ShopData, Amount);
        Error ->
            Error
    end.
check_limit(User = #user{role_id = RoleId, shop = ShopList, vip = #vip{level = VipLevel}}, ShopData = #shop_data{shop_id = ShopId}, Amount) ->
    ExtraLimit = listing:key_find(VipLevel, 1, ShopData#shop_data.vip_limit, 0),
    Shop = listing:key_find(ShopId, #shop.shop_id, ShopList, #shop{role_id = RoleId, shop_id = ShopId}),
    case Shop#shop.amount + Amount =< ShopData#shop_data.limit + ExtraLimit of
        true ->
            check_cost(User, Shop, ShopData, Amount);
        _ ->
            {error, 7}
    end.
check_cost(User, Shop = #shop{amount = OldAmount}, #shop_data{pay_assets = Assets, price = Price, item_id = ItemId, amount = ItemAmount, bind = Bind}, Amount) ->
    Cost = [{Assets, Amount * Price, 8}],
    case user_checker:check(User, Cost) of
        ok ->
            {ok, Shop#shop{amount = OldAmount + Amount, flag = update}, [{ItemId, ItemAmount * Amount, Bind}], Cost};
        Error ->
            Error
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================