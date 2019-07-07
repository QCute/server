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
load(User = #user{id = Id}) ->
    Data = shop_sql:select(Id),
    List = parser:convert(Data, shop),
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
buy(User = #user{shop = ShopList}, ShopId, Amount) ->
    case check_amount(User, ShopId, Amount) of
        {ok, NewShop, Items, Cost} ->
            NewList = lists:keystore(ShopId, #shop.shop_id, ShopList, NewShop),
            {ok, NewUser} = asset:cost(User, Cost),
            item:add(NewUser#user{shop = NewList}, Items);
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
        DataShop = #data_shop{} ->
            check_level(User, DataShop, Amount);
        _ ->
            {error, 3}
    end.
check_level(User, DataShop = #data_shop{level = Level, vip_level = VipLevel}, Amount) ->
    case role_checker:check(User, [{level, Level, 4}, {vip, VipLevel, 5}]) of
        ok ->
            check_limit(User, DataShop, Amount);
        Error ->
            Error
    end.
check_limit(User = #user{id = Id, shop = ShopList, vip = #vip{level = VipLevel}}, DataShop = #data_shop{shop_id = ShopId}, Amount) ->
    ExtraLimit = listing:key_find(VipLevel, 1, DataShop#data_shop.vip_limit, 0),
    Shop = listing:key_find(ShopId, #shop.shop_id, ShopList, #shop{role_id = Id, shop_id = ShopId}),
    case Shop#shop.amount + Amount =< DataShop#data_shop.limit + ExtraLimit of
        true ->
            check_cost(User, Shop, DataShop, Amount);
        _ ->
            {error, 7}
    end.
check_cost(User, Shop = #shop{amount = OldAmount}, #data_shop{pay_assets = Assets, price = Price, item_id = ItemId, amount = ItemAmount, bind = Bind}, Amount) ->
    Cost = [{Assets, Amount * Price, 8}],
    case role_checker:check(User, Cost) of
        ok ->
            {ok, Shop#shop{amount = OldAmount + Amount, flag = update}, [{ItemId, ItemAmount * Amount, Bind}], Cost};
        Error ->
            Error
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================