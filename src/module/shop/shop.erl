%%%------------------------------------------------------------------
%%% @doc
%%% module shop
%%% @end
%%%------------------------------------------------------------------
-module(shop).
%% API
-export([load/1, save/1, clean/1]).
-export([query/1]).
-export([buy/3]).
%% Includes
-include("common.hrl").
-include("user.hrl").
-include("shop.hrl").
-include("vip.hrl").
-include("protocol.hrl").

%%%==================================================================
%%% API functions
%%%==================================================================
%% @doc load
-spec load(User :: #user{}) -> NewUser :: #user{}.
load(User = #user{role_id = RoleId}) ->
    List = parser:convert(shop_sql:select(RoleId), ?MODULE),
    User#user{shop = List}.

%% @doc save
-spec save(User :: #user{}) -> NewUser :: #user{}.
save(User = #user{shop = Shop}) ->
    NewShop = shop_sql:insert_update(Shop),
    User#user{shop = NewShop}.

%% @doc clean
-spec clean(User :: #user{}) -> NewUser :: #user{}.
clean(User) ->
    User.

%% @doc query
-spec query(User :: #user{}) -> ok().
query(#user{shop = Shop}) ->
    {ok, Shop}.

%% @doc buy
-spec buy(User :: #user{}, ShopId :: non_neg_integer(), Number :: non_neg_integer()) -> ok() | error().
buy(User = #user{role_id = RoleId, shop = ShopList}, ShopId, Number) ->
    case check_number(User, ShopId, Number) of
        {ok, NewShop, Items, Cost} ->
            NewList = lists:keystore(ShopId, #shop.shop_id, ShopList, NewShop),
            {ok, NewUser} = asset:cost(User, Cost),
            %% log
            log:shop_log(RoleId, ShopId, Number, time:ts()),
            %% add item
            {ok, NewUser} = item:add(NewUser#user{shop = NewList}, Items, ?MODULE),
            {ok, 1, NewUser};
        Error ->
            Error
    end.

check_number(User, ShopId, Number) ->
    case 0 < Number of
        true ->
            check_id(User, ShopId, Number);
        _ ->
            {error, 2}
    end.
check_id(User, ShopId, Number) ->
    case shop_data:get(ShopId) of
        ShopData = #shop_data{} ->
            check_level(User, ShopData, Number);
        _ ->
            {error, 3}
    end.
check_level(User, ShopData = #shop_data{level = Level, vip_level = VipLevel}, Number) ->
    case user_checker:check(User, [{level, Level, 4}, {vip, VipLevel, 5}]) of
        {ok, _} ->
            check_limit(User, ShopData, Number);
        Error ->
            Error
    end.
check_limit(User = #user{role_id = RoleId, shop = ShopList, vip = #vip{vip_level = VipLevel}}, ShopData = #shop_data{shop_id = ShopId}, Number) ->
    ExtraLimit = listing:key_find(VipLevel, 1, ShopData#shop_data.vip_limit, 0),
    Shop = listing:key_find(ShopId, #shop.shop_id, ShopList, #shop{role_id = RoleId, shop_id = ShopId}),
    case Shop#shop.number + Number =< ShopData#shop_data.limit + ExtraLimit of
        true ->
            check_cost(User, Shop, ShopData, Number);
        _ ->
            {error, 7}
    end.
check_cost(User, Shop = #shop{number = OldNumber}, #shop_data{pay_assets = Assets, price = Price, item_id = ItemId, number = ItemNumber, bind = Bind}, Number) ->
    Cost = [{Assets, Number * Price, 8}],
    case user_checker:check(User, Cost) of
        {ok, _} ->
            {ok, Shop#shop{number = OldNumber + Number, flag = update}, [{ItemId, ItemNumber * Number, Bind}], Cost};
        Error ->
            Error
    end.

%%%==================================================================
%%% Internal functions
%%%==================================================================