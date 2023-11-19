%%%-------------------------------------------------------------------
%%% @doc
%%% item
%%% @end
%%%-------------------------------------------------------------------
-module(item).
%% API
-export([on_load/1, on_save/1, on_expire/1]).
-export([query_item/1, query_body/1, query_bag/1, query_store/1]).
-export([find/2, find/3, save/2]).
-export([empty_grid/2]).
-export([classify/1, overlap/1]).
-export([add/3]).
-export([validate/3, reduce/3]).
-export([check/3, cost/3]).
%% Includes
-include("common.hrl").
-include("protocol.hrl").
-include("user.hrl").
-include("package.hrl").
-include("item.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc on load
-spec on_load(User :: #user{}) -> NewUser :: #user{}.
on_load(User = #user{role_id = RoleId}) ->
    DataList = item_sql:select(RoleId),
    %% split diff type
    {?ITEM_TYPE_COMMON, Item} = listing:key_find(?ITEM_TYPE_COMMON, 1, classify(DataList), {?ITEM_TYPE_COMMON, []}),
    {?ITEM_TYPE_BAG, Bag} = listing:key_find(?ITEM_TYPE_BAG, 1, classify(DataList), {?ITEM_TYPE_BAG, []}),
    {?ITEM_TYPE_BODY, Body} = listing:key_find(?ITEM_TYPE_BODY, 1, classify(DataList), {?ITEM_TYPE_BODY, []}),
    {?ITEM_TYPE_STORE, Store} = listing:key_find(?ITEM_TYPE_STORE, 1, classify(DataList), {?ITEM_TYPE_STORE, []}),
    User#user{item = Item, bag = Bag, body = Body, store = Store}.

%% @doc on save
-spec on_save(User :: #user{}) -> NewUser :: #user{}.
on_save(User = #user{item = Item, bag = Bag, body = Body, store = Store}) ->
    NewItem = item_sql:save(Item),
    NewBag = item_sql:save(Bag),
    NewBody = item_sql:save(Body),
    NewStore = item_sql:save(Store),
    User#user{item = NewItem, bag = NewBag, body = NewBody, store = NewStore}.

%% @doc on_expire
-spec on_expire(#user{}) -> #user{}.
on_expire(User = #user{item = Item, bag = Bag, body = Body}) ->
    Now = time:now(),
    {NewItem, DeleteItem} = expire_loop(Item, Now, [], []),
    {NewBag, DeleteBag} = expire_loop(Bag, Now, [], DeleteItem),
    {NewBody, DeleteBody} = expire_loop(Body, Now, [], DeleteBag),
    item_sql:delete_in_item_no(listing:collect(#item.item_no, DeleteBody)),
    _ = DeleteBody =/= [] andalso user_sender:send(User, ?PROTOCOL_ITEM_DELETE, DeleteBody),
    User#user{item = NewItem, bag = NewBag, body = NewBody}.

expire_loop([], _, List, Delete) ->
    {List, Delete};
expire_loop([Item = #item{expire_time = 0} | T], Now, List, Delete) ->
    expire_loop(T, Now, [Item | List], Delete);
expire_loop([Item = #item{expire_time = ExpireTime} | T], Now, List, Delete) ->
    case Now < ExpireTime of
        true ->
            expire_loop(T, Now, List, [Item | Delete]);
        false ->
            expire_loop(T, Now, [Item | List], Delete)
    end.

%% @doc query item
-spec query_item(User :: #user{}) -> ok().
query_item(#user{item = Item}) ->
    {ok, Item}.

%% @doc query bag
-spec query_bag(User :: #user{}) -> ok().
query_bag(#user{bag = Bag}) ->
    {ok, Bag}.

%% @doc query body
-spec query_body(User :: #user{}) -> ok().
query_body(#user{body = Body}) ->
    {ok, Body}.

%% @doc query store
-spec query_store(User :: #user{}) -> ok().
query_store(#user{store = Store}) ->
    {ok, Store}.

%% @doc find
-spec find(User :: #user{}, ItemNo :: non_neg_integer()) -> #item{}.
find(User, ItemNo) ->
    find_loop(User, ItemNo, ?ITEM_TYPE_COMMON).

find_loop(User = #user{item = Item}, ItemNo, ?ITEM_TYPE_COMMON) ->
    case lists:keyfind(ItemNo, #item.item_no, Item) of
        #item{} = Item ->
            Item;
        _ ->
            find_loop(User, ItemNo, ?ITEM_TYPE_BAG)
    end;
find_loop(User = #user{bag = Bag}, ItemNo, ?ITEM_TYPE_BAG) ->
    case lists:keyfind(ItemNo, #item.item_no, Bag) of
        #item{} = Item ->
            Item;
        _ ->
            find_loop(User, ItemNo, ?ITEM_TYPE_BODY)
    end;
find_loop(User = #user{body = Body}, ItemNo, ?ITEM_TYPE_BODY) ->
    case lists:keyfind(ItemNo, #item.item_no, Body) of
        #item{} = Item ->
            Item;
        _ ->
            find_loop(User, ItemNo, ?ITEM_TYPE_STORE)
    end;
find_loop(#user{store = Store}, ItemNo, ?ITEM_TYPE_STORE) ->
    case lists:keyfind(ItemNo, #item.item_no, Store) of
        #item{} = Item ->
            Item;
        _ ->
            #item{}
    end.

%% @doc find
-spec find(User :: #user{}, ItemNo :: non_neg_integer(), Type :: non_neg_integer()) -> #item{}.
find(#user{item = Item}, ItemNo, ?ITEM_TYPE_COMMON) ->
    listing:key_find(ItemNo, #item.item_no, Item, #item{});
find(#user{bag = Bag}, ItemNo, ?ITEM_TYPE_BAG) ->
    listing:key_find(ItemNo, #item.item_no, Bag, #item{});
find(#user{body = Body}, ItemNo, ?ITEM_TYPE_BODY) ->
    listing:key_find(ItemNo, #item.item_no, Body, #item{});
find(#user{store = Store}, ItemNo, ?ITEM_TYPE_STORE) ->
    listing:key_find(ItemNo, #item.item_no, Store, #item{}).

%% @doc store
-spec save(User :: #user{}, Item :: #item{}) -> #user{}.
save(User = #user{item = ItemList}, Item = #item{item_no = ItemNo, type = ?ITEM_TYPE_COMMON}) ->
    NewList = lists:keystore(ItemNo, #item.item_no, ItemList, Item),
    User#user{item = NewList};
save(User = #user{bag = BagList}, Item = #item{item_no = ItemNo, type = ?ITEM_TYPE_BAG}) ->
    NewList = lists:keystore(ItemNo, #item.item_no, BagList, Item),
    User#user{bag = NewList};
save(User = #user{body = BodyList}, Item = #item{item_no = ItemNo, type = ?ITEM_TYPE_BODY}) ->
    NewList = lists:keystore(ItemNo, #item.item_no, BodyList, Item),
    User#user{body = NewList};
save(User = #user{store = StoreList}, Item = #item{item_no = ItemNo, type = ?ITEM_TYPE_STORE}) ->
    NewList = lists:keystore(ItemNo, #item.item_no, StoreList, Item),
    User#user{store = NewList}.

%% @doc empty grid
-spec empty_grid(User :: #user{}, Type :: non_neg_integer()) -> non_neg_integer().
empty_grid(User = #user{item = Item}, Type = ?ITEM_TYPE_COMMON) ->
    max(package:get_capacity(User, Type) - length(Item), 0);
empty_grid(User = #user{bag = Bag}, Type = ?ITEM_TYPE_BAG) ->
    max(package:get_capacity(User, Type) - length(Bag), 0);
empty_grid(User = #user{body = Body}, Type = ?ITEM_TYPE_BODY) ->
    max(package:get_capacity(User, Type) - length(Body), 0);
empty_grid(User = #user{store = Store}, Type = ?ITEM_TYPE_STORE) ->
    max(package:get_capacity(User, Type) - length(Store), 0);
empty_grid(_, _) ->
    0.

%% @doc classify
-spec classify(List :: [#item{} | {non_neg_integer(), non_neg_integer()}]) -> list().
classify(List) ->
    classify_loop(List, []).

classify_loop([], List) ->
    List;
classify_loop([Item = #item{type = Type} | T], List) ->
    classify_loop(T, listing:key_append(Type, List, Item));
classify_loop([{ItemId, Number} | T], List) ->
    classify_loop(T, listing:key_append((item_data:get(ItemId))#item_data.type, List, {ItemId, Number})).

%% @doc overlap
-spec overlap(List :: [{non_neg_integer(), non_neg_integer()}]) -> list().
overlap(List) ->
    overlap_loop(List, []).

overlap_loop([], List) ->
    List;
overlap_loop({ItemId, Number}, List) ->
    listing:update_count(ItemId, List, Number).

%% @doc add item list
-spec add(User :: #user{}, List :: list(), From :: term()) -> ok() | error().
add(User = #user{item = Item, bag = Bag, body = Body, store = Store}, List, From) ->
    case add_loop(User, List, From, time:now(), Item, Bag, Body, Store, [], [], []) of
        {ok, NewUser, Update, Mail, Asset} ->
            %% update
            case Update of
                [] ->
                    ok;
                _ ->
                    user_sender:send(NewUser, ?PROTOCOL_ITEM_QUERY_ITEM, Update)
            end,

            %% mail
            MailUser = case Mail of
                [] ->
                    NewUser;
                _ ->
                    mail:add(NewUser, mail_text_add_item_title, mail_text_add_item_content, item, Mail)
            end,

            %% asset
            case Asset of
                [] ->
                    {ok, MailUser};
                _ ->
                    asset:add(MailUser, Asset, From)
            end;
        Error ->
            Error
    end.

%% add loop
add_loop(User, [], _, _, Item, Bag, Body, Store, Update, Mail, Asset) ->
    {ok, User#user{item = Item, bag = Bag, body = Body, store = Store}, Update, Mail, Asset};
add_loop(User = #user{role_id = RoleId}, [{ItemId, Number} | T], From, Now, CommonItem, Bag, Body, Store, Update, Mail, Asset) ->
    case item_data:get(ItemId) of
        #item_data{type = ?ITEM_TYPE_ASSET, use_effect = EffectAsset} ->
            add_loop(User, T, From, Now, CommonItem, Bag, Body, Store, Update, Mail, [{EffectAsset, Number} | Asset]);

        ItemData = #item_data{type = Type = ?ITEM_TYPE_COMMON, overlap = 1} ->
            Capacity = package:get_capacity(User, Type),
            %% do not overlap, check capacity and add new directly
            {NewList, NewUpdate, NewMail} = add_overlap(RoleId, ItemData, Number, From, Now, Capacity, [], CommonItem, Update, Mail),
            add_loop(User, T, From, Now, NewList, Bag, Body, Store, NewUpdate, NewMail, Asset);
        ItemData = #item_data{type = Type = ?ITEM_TYPE_COMMON} ->
            %% ItemList = get_list(User, Type),
            Capacity = package:get_capacity(User, Type),
            %% overlap, check capacity and add new after loop overlap
            {NewList, NewUpdate, NewMail} = add_overlap(RoleId, ItemData, Number, From, Now, Capacity, CommonItem, [], Update, Mail),
            add_loop(User, T, From, Now, NewList, Bag, Body, Store, NewUpdate, NewMail, Asset);

        ItemData = #item_data{type = Type = ?ITEM_TYPE_BAG, overlap = 1} ->
            Capacity = package:get_capacity(User, Type),
            %% do not overlap, check capacity and add new directly
            {NewList, NewUpdate, NewMail} = add_overlap(RoleId, ItemData, Number, From, Now, Capacity, [], Bag, Update, Mail),
            add_loop(User, T, From, Now, CommonItem, NewList, Body, Store, NewUpdate, NewMail, Asset);
        ItemData = #item_data{type = Type = ?ITEM_TYPE_BAG} ->
            %% ItemList = get_list(User, Type),
            Capacity = package:get_capacity(User, Type),
            %% overlap, check capacity and add new after loop overlap
            {NewList, NewUpdate, NewMail} = add_overlap(RoleId, ItemData, Number, From, Now, Capacity, Bag, [], Update, Mail),
            add_loop(User, T, From, Now, CommonItem, NewList, Body, Store, NewUpdate, NewMail, Asset);

        ItemData = #item_data{type = Type = ?ITEM_TYPE_BODY, overlap = 1} ->
            Capacity = package:get_capacity(User, Type),
            %% do not overlap, check capacity and add new directly
            {NewList, NewUpdate, NewMail} = add_overlap(RoleId, ItemData, Number, From, Now, Capacity, [], Body, Update, Mail),
            add_loop(User, T, From, Now, CommonItem, Bag, NewList, Store, NewUpdate, NewMail, Asset);
        ItemData = #item_data{type = Type = ?ITEM_TYPE_BODY} ->
            %% ItemList = get_list(User, Type),
            Capacity = package:get_capacity(User, Type),
            %% overlap, check capacity and add new after loop overlap
            {NewList, NewUpdate, NewMail} = add_overlap(RoleId, ItemData, Number, From, Now, Capacity, Body, [], Update, Mail),
            add_loop(User, T, From, Now, CommonItem, Bag, NewList, Store, NewUpdate, NewMail, Asset);

        ItemData = #item_data{type = Type = ?ITEM_TYPE_STORE, overlap = 1} ->
            Capacity = package:get_capacity(User, Type),
            %% do not overlap, check capacity and add new directly
            {NewList, NewUpdate, NewMail} = add_overlap(RoleId, ItemData, Number, From, Now, Capacity, [], Store, Update, Mail),
            add_loop(User, T, From, Now, CommonItem, Bag, Body, NewList, NewUpdate, NewMail, Asset);
        ItemData = #item_data{type = Type = ?ITEM_TYPE_STORE} ->
            %% ItemList = get_list(User, Type),
            Capacity = package:get_capacity(User, Type),
            %% overlap, check capacity and add new after loop overlap
            {NewList, NewUpdate, NewMail} = add_overlap(RoleId, ItemData, Number, From, Now, Capacity, Store, [], Update, Mail),
            add_loop(User, T, From, Now, CommonItem, Bag, Body, NewList, NewUpdate, NewMail, Asset);

        _ ->
            {error, ItemId}
    end.

%% add new item list
add_overlap(RoleId, ItemData = #item_data{item_id = ItemId, overlap = Overlap}, Number, From, Now, Capacity, [], ItemList, Update, Mail) ->
    case length(ItemList) < Capacity of
        true ->
            %% capacity enough
            case Number =< Overlap of
                true ->
                    %% single item
                    NewItem = add_new(RoleId, ItemData, Number, From, Now),
                    %% add complete
                    {[NewItem | ItemList], [NewItem | Update], Mail};
                false ->
                    %% multi item
                    NewItem = add_new(RoleId, ItemData, Overlap, From, Now),
                    %% add overlap continue
                    add_overlap(RoleId, ItemData, Number - Overlap, From, Now, Capacity, [], [NewItem | ItemList], [NewItem | Update], Mail)
            end;
        false ->
            %% capacity not enough add to mail
            {ItemList, Update, [{ItemId, Number} | Mail]}
    end;

%% find and overlap to old item list
add_overlap(RoleId, ItemData = #item_data{item_id = ItemId, overlap = Overlap}, Number, From, Now, Capacity, [Item = #item{item_id = ItemId, number = OldNumber} | T], ItemList, Update, Mail) ->
    case OldNumber + Number =< Overlap of
        true ->
            %% overlap all to old
            NewItem = Item#item{number = OldNumber + Number, flag = 1},
            %% log
            log:item_produce_log(RoleId, ItemId, From, overlap, Now),
            %% merge two list
            {lists:reverse([NewItem | T], ItemList), [NewItem | Update], Mail};
        _ ->
            %% overlap to old
            NewItem = Item#item{number = Overlap, flag = 1},
            %% log
            log:item_produce_log(RoleId, ItemId, From, overlap, Now),
            %% continue
            add_overlap(RoleId, ItemData, Number - (Overlap - OldNumber), From, Now, Capacity, T, [NewItem | ItemList], [NewItem | Update], Mail)
    end;

%% not this type
add_overlap(RoleId, ItemData, Number, From, Now, Capacity, [H | T], ItemList, Update, Mail) ->
    add_overlap(RoleId, ItemData, Number, From, Now, Capacity, T, [H | ItemList], Update, Mail).

%% add new item
add_new(RoleId, #item_data{item_id = ItemId, type = Type, expire_time = 0}, Number, From, Now) ->
    %% get item no
    ItemNo = increment_server:next(?MODULE),
    %% log
    log:item_produce_log(RoleId, ItemId, From, new, Now),
    %% item is permanent
    #item{item_no = ItemNo, role_id = RoleId, item_id = ItemId, number = Number, type = Type, expire_time = 0, flag = 1};
add_new(RoleId, #item_data{item_id = ItemId, type = Type, expire_time = ExpireTime}, Number, From, Now) ->
    %% get item no
    ItemNo = increment_server:next(?MODULE),
    %% log
    log:item_produce_log(RoleId, ItemId, From, new, Now),
    %% item with expire time
    #item{item_no = ItemNo, role_id = RoleId, item_id = ItemId, number = Number, type = Type, expire_time = Now + ExpireTime, flag = 1}.

%% @doc validate list by item no
%% use for item no, item number, item type
%% attention !!! merge list is necessary.
-spec validate(User :: #user{}, List :: [{ItemNo :: non_neg_integer(), Number :: non_neg_integer(), Type :: non_neg_integer()}], From :: term()) -> ok() | error().
validate(User, List, From) ->
    validate_loop(List, User, From).

validate_loop([], _, _) ->
    {ok, ok};
validate_loop([{Asset, Number, ?ITEM_TYPE_ASSET} | T], User, From) ->
    case asset:check(User, [{Asset, Number}], From) of
        ok ->
            validate_loop(T, User, From);
        Error ->
            Error
    end;
validate_loop([{ItemNo, Number, ?ITEM_TYPE_COMMON} | T], User = #user{item = Item}, From) ->
    case lists:keyfind(ItemNo, #item.item_no, Item) of
        #item{number = ThisNumber} when Number =< ThisNumber ->
            validate_loop(T, User, From);
        _ ->
            {error, item_not_enough}
    end;
validate_loop([{ItemNo, Number, ?ITEM_TYPE_BAG} | T], User = #user{bag = Bag}, From) ->
    case lists:keyfind(ItemNo, #item.item_no, Bag) of
        #item{number = ThisNumber} when Number =< ThisNumber ->
            validate_loop(T, User, From);
        _ ->
            {error, item_not_enough}
    end;
validate_loop([{ItemNo, Number, ?ITEM_TYPE_BODY} | T], User = #user{body = Body}, From) ->
    case lists:keyfind(ItemNo, #item.item_no, Body) of
        #item{number = ThisNumber} when Number =< ThisNumber ->
            validate_loop(T, User, From);
        _ ->
            {error, item_not_enough}
    end;
validate_loop([{ItemNo, Number, ?ITEM_TYPE_STORE} | T], User = #user{store = Store}, From) ->
    case lists:keyfind(ItemNo, #item.item_no, Store) of
        #item{number = ThisNumber} when Number =< ThisNumber ->
            validate_loop(T, User, From);
        _ ->
            {error, item_not_enough}
    end.

%% @doc reduce item no list
%% reduce item/asset list from check result list
-spec reduce(User :: #user{}, List :: [{ItemNo :: non_neg_integer(), Number :: non_neg_integer(), Type :: non_neg_integer()}], From :: term()) -> ok() | error().
reduce(User = #user{role_id = RoleId, item = Item, bag = Bag, body = Body, store = Store}, List, From) ->
    case reduce_loop(List, User, From, time:now(), Item, Bag, Body, Store, [], [], []) of
        {ok, NewUser, Update, Delete, Asset} ->
            %% update
            case Update of
                [] ->
                    ok;
                _ ->
                    user_sender:send(NewUser, ?PROTOCOL_ITEM_QUERY_ITEM, Update),
                    [log:item_consume_log(RoleId, ItemId, reduce, From, time:now()) || #item{item_id = ItemId} <- Update]
            end,

            %% delete
            case Delete of
                [] ->
                    ok;
                _ ->
                    user_sender:send(NewUser, ?PROTOCOL_ITEM_DELETE, Delete),
                    item_sql:delete_in_item_no(listing:collect(#item.item_no, Delete)),
                    [log:item_consume_log(RoleId, ItemId, delete, From, time:now()) || #item{item_id = ItemId} <- Delete]
            end,

            %% asset
            asset:cost(NewUser, Asset, From);
        Error ->
            Error
    end.

reduce_loop([], User, _, _, Item, Bag, Body, Store, Update, Delete, Asset) ->
    {ok, User#user{item = Item, bag = Bag, body = Body, store = Store}, Update, Delete, Asset};
reduce_loop([{Asset, Number, ?ITEM_TYPE_ASSET} | T], User, From, Now, CommonItem, Bag, Body, Store, Update, Delete, Asset) ->
    case asset:check(User, [{Asset, Number}], From) of
        ok ->
            reduce_loop(T, User, From, Now, CommonItem, Bag, Body, Store, Update, Delete, [{Asset, Number} | Asset]);
        Error ->
            Error
    end;
reduce_loop([{ItemNo, Number, ?ITEM_TYPE_COMMON} | T], User, From, Now, CommonItem, Bag, Body, Store, Update, Delete, Asset) ->
    case lists:keyfind(ItemNo, #item.item_no, CommonItem) of
        Item = #item{number = ThisNumber} when Number < ThisNumber ->
            NewItem = Item#item{number = ThisNumber - Number},
            NewList = lists:keyreplace(ItemNo, #item.item_no, CommonItem, NewItem),
            reduce_loop(T, User, From, Now, NewList, Bag, Body, Store, [NewItem | Update], Delete, Asset);
        Item = #item{number = Number} ->
            NewItem = Item#item{number = 0},
            NewList = lists:keydelete(ItemNo, #item.item_no, CommonItem),
            reduce_loop(T, User, From, Now, NewList, Bag, Body, Store, Update, [NewItem | Delete], Asset);
        _ ->
            {error, no_such_item}
    end;
reduce_loop([{ItemNo, Number, ?ITEM_TYPE_BAG} | T], User, From, Now, CommonItem, Bag, Body, Store, Update, Delete, Asset) ->
    case lists:keyfind(ItemNo, #item.item_no, Bag) of
        Item = #item{number = ThisNumber} when Number < ThisNumber ->
            NewItem = Item#item{number = ThisNumber - Number},
            NewList = lists:keyreplace(ItemNo, #item.item_no, Bag, NewItem),
            reduce_loop(T, User, From, Now, Item, NewList, Body, Store, [NewItem | Update], Delete, Asset);
        Item = #item{number = Number} ->
            NewItem = Item#item{number = 0},
            NewList = lists:keydelete(ItemNo, #item.item_no, Bag),
            reduce_loop(T, User, From, Now, CommonItem, NewList, Body, Store, Update, [NewItem | Delete], Asset);
        _ ->
            {error, no_such_item}
    end;
reduce_loop([{ItemNo, Number, ?ITEM_TYPE_BODY} | T], User, From, Now, CommonItem, Bag, Body, Store, Update, Delete, Asset) ->
    case lists:keyfind(ItemNo, #item.item_no, Body) of
        Item = #item{number = ThisNumber} when Number < ThisNumber ->
            NewItem = Item#item{number = ThisNumber - Number},
            NewList = lists:keyreplace(ItemNo, #item.item_no, Body, NewItem),
            reduce_loop(T, User, From, Now, Item, Bag, NewList, Store, [NewItem | Update], Delete, Asset);
        Item = #item{number = Number} ->
            NewItem = Item#item{number = 0},
            NewList = lists:keydelete(ItemNo, #item.item_no, Body),
            reduce_loop(T, User, From, Now, CommonItem, Bag, NewList, Store, Update, [NewItem | Delete], Asset);
        _ ->
            {error, no_such_item}
    end;
reduce_loop([{ItemNo, Number, ?ITEM_TYPE_STORE} | T], User, From, Now, CommonItem, Bag, Body, Store, Update, Delete, Asset) ->
    case lists:keyfind(ItemNo, #item.item_no, Store) of
        Item = #item{number = ThisNumber} when Number < ThisNumber ->
            NewItem = Item#item{number = ThisNumber - Number},
            NewList = lists:keyreplace(ItemNo, #item.item_no, Store, NewItem),
            reduce_loop(T, User, From, Now, Item, Bag, Body, NewList, [NewItem | Update], Delete, Asset);
        Item = #item{number = Number} ->
            NewItem = Item#item{number = 0},
            NewList = lists:keydelete(ItemNo, #item.item_no, Store),
            reduce_loop(T, User, From, Now, CommonItem, Bag, Body, NewList, Update, [NewItem | Delete], Asset);
        _ ->
            {error, no_such_item}
    end.

%% @doc check list by item id
%% use for item id, item number
%% attention !!! use reduce function to reduce return cost item/asset.
-spec check(User :: #user{}, List :: [{ItemId :: non_neg_integer(), Number :: non_neg_integer()}], From :: term()) -> ok() | error().
check(User = #user{item = Item, bag = Bag, body = Body, store = Store}, List, From) ->
    check_loop(List, User, From, Item, Bag, Body, Store, []).

check_loop([], _, _, _, _, _, _, Result) ->
    {ok, Result};
check_loop([H = {ItemId, Number} | T], User, From, CommonItem, Bag, Body, Store, Result) ->
    case item_data:get(ItemId) of
        #item_data{type = ?ITEM_TYPE_ASSET, use_effect = Asset} ->
            case asset:check(User, [{Asset, Number}], From) of
                ok ->
                    check_loop(T, User, From, CommonItem, Bag, Body, Store, [{Asset, Number, ?ITEM_TYPE_ASSET} | Result]);
                Error ->
                    Error
            end;
        #item_data{type = ?ITEM_TYPE_COMMON} ->
            case check_one_loop(CommonItem, H, Result) of
                {ok, NewResult} ->
                    check_loop(T, User, From, CommonItem, Bag, Body, Store, NewResult);
                Error ->
                    Error
            end;
        #item_data{type = ?ITEM_TYPE_BAG} ->
            case check_one_loop(Bag, H, Result) of
                {ok, NewResult} ->
                    check_loop(T, User, From, CommonItem, Bag, Body, Store, NewResult);
                Error ->
                    Error
            end;
        #item_data{type = ?ITEM_TYPE_BODY} ->
            case check_one_loop(Body, H, Result) of
                {ok, NewResult} ->
                    check_loop(T, User, From, CommonItem, Bag, Body, Store, NewResult);
                Error ->
                    Error
            end;
        #item_data{type = ?ITEM_TYPE_STORE} ->
            case check_one_loop(Store, H, Result) of
                {ok, NewResult} ->
                    check_loop(T, User, From, CommonItem, Bag, Body, Store, NewResult);
                Error ->
                    Error
            end;
        _ ->
            {error, ItemId}
    end.

check_one_loop([], _, _) ->
    %% not enough item
    {error, item_not_enough};
check_one_loop([#item{item_no = ItemNo, item_id = ItemId, number = Number, type = Type} | _], {ItemId, NeedNumber}, Result) when NeedNumber =< Number ->
    %% enough
    {ok, [{ItemNo, NeedNumber, Type} | Result]};
check_one_loop([#item{item_no = ItemNo, item_id = ItemId, number = Number, type = Type} | T], {ItemId, NeedNumber}, Result) when NeedNumber > Number ->
    %% not enough
    check_one_loop(T, {ItemId, NeedNumber - Number}, [{ItemNo, Number, Type} | Result]);
check_one_loop([_ | T], {ItemId, NeedNumber}, Result) ->
    %% not need item
    check_one_loop(T, {ItemId, NeedNumber}, Result).

%% @doc cost list by item id
%% cost item/asset directly, return failed when item/asset not enough
-spec cost(User :: #user{}, List :: [{ItemId :: non_neg_integer(), Number :: non_neg_integer()}], From :: term()) -> ok() | error().
cost(User = #user{role_id = RoleId, item = Item, bag = Bag, body = Body, store = Store}, List, From) ->
    case cost_loop(List, User, From, time:now(), Item, Bag, Body, Store, [], [], []) of
        {ok, NewUser, [], [], []} ->
            {ok, NewUser};
        {ok, NewUser, Update, [], []} ->
            %% update
            user_sender:send(NewUser, ?PROTOCOL_ITEM_QUERY_ITEM, Update),
            [log:item_consume_log(RoleId, ItemId, reduce, From, time:now()) || #item{item_id = ItemId} <- Update],
            {ok, NewUser};
        {ok, NewUser, Update, Delete, []} ->
            %% update
            user_sender:send(NewUser, ?PROTOCOL_ITEM_QUERY_ITEM, Update),
            [log:item_consume_log(RoleId, ItemId, reduce, From, time:now()) || #item{item_id = ItemId} <- Update],
            %% delete
            user_sender:send(NewUser, ?PROTOCOL_ITEM_DELETE, Delete),
            item_sql:delete_in_item_no(listing:collect(#item.item_no, Delete)),
            [log:item_consume_log(RoleId, ItemId, delete, From, time:now()) || #item{item_id = ItemId} <- Delete],
            {ok, NewUser};
        {ok, NewUser, Update, Delete, Asset} ->
            %% update
            user_sender:send(NewUser, ?PROTOCOL_ITEM_QUERY_ITEM, Update),
            [log:item_consume_log(RoleId, ItemId, reduce, From, time:now()) || #item{item_id = ItemId} <- Update],
            %% delete
            user_sender:send(NewUser, ?PROTOCOL_ITEM_DELETE, Delete),
            item_sql:delete_in_item_no(listing:collect(#item.item_no, Delete)),
            [log:item_consume_log(RoleId, ItemId, delete, From, time:now()) || #item{item_id = ItemId} <- Delete],
            %% asset
            asset:cost(NewUser, Asset, From);
        Error ->
            Error
    end.

cost_loop([], User, _, _, Item, Bag, Body, Store, Update, Delete, Asset) ->
    {ok, User#user{item = Item, bag = Bag, body = Body, store = Store}, Update, Delete, Asset};
cost_loop([H = {ItemId, Number} | T], User, From, Now, CommonItem, Bag, Body, Store, Update, Delete, Asset) ->
    case item_data:get(ItemId) of
        #item_data{type = ?ITEM_TYPE_ASSET, use_effect = EffectAsset} ->
            case asset:check(User, [{EffectAsset, Number}], From) of
                ok ->
                    cost_loop(T, User, From, Now, CommonItem, Bag, Body, Store, Update, Delete, [{EffectAsset, Number} | Asset]);
                Error ->
                    Error
            end;
        #item_data{type = ?ITEM_TYPE_COMMON} ->
            case cost_one_loop(CommonItem, H, [], Update, Delete) of
                {ok, NewList, NewUpdate, NewDelete} ->
                    cost_loop(T, User, From, Now, NewList, Bag, Body, Store, NewUpdate, NewDelete, Asset);
                Error ->
                    Error
            end;
        #item_data{type = ?ITEM_TYPE_BAG} ->
            case cost_one_loop(Bag, H, [], Update, Delete) of
                {ok, NewList, NewUpdate, NewDelete} ->
                    cost_loop(T, User, From, Now, CommonItem, NewList, Body, Store, NewUpdate, NewDelete, Asset);
                Error ->
                    Error
            end;
        #item_data{type = ?ITEM_TYPE_BODY} ->
            case cost_one_loop(Body, H, [], Update, Delete) of
                {ok, NewList, NewUpdate, NewDelete} ->
                    cost_loop(T, User, From, Now, CommonItem, Bag, NewList, Store, NewUpdate, NewDelete, Asset);
                Error ->
                    Error
            end;
        #item_data{type = ?ITEM_TYPE_STORE} ->
            case cost_one_loop(Store, H, [], Update, Delete) of
                {ok, NewList, NewUpdate, NewDelete} ->
                    cost_loop(T, User, From, Now, CommonItem, Bag, Body, NewList, NewUpdate, NewDelete, Asset);
                Error ->
                    Error
            end;
        _ ->
            {error, ItemId}
    end.

cost_one_loop([], _, _, _, _) ->
    %% not enough item
    {error, item_not_enough};
cost_one_loop([Item = #item{item_id = ItemId, number = Number} | T], {ItemId, NeedNumber}, List, Update, Delete) when NeedNumber < Number ->
    %% enough
    NewItem = Item#item{number = Number - NeedNumber},
    {ok, lists:reverse([NewItem | List], T), [NewItem | Update], Delete};
cost_one_loop([Item = #item{item_id = ItemId, number = Number} | T], {ItemId, Number}, List, Update, Delete) ->
    %% enough
    NewItem = Item#item{number = 0},
    {ok, lists:reverse(List, T), Update, [NewItem | Delete]};
cost_one_loop([Item = #item{item_id = ItemId, number = Number} | T], {ItemId, NeedNumber}, List, Update, Delete) when NeedNumber > Number ->
    %% not enough
    NewItem = Item#item{number = 0},
    cost_one_loop(T, {ItemId, NeedNumber - Number}, List, Update, [NewItem | Delete]);
cost_one_loop([H | T], {ItemId, NeedNumber}, List, Update, Delete) ->
    %% not need item
    cost_one_loop(T, {ItemId, NeedNumber}, [H | List], Update, Delete).

%%%===================================================================
%%% Internal functions
%%%===================================================================
