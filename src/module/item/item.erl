%%%-------------------------------------------------------------------
%%% @doc
%%% item
%%% @end
%%%-------------------------------------------------------------------
-module(item).
%% API
-export([load/1, save/1]).
-export([query_item/1, query_body/1, query_bag/1, query_store/1]).
-export([find/3, store/2]).
-export([get_list/2, save_list/3]).
-export([get_capacity/2, save_capacity/3]).
-export([empty_grid/2]).
-export([classify/1, overlap/1]).
-export([add/3]).
-export([validate/3, reduce/3]).
-export([check/3, cost/3]).
-export([expire/1]).
%% Includes
-include("common.hrl").
-include("protocol.hrl").
-include("user.hrl").
-include("role.hrl").
-include("item.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc load
-spec load(User :: #user{}) -> NewUser :: #user{}.
load(User = #user{role_id = RoleId}) ->
    DataList = item_sql:select_by_role_id(RoleId),
    %% split diff type
    load_loop(classify(DataList), User).

load_loop([], User) ->
    User;
load_loop([{Type, List} | T], User) ->
    load_loop(T, save_list(User, Type, List)).

%% @doc save
-spec save(User :: #user{}) -> NewUser :: #user{}.
save(User) ->
    save_loop(?ITEM_TYPE_LIST, User).

save_loop([], User) ->
    User;
save_loop([Type | T], User) ->
    save_loop(T, save_list(User, Type, item_sql:insert_update(get_list(User, Type)))).

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
-spec find(User :: #user{}, ItemNo :: non_neg_integer(), Type :: non_neg_integer()) -> #item{}.
find(User, ItemNo, Type) ->
    listing:key_find(ItemNo, #item.item_no, get_list(User, Type), #item{}).

%% @doc store
-spec store(User :: #user{}, Item :: #item{}) -> #user{}.
store(User, Item = #item{item_no = ItemNo, type = Type}) ->
    NewList = lists:keystore(ItemNo, #item.item_no, get_list(User, Type), Item),
    save_list(User, Type, NewList).

%% @doc list user field position (add type filed map @here)
-spec list_position(non_neg_integer()) -> non_neg_integer().
list_position(?ITEM_TYPE_COMMON) ->
    #user.item;
list_position(?ITEM_TYPE_BAG) ->
    #user.bag;
list_position(?ITEM_TYPE_BODY) ->
    #user.body;
list_position(?ITEM_TYPE_STORE) ->
    #user.store;
list_position(_) ->
    0.

%% @doc list size role field position (add type size map @here)
-spec size_position(non_neg_integer()) -> non_neg_integer().
size_position(?ITEM_TYPE_COMMON) ->
    #role.item_size;
size_position(?ITEM_TYPE_BAG) ->
    #role.bag_size;
size_position(?ITEM_TYPE_STORE) ->
    #role.store_size;
size_position(_) ->
    0.

%% @doc item list
-spec get_list(#user{}, non_neg_integer()) -> [#item{}].
get_list(User, Type) ->
    case list_position(Type) of
        0 ->
            [];
        Position ->
            element(Position, User)
    end.

%% @doc item list
-spec save_list(#user{}, non_neg_integer(), [#item{}]) -> #user{}.
save_list(User, Type, List) ->
    case list_position(Type) of
        0 ->
            User;
        Position ->
            setelement(Position, User, List)
    end.

%% @doc get capacity
-spec get_capacity(#user{}, non_neg_integer()) -> non_neg_integer().
get_capacity(#user{role = Role}, Type) ->
    case size_position(Type) of
        0 ->
            0;
        Position ->
            element(Position, Role)
    end.

%% @doc save capacity
-spec save_capacity(#user{}, non_neg_integer(), non_neg_integer()) -> #user{}.
save_capacity(User = #user{role = Role}, Type, Size) ->
    case list_position(Type) of
        0 ->
            User;
        Position ->
            User#user{role = setelement(Position, Role, Size)}
    end.

%% @doc empty grid
-spec empty_grid(User :: #user{}, Type :: non_neg_integer()) -> non_neg_integer().
empty_grid(User, Type) ->
    max(get_capacity(User, Type) - length(get_list(User, Type)), 0).

%% @doc classify
-spec classify(List :: [#item{} | {non_neg_integer(), non_neg_integer()}]) -> list().
classify(List) ->
    classify_loop(List, []).

classify_loop([], List) ->
    List;
classify_loop([H = #item{type = Type} | T], List) ->
    classify_loop(T, listing:key_append(Type, List, H));
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
add(User, List, From) ->
    case add_loop(User, List, From, time:now(), [], [], []) of
        {ok, NewUser, [], [], []} ->
            {ok, NewUser};
        {ok, NewUser, Update, [], []} ->
            %% update
            user_sender:send(User, ?PROTOCOL_ITEM_QUERY_ITEM, Update),
            {ok, NewUser};
        {ok, NewUser, Update, Mail, []} ->
            %% update
            user_sender:send(User, ?PROTOCOL_ITEM_QUERY_ITEM, Update),
            %% mail
            NewestUser = mail:add(NewUser, add_item_title, add_item_content, item, Mail),
            {ok, NewestUser};
        {ok, NewUser, Update, Mail, Asset} ->
            %% update
            user_sender:send(User, ?PROTOCOL_ITEM_QUERY_ITEM, Update),
            %% mail
            NewestUser = mail:add(NewUser, add_item_title, add_item_content, item, Mail),
            %% asset
            asset:add(NewestUser, Asset, From);
        Error ->
            Error
    end.

%% add loop
add_loop(User, [], _, _, Update, Mail, Asset) ->
    {ok, User, Update, Mail, Asset};
add_loop(User = #user{role_id = RoleId}, [{ItemId, Number} | T], From, Now, Update, Mail, Asset) ->
    case item_data:get(ItemId) of
        #item_data{type = ?ITEM_TYPE_ASSET, use_effect = EffectAsset} ->
            add_loop(User, T, From, Now, Update, Mail, [{EffectAsset, Number} | Asset]);
        ItemData = #item_data{type = Type, overlap = 1} ->
            ItemList = get_list(User, Type),
            Capacity = get_capacity(User, Type),
            %% do not overlap, check capacity and add new directly
            {NewItemList, NewUpdate, NewMail} = add_overlap(RoleId, ItemData, Number, From, Now, Capacity, [], ItemList, Update, Mail),
            NewUser = save_list(User, Type, NewItemList),
            add_loop(NewUser, T, From, Now, NewUpdate, NewMail, Asset);
        ItemData = #item_data{type = Type} ->
            ItemList = get_list(User, Type),
            Capacity = get_capacity(User, Type),
            %% overlap, check capacity and add new after loop overlap
            {NewItemList, NewUpdate, NewMail} = add_overlap(RoleId, ItemData, Number, From, Now, Capacity, ItemList, [], Update, Mail),
            NewUser = save_list(User, Type, NewItemList),
            add_loop(NewUser, T, From, Now, NewUpdate, NewMail, Asset);
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
add_new(RoleId, #item_data{item_id = ItemId, type = Type, time = 0}, Number, From, Now) ->
    %% get unique no
    ItemNo = increment_server:next(?MODULE),
    %% log
    log:item_produce_log(RoleId, ItemId, From, new, Now),
    %% item is permanent
    #item{item_no = ItemNo, role_id = RoleId, item_id = ItemId, number = Number, type = Type, expire_time = 0, flag = 1};
add_new(RoleId, #item_data{item_id = ItemId, type = Type, time = Time}, Number, From, Now) ->
    %% get unique no
    ItemNo = increment_server:next(?MODULE),
    %% log
    log:item_produce_log(RoleId, ItemId, From, new, Now),
    %% item with expire time
    #item{item_no = ItemNo, role_id = RoleId, item_id = ItemId, number = Number, type = Type, expire_time = Now + Time, flag = 1}.

%% @doc validate list by item no
%% use for item no, item number, item type
%% attention !!! merge list is necessary.
-spec validate(User :: #user{}, [{ItemNo :: non_neg_integer(), Number :: non_neg_integer(), Type :: non_neg_integer()}], From :: term()) -> ok() | error().
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
validate_loop([{ItemNo, Number, Type} | T], User, From) ->
    List = get_list(User, Type),
    case lists:keyfind(ItemNo, #item.item_no, List) of
        #item{number = ThisNumber} when Number =< ThisNumber ->
            validate_loop(T, User, From);
        _ ->
            {error, item_not_enough}
    end.

%% @doc reduce item no list
%% reduce item/asset list from check result list
-spec reduce(User :: #user{}, List :: list(), From :: term()) -> ok() | error().
reduce(User = #user{role_id = RoleId}, List, From) ->
    case reduce_loop(List, User, From, [], [], []) of
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
            [log:item_consume_log(RoleId, ItemId, reduce, From, time:now()) || #item{item_id = ItemId} <- Delete],
            {ok, NewUser};
        {ok, NewUser, Update, Delete, Asset} ->
            %% update
            user_sender:send(NewUser, ?PROTOCOL_ITEM_QUERY_ITEM, Update),
            [log:item_consume_log(RoleId, ItemId, reduce, From, time:now()) || #item{item_id = ItemId} <- Update],
            %% delete
            user_sender:send(NewUser, ?PROTOCOL_ITEM_DELETE, Delete),
            item_sql:delete_in_item_no(listing:collect(#item.item_no, Delete)),
            [log:item_consume_log(RoleId, ItemId, reduce, From, time:now()) || #item{item_id = ItemId} <- Delete],
            %% asset
            asset:cost(NewUser, Asset, From);
        Error ->
            Error
    end.

reduce_loop([], User, _, Update, Delete, Asset) ->
    {ok, User, Update, Delete, Asset};
reduce_loop([{CheckAsset, Number, ?ITEM_TYPE_ASSET} | T], User, From, Update, Delete, Asset) ->
    case asset:check(User, [{CheckAsset, Number}], From) of
        ok ->
            reduce_loop(T, User, From, Update, Delete, [{CheckAsset, Number} | Asset]);
        Error ->
            Error
    end;
reduce_loop([{ItemNo, Number, Type} | T], User, From, Update, Delete, Asset) ->
    List = get_list(User, Type),
    case lists:keyfind(ItemNo, #item.item_no, List) of
        Item = #item{number = THisNumber} when Number < THisNumber ->
            NewItem = Item#item{number = THisNumber - Number},
            NewList = lists:keyreplace(ItemNo, #item.item_no, List, NewItem),
            NewUser = save_list(User, Type, NewList),
            reduce_loop(T, NewUser, From, [NewItem | Update], Delete, Asset);
        Item = #item{number = Number} ->
            NewItem = Item#item{number = 0},
            NewList = lists:keydelete(ItemNo, #item.item_no, List),
            NewUser = save_list(User, Type, NewList),
            reduce_loop(T, NewUser, From, Update, [NewItem | Delete], Asset);
        _ ->
            {error, no_such_item}
    end.

%% @doc check list by item id
%% use for item id, item number
%% attention !!! use reduce function to reduce return cost item/asset.
-spec check(User :: #user{}, list(), From :: term()) -> ok() | error().
check(User, List, From) ->
    check_loop(List, User, From, []).

check_loop([], _, _, Result) ->
    {ok, Result};
check_loop([H = {ItemId, Number} | T], User, From, Result) ->
    case item_data:get(ItemId) of
        #item_data{type = ?ITEM_TYPE_ASSET, use_effect = Asset} ->
            case asset:check(User, [{Asset, Number}], From) of
                ok ->
                    check_loop(T, User, From, [{Asset, Number, ?ITEM_TYPE_ASSET} | Result]);
                Error ->
                    Error
            end;
        #item_data{type = Type} ->
            List = get_list(User, Type),
            case check_one_loop(List, H, Result) of
                {ok, NewResult} ->
                    check_loop(T, User, From, NewResult);
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
-spec cost(User :: #user{}, list(), From :: term()) -> ok() | error().
cost(User = #user{role_id = RoleId}, List, From) ->
    case cost_loop(List, User, From, [], [], []) of
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
            [log:item_consume_log(RoleId, ItemId, reduce, From, time:now()) || #item{item_id = ItemId} <- Delete],
            {ok, NewUser};
        {ok, NewUser, Update, Delete, Asset} ->
            %% update
            user_sender:send(NewUser, ?PROTOCOL_ITEM_QUERY_ITEM, Update),
            [log:item_consume_log(RoleId, ItemId, reduce, From, time:now()) || #item{item_id = ItemId} <- Update],
            %% delete
            user_sender:send(NewUser, ?PROTOCOL_ITEM_DELETE, Delete),
            item_sql:delete_in_item_no(listing:collect(#item.item_no, Delete)),
            [log:item_consume_log(RoleId, ItemId, reduce, From, time:now()) || #item{item_id = ItemId} <- Delete],
            %% asset
            asset:cost(NewUser, Asset, From);
        Error ->
            Error
    end.

cost_loop([], User, _, Update, Delete, Asset) ->
    {ok, User, Update, Delete, Asset};
cost_loop([H = {ItemId, Number} | T], User, From, Update, Delete, Asset) ->
    case item_data:get(ItemId) of
        #item_data{type = ?ITEM_TYPE_ASSET, use_effect = EffectAsset} ->
            case asset:check(User, [{EffectAsset, Number}], From) of
                ok ->
                    cost_loop(T, User, From, Update, Delete, [{EffectAsset, Number} | Asset]);
                Error ->
                    Error
            end;
        #item_data{type = Type} ->
            List = get_list(User, Type),
            case cost_one_loop(List, H, [], Update, Delete) of
                {ok, NewList, NewUpdate, NewDelete} ->
                    NewUser = save_list(User, Type, NewList),
                    cost_loop(T, NewUser, From, NewUpdate, NewDelete, Asset);
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

%% @doc expire
-spec expire(#user{}) -> #user{}.
expire(User = #user{item = Item, bag = Bag, body = Body}) ->
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

%%%===================================================================
%%% Internal functions
%%%===================================================================
