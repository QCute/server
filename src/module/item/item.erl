%%%-------------------------------------------------------------------
%%% @doc
%%% module item base, manager item bag
%%% @end
%%%-------------------------------------------------------------------
-module(item).
%% API
-export([load/1, save/1]).
-export([query_item/1, query_body/1, query_bag/1, query_store/1]).
-export([find/3, store/2]).
-export([get_list_by_type/2, save_list_by_type/3]).
-export([get_size_by_type/2, save_size_by_type/3]).
-export([empty_grid/2]).
-export([classify/1, data_classify/1]).
-export([add/3, reduce/3, validate/2, check/2, expire/1]).
%% Includes
-include("common.hrl").
-include("user.hrl").
-include("role.hrl").
-include("item.hrl").
-include("protocol.hrl").
%%%===================================================================
%%% API
%%%===================================================================
%% @doc load
-spec load(User :: #user{}) -> NewUser :: #user{}.
load(User = #user{role_id = RoleId}) ->
    List = parser:convert(item_sql:select(RoleId), ?MODULE),
    %% split diff type
    [{_, Item}, {_, Bag}, {_, Body}, {_, Store}  | _] = lists:keysort(1, classify(List)),
    User#user{item = Item, bag = Bag, body = Body, store = Store}.

%% @doc save
-spec save(User :: #user{}) -> NewUser :: #user{}.
save(User = #user{item = Items, bag = Bag, body = Body, store = Store}) ->
    NewItem = item_sql:insert_update(Items),
    NewBag = item_sql:insert_update(Bag),
    NewBody = item_sql:insert_update(Body),
    NewStore = item_sql:insert_update(Store),
    User#user{item = NewItem, bag = NewBag, body = NewBody, store = NewStore}.

%% @doc query item
-spec query_item(User :: #user{}) -> ok().
query_item(#user{item = Item}) ->
    {ok, [Item]}.

%% @doc query bag
-spec query_bag(User :: #user{}) -> ok().
query_bag(#user{bag = Bag}) ->
    {ok, [Bag]}.

%% @doc query body
-spec query_body(User :: #user{}) -> ok().
query_body(#user{body = Body}) ->
    {ok, [Body]}.

%% @doc query store
-spec query_store(User :: #user{}) -> ok().
query_store(#user{store = Store}) ->
    {ok, [Store]}.

%% @doc find
-spec find(User :: #user{}, UniqueId :: non_neg_integer(), Type :: neg_integer()) -> #item{}.
find(User, UniqueId, Type) ->
    listing:key_find(UniqueId, #item.unique_id, get_list_by_type(User, Type), #item{}).

%% @doc store
-spec store(User :: #user{}, Item :: #item{}) -> #user{}.
store(User, Item = #item{unique_id = UniqueId, type = Type}) ->
    NewList = lists:keystore(UniqueId, #item.unique_id, get_list_by_type(User, Type), Item),
    save_list_by_type(User, Type, NewList).

%% @doc item list
-spec get_list_by_type(#user{}, non_neg_integer()) -> list().
get_list_by_type(#user{item = Item}, ?ITEM_TYPE_COMMON) ->
    Item;
get_list_by_type(#user{bag = Bag}, ?ITEM_TYPE_BAG) ->
    Bag;
get_list_by_type(#user{body = Body}, ?ITEM_TYPE_BODY) ->
    Body;
get_list_by_type(#user{store = Store}, ?ITEM_TYPE_STORE) ->
    Store.

%% @doc item list
-spec save_list_by_type(#user{}, non_neg_integer(), list()) -> #user{}.
save_list_by_type(User, ?ITEM_TYPE_COMMON, Item) ->
    User#user{item = Item};
save_list_by_type(User, ?ITEM_TYPE_BAG, Bag) ->
    User#user{item = Bag};
save_list_by_type(User, ?ITEM_TYPE_BODY, Body) ->
    User#user{item = Body};
save_list_by_type(User, ?ITEM_TYPE_STORE, Store) ->
    User#user{item = Store}.

%% @doc limit size
-spec get_size_by_type(#user{}, non_neg_integer()) -> non_neg_integer().
get_size_by_type(#user{role = #role{item_size = ItemSize}}, ?ITEM_TYPE_COMMON) ->
    ItemSize;
get_size_by_type(#user{role = #role{bag_size = BagSize}}, ?ITEM_TYPE_BAG) ->
    BagSize;
get_size_by_type(#user{role = #role{store_size = StoreSize}}, ?ITEM_TYPE_STORE) ->
    StoreSize.

%% @doc limit size
-spec save_size_by_type(#user{}, non_neg_integer(), non_neg_integer()) -> #user{}.
save_size_by_type(User, ?ITEM_TYPE_COMMON, ItemSize) ->
    User#user{role = #role{item_size = ItemSize}};
save_size_by_type(User, ?ITEM_TYPE_BAG, BagSize) ->
    User#user{role = #role{bag_size = BagSize}};
save_size_by_type(User, ?ITEM_TYPE_STORE, StoreSize) ->
    User#user{role = #role{store_size = StoreSize}}.

%% @doc empty grid
-spec empty_grid(User :: #user{}, Type :: non_neg_integer()) ->non_neg_integer().
empty_grid(#user{role = #role{item_size = ItemSize}, item = Items}, ?ITEM_TYPE_COMMON) ->
    ItemSize - length(Items);
empty_grid(#user{role = #role{bag_size = BagSize}, bag = Bag}, ?ITEM_TYPE_BAG) ->
    BagSize - length(Bag);
empty_grid(#user{role = #role{store_size = StoreSize}, store = Store}, ?ITEM_TYPE_STORE) ->
    StoreSize - length(Store).

%% @doc classify
-spec classify(List :: list()) -> list().
classify(List) ->
    lists:foldl(fun(X = #item{type = Type}, Acc) -> listing:key_append(Type, Acc, X) end, [{X, []} || X <- ?ITEM_TYPE_LIST], List).

%% @doc classify
-spec data_classify(List :: [{non_neg_integer(), non_neg_integer(), non_neg_integer()}]) -> list().
data_classify(List) ->
    lists:foldl(fun({ItemId, Number, Bind}, Acc) -> listing:key_append((item_data:get(ItemId))#item_data.type, Acc, {ItemId, Number, Bind}) end, [{X, []} || X <- ?ITEM_TYPE_LIST], List).

%% @doc add item list
-spec add(User :: #user{}, List :: list(), From :: term()) -> {ok, NewUser :: #user{}} | {ok, NewUser :: #user{}}.
add(User, List, From) ->
    {NewUser, NewItemList, MailItem, IsHasAsset} = add_loop(User, List, From, time:ts(), [], [], false),
    case IsHasAsset of
        true ->
            asset:push(User);
        false ->
            skip
    end,
    case NewItemList of
        [_ | _] ->
            user_sender:send(User, ?PROTOCOL_ITEM, [NewItemList]);
        [] ->
            skip
    end,
    case MailItem of
        [_ | _] ->
            FinalUser = mail:add(NewUser, add_item_title, add_item_content, item, MailItem);
        [] ->
            FinalUser = NewUser
    end,
    {ok, FinalUser}.

%% add loop
add_loop(User, [], _, _, List, Mail, IsHasAsset) ->
    {User, List, Mail, IsHasAsset};
add_loop(User = #user{role_id = RoleId}, [H = {ItemId, Number} | T], From, Time, List, Mail, IsHasAsset) ->
    case item_data:get(ItemId) of
        #item_data{type = ?ITEM_TYPE_ASSET, asset = Asset} ->
            {ok, NewUser} = asset:add(User, [{Asset, Number}]),
            add_loop(NewUser, T, From, Time, List, Mail, true);
        #item_data{type = Type, overlap = Overlap = 1} ->
            ItemList = get_list_by_type(User, Type),
            ItemSize = get_size_by_type(User, Type),
            {NewList, NewMail, Update} = add_lap(RoleId, H, From, Time, Type, Overlap, ItemSize, [], ItemList, Mail, List),
            NewUser = User#user{item = NewList},
            add_loop(NewUser, T, From, Time, Update, NewMail, IsHasAsset);
        #item_data{type = Type, overlap = Overlap} ->
            ItemList = get_list_by_type(User, Type),
            ItemSize = get_size_by_type(User, Type),
            {NewList, NewMail, Update} = add_lap(RoleId, H, From, Time, Type, Overlap, ItemSize, ItemList, [], Mail, List),
            NewUser = User#user{item = NewList},
            add_loop(NewUser, T, From, Time, Update, NewMail, IsHasAsset);
        _ ->
            add_loop(User, T, From, Time, List, Mail, IsHasAsset)
    end.

%% add new item list
add_lap(RoleId, {ItemId, Number, Bind}, From, Time, Type, Overlap, Size, [], List, Mail, Update) ->
    case length(List) < Size of
        true ->
            case Number =< Overlap of
                true ->
                    Item = #item{role_id = RoleId, item_id = ItemId, number = Number, bind = Bind, type = Type},
                    UniqueId = item_sql:insert(Item),
                    NewItem = Item#item{unique_id = UniqueId},
                    %% log
                    log:item_produce_log(RoleId, ItemId, From, new, Time),
                    {[NewItem | List], Mail, [NewItem | Update]};
                false ->
                    %% capacity enough but produce multi item
                    Item = #item{role_id = RoleId, item_id = ItemId, number = Overlap, bind = Bind, type = Type},
                    UniqueId = item_sql:insert(Item),
                    NewItem = Item#item{unique_id = UniqueId},
                    %% log
                    log:item_produce_log(RoleId, ItemId, From, new, Time),
                    add_lap(RoleId, {ItemId, Number - Overlap, Bind}, From, Time, Type, Overlap, Size, [], [NewItem | List], Mail, [NewItem | Update])
            end;
        false ->
            %% capacity not enough add to mail
            {List, [{ItemId, Number, Bind} | Mail], Update}
    end;

%% find and lap to old item list
add_lap(RoleId, {ItemId, Number, Bind}, From, Time, Type, Overlap, Size, [#item{item_id = ItemId, number = OldNumber, bind = Bind} = H | T], List, Mail, Update) when Overlap > 1 ->
    case OldNumber + Number =< Overlap of
        true ->
            %% lap all to old
            NewItem = H#item{number = OldNumber + Number, flag = update},
            %% log
            log:item_produce_log(RoleId, ItemId, From, lap, Time),
            {lists:reverse([NewItem | T], List), Mail, [NewItem | Update]};
        _ ->
            %% lap to old and remain
            NewItem = H#item{number = Overlap, flag = update},
            %% log
            log:item_produce_log(RoleId, ItemId, From, lap, Time),
            add_lap(RoleId, {ItemId, Number - (Overlap - OldNumber), Bind}, From, Time, Type, Overlap, Size, T, [NewItem | List], Mail, [NewItem | Update])
    end;

add_lap(RoleId, {ItemId, Add, Bind}, From, Time, Type, Overlap, Size, [H | T], List, Mail, Update) ->
    add_lap(RoleId, {ItemId, Add, Bind}, From, Time, Type, Overlap, Size, T, [H | List], Mail, Update).

%% @doc reduce unique list
-spec reduce(User :: #user{}, List :: list(), From :: term()) -> ok() | error().
reduce(User = #user{role_id = RoleId}, List, From) ->
    case reduce_loop(List, User, [], [], false) of
        {ok, NewUser, Update, Delete, IsHasAsset} ->
            Now = time:ts(),
            case Update of
                [_ | _] ->
                    user_sender:send(NewUser, ?PROTOCOL_ITEM, [Update]),
                    [log:item_consume_log(RoleId, ItemId, reduce, From, Now) || #item{item_id = ItemId} <- Update];
                [] ->
                    skip
            end,
            case Delete of
                [_ | _] ->
                    user_sender:send(NewUser, ?PROTOCOL_ITEM_DELETE, [Delete]),
                    item_sql:delete_in_unique_id(listing:collect(#item.unique_id, Delete)),
                    [log:item_consume_log(RoleId, ItemId, reduce, From, Now) || #item{item_id = ItemId} <- Delete];
                [] ->
                    skip
            end,
            case IsHasAsset of
                true ->
                    asset:push(NewUser);
                false ->
                    skip
            end,
            {ok, NewUser};
        Error ->
            Error
    end.

reduce_loop([], User, Update, Delete, Asset) ->
    {ok, User, Update, Delete, Asset};
reduce_loop([{Asset, Number} | T], User, Update, Delete, _) when is_atom(Asset) ->
    case asset:cost(User, [{Asset, Number}]) of
        {ok, NewUser} ->
            reduce_loop(T, NewUser, Update, Delete, true);
        Error ->
            Error
    end;
reduce_loop([{Asset, Number, ?ITEM_TYPE_ASSET} | T], User, Update, Delete, _) ->
    case asset:cost(User, [{Asset, Number}]) of
        {ok, NewUser} ->
            reduce_loop(T, NewUser, Update, Delete, true);
        Error ->
            Error
    end;
reduce_loop([{UniqueId, Number, Type} | T], User, Update, Delete, IsHasAsset) ->
    List = get_list_by_type(User, Type),
    case lists:keyfind(UniqueId, #item.unique_id, List) of
        Item = #item{number = THisNumber} when Number < THisNumber ->
            NewItem = Item#item{number = THisNumber - Number},
            NewList = lists:keyreplace(UniqueId, #item.unique_id, List, NewItem),
            NewUser = save_list_by_type(User, Type, NewList),
            reduce_loop(T, NewUser, [NewItem | Update], Delete, IsHasAsset);
        Item = #item{number = Number} ->
            NewItem = Item#item{number = 0},
            NewList = lists:keydelete(UniqueId, #item.unique_id, List),
            NewUser = save_list_by_type(User, Type, NewList),
            reduce_loop(T, NewUser, Update, [NewItem | Delete], IsHasAsset);
        _ ->
            {error, 0}
    end.

%% @doc validate list by unique id
%% attention !!! merge list is need
-spec validate(User :: #user{}, [{UniqueId :: non_neg_integer(), Number :: non_neg_integer(), Type :: non_neg_integer()}]) -> ok() | error().
validate(User, List) ->
    validate_loop(List, User).

validate_loop([], _) ->
    {ok, 1};
validate_loop([{Asset, Number} | T], User) when is_atom(Asset) ->
    case asset:check(User, [{Asset, Number}]) of
        ok ->
            validate_loop(T, User);
        Error ->
            Error
    end;
validate_loop([{Asset, Number, ?ITEM_TYPE_ASSET} | T], User) ->
    case asset:check(User, [{Asset, Number}]) of
        ok ->
            validate_loop(T, User);
        Error ->
            Error
    end;
validate_loop([{UniqueId, Number, Type} | T], User) ->
    List = get_list_by_type(User, Type),
    case lists:keyfind(UniqueId, #item.unique_id, List) of
        #item{number = ThisNumber} when Number =< ThisNumber ->
            validate_loop(T, User);
        _ ->
            {error, 0}
    end.

%% @doc check list by item id
%% attention !!! merge list is need
-spec check(User :: #user{}, [{ItemId :: non_neg_integer(), Number :: non_neg_integer()}]) -> ok() | error().
check(User, List) ->
    check_loop(List, User, []).

check_loop([], _, Result) ->
    {ok, Result};
check_loop([H = {ItemId, NeedNumber} | T], User, Result) ->
    case item_data:get(ItemId) of
        #item_data{type = ?ITEM_TYPE_ASSET, asset = Asset} ->
            case asset:check(User, [{Asset, NeedNumber}]) of
                ok ->
                    check_loop(T, User, [{Asset, NeedNumber} | Result]);
                Error ->
                    Error
            end;
        #item_data{type = Type} ->
            List = get_list_by_type(User, Type),
            case check_one_loop(List, H, Result) of
                {ok, NewResult} ->
                    check_loop(T, User, NewResult);
                Error ->
                    Error
            end;
        _ ->
            {error, 1}
    end.

check_one_loop([], _, _) ->
    %% no enough item
    {error, 0};
check_one_loop([#item{unique_id = UniqueId, item_id = ItemId, number = Number, type = Type} | _], {ItemId, NeedNumber}, Result) when NeedNumber =< Number->
    %% enough
    {ok, [{UniqueId, NeedNumber, Type} | Result]};
check_one_loop([#item{unique_id = UniqueId, item_id = ItemId, number = Number, type = Type} | T], {ItemId, NeedNumber}, Result) when Number < NeedNumber->
    %% not enough
    check_one_loop(T, {ItemId, NeedNumber - Number}, [{UniqueId, Number, Type} | Result]);
check_one_loop([_ | T], {ItemId, NeedNumber}, Result) ->
    %% not need item
    check_one_loop(T, {ItemId, NeedNumber}, Result).

%% @doc expire
-spec expire(#user{}) -> #user{}.
expire(User = #user{item = Item, bag = Bag, body = Body}) ->
    Now = time:ts(),
    {NewItem, DeleteItem} = expire_loop(Item, Now, [], []),
    {NewBag, DeleteBag} = expire_loop(Bag, Now, [], DeleteItem),
    {NewBody, DeleteBody} = expire_loop(Body, Now, [], DeleteBag),
    item_sql:delete_in_unique_id(listing:collect(#item.unique_id, DeleteBody)),
    user_sender:send(User, ?PROTOCOL_ITEM_DELETE, [DeleteBody]),
    User#user{item = NewItem, bag = NewBag, body = NewBody}.

expire_loop([], _, List, Delete) ->
    {List, Delete};
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

