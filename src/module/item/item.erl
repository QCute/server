%%%-------------------------------------------------------------------
%%% @doc
%%% module item base, manager item bag
%%% @end
%%%-------------------------------------------------------------------
-module(item).
%% API
-export([load/1, save/1]).
-export([push_item/1, push_bag/1, push_store/1]).
-export([add/3, add/4]).
-export([reduce/2, validate/2, check/2]).
-export([classify/1, data_classify/1]).
-export([empty_grid/2]).
%% Includes
-include("common.hrl").
-include("user.hrl").
-include("role.hrl").
-include("item.hrl").
-include("protocol.hrl").
%%%===================================================================
%%% API
%%%===================================================================
%% @doc load user items
-spec load(User :: #user{}) -> NewUser :: #user{}.
load(User = #user{role_id = RoleId}) ->
    List = parser:convert(item_sql:select(RoleId), ?MODULE),
    %% split diff type
    [Items, Bag, Store | _] = classify(List),
    User#user{item = Items, bag = Bag, store = Store}.

%% @doc save user items
-spec save(User :: #user{}) -> NewUser :: #user{}.
save(User = #user{item = Items, bag = Bag, store = Store}) ->
    NewItem = item_sql:insert_update(Items),
    NewBag = item_sql:insert_update(Bag),
    NewStore = item_sql:insert_update(Store),
    User#user{item = NewItem, bag = NewBag, store = NewStore}.

%% @doc query item
-spec push_item(User :: #user{}) -> ok().
push_item(#user{item = Item}) ->
    {ok, [Item]}.

%% @doc query bag
-spec push_bag(User :: #user{}) -> ok().
push_bag(#user{bag = Bag}) ->
    {ok, [Bag]}.

%% @doc query store
-spec push_store(User :: #user{}) -> ok().
push_store(#user{store = Store}) ->
    {ok, [Store]}.

%% @doc classify
-spec classify(List :: [#item{}]) -> list().
classify(List) ->
    F = fun
        (X = #item{type = ?ITEM_TYPE_COMMON},    [I, B, S]) -> [[X | I], B, S];
        (X = #item{type = ?ITEM_TYPE_EQUIPMENT}, [I, B, S]) -> [I, [X | B], S];
        (X = #item{type = ?ITEM_TYPE_STORE},     [I, B, S]) -> [I, B, [X | S]]
    end,
    lists:foldl(F, [[], [], []], List).

%% @doc classify
-spec data_classify(List :: [{non_neg_integer(), non_neg_integer(), non_neg_integer()}]) -> list().
data_classify(List) ->
    F = fun({ItemId, Amount, Bind}, [I, B]) ->
            case item_data:get(ItemId) of
                #item_data{type = ?ITEM_TYPE_COMMON} ->
                    [[{ItemId, Amount, Bind} | I], B];
                #item_data{type = ?ITEM_TYPE_EQUIPMENT} ->
                    [I, [{ItemId, Amount, Bind} | B]];
                _ ->
                    [I, B]
            end
        end,
    lists:foldl(F, [[], [], []], List).

%% @doc add item list
-spec add(User :: #user{}, List :: list(), From :: term()) -> {ok, NewUser :: #user{}}.
add(User, List, From) ->
    add(User, List, From, query).

%% @doc add item list
-spec add(User :: #user{}, List :: list(), From :: term(), Push :: query | keep) -> {ok, NewUser :: #user{}} | {ok, NewUser :: #user{}, Binary :: binary()}.
add(User, List, From, query) ->
    {ok, NewUser, Binary} = do_add(User, From, List),
    user_sender:send(NewUser, Binary),
    {ok, NewUser};
add(User, List, From, _) ->
    do_add(User, From, List).

%% do add
do_add(User, List, From) ->
    {NewUser, NewList, MailItem, Assets} = add_loop(User, List, From, time:ts(), [], [], []),
    case NewList of
        [] ->
            NewListBinary = <<>>;
        _ ->
            {ok, NewListBinary} = user_router:write(?PROTOCOL_ITEM, [NewList])
    end,
    case Assets of
        [] ->
            AssetsBinary = <<>>;
        _ ->
            {ok, AssetsBinary} = user_router:write(?PROTOCOL_ASSET, [NewUser#user.asset])
    end,
    case MailItem of
        [] ->
            FinalUser = NewUser;
        _ ->
            FinalUser = mail:add(NewUser, add_item_title, add_item_content, item, MailItem)
    end,
    {ok, FinalUser, <<NewListBinary/binary, AssetsBinary/binary>>}.

%% add loop
add_loop(User, [], _, _, List, Mail, Assets) ->
    {User, List, Mail, Assets};
add_loop(User = #user{role_id = RoleId, role = #role{item_size = ItemSize, bag_size = BagSize}, item = ItemList, bag = BagList}, [H = {ItemId, Amount} | T], From, Time, List, Mail, Assets) ->
    case item_data:get(ItemId) of
        #item_data{type = Type = ?ITEM_TYPE_COMMON, overlap = Overlap = 1} ->
            {NewList, NewMail, Update} = add_lap(RoleId, H, From, Time, Type, Overlap, ItemSize, [], ItemList, Mail, List),
            NewUser = User#user{item = NewList},
            add_loop(NewUser, T, From, Time, Update, NewMail, Assets);
        #item_data{type = Type = ?ITEM_TYPE_COMMON, overlap = Overlap} ->
            {NewList, NewMail, Update} = add_lap(RoleId, H, From, Time, Type, Overlap, ItemSize, ItemList, [], Mail, List),
            NewUser = User#user{item = NewList},
            add_loop(NewUser, T, From, Time, Update, NewMail, Assets);
        #item_data{type = Type = ?ITEM_TYPE_EQUIPMENT, overlap = 1} ->
            {NewList, NewMail, Update} = add_lap(RoleId, H, From, Time, Type, 1, BagSize, [], BagList, Mail, List),
            NewUser = User#user{bag = NewList},
            add_loop(NewUser, T, From, Time, Update, NewMail, Assets);
        #item_data{type = ?ITEM_TYPE_GOLD} ->
            Asset = {gold, Amount},
            {ok, NewUser} = asset:add(User, [Asset]),
            add_loop(NewUser, T, From, Time, List, Mail, [Asset | Assets]);
        #item_data{type = ?ITEM_TYPE_SLIVER} ->
            Asset = {silver, Amount},
            {ok, NewUser} = asset:add(User, [Asset]),
            add_loop(NewUser, T, From, Time, List, Mail, [Asset | Assets]);
        #item_data{type = ?ITEM_TYPE_COPPER} ->
            Asset = {copper, Amount},
            {ok, NewUser} = asset:add(User, [Asset]),
            add_loop(NewUser, T, From, Time, List, Mail, [Asset | Assets]);
        #item_data{type = ?ITEM_TYPE_COIN} ->
            Asset = {coin, Amount},
            {ok, NewUser} = asset:add(User, [Asset]),
            add_loop(NewUser, T, From, Time, List, Mail, [Asset | Assets]);
        #item_data{type = ?ITEM_TYPE_EXP} ->
            Asset = {exp, Amount},
            {ok, NewUser} = asset:add(User, [Asset]),
            add_loop(NewUser, T, From, Time, List, Mail, [Asset | Assets]);
        _ ->
            add_loop(User, T, From, Time, List, Mail, Assets)
    end.

%% add new item list
add_lap(RoleId, {ItemId, Amount, Bind}, From, Time, Type, Overlap, Size, [], List, Mail, Update) ->
    case length(List) < Size of
        true ->
            case Amount =< Overlap of
                true ->
                    Item = #item{role_id = RoleId, item_id = ItemId, amount = Amount, bind = Bind, type = Type},
                    UniqueId = item_sql:insert(Item),
                    NewItem = Item#item{unique_id = UniqueId},
                    %% log
                    log:item_produce_log(RoleId, ItemId, From, new, Time),
                    {[NewItem | List], Mail, [NewItem | Update]};
                false ->
                    %% capacity enough but produce multi item
                    Item = #item{role_id = RoleId, item_id = ItemId, amount = Overlap, bind = Bind, type = Type},
                    UniqueId = item_sql:insert(Item),
                    NewItem = Item#item{unique_id = UniqueId},
                    %% log
                    log:item_produce_log(RoleId, ItemId, From, new, Time),
                    add_lap(RoleId, {ItemId, Amount - Overlap, Bind}, From, Time, Type, Overlap, Size, [], [NewItem | List], Mail, [NewItem | Update])
            end;
        false ->
            %% capacity not enough add to mail
            {List, [{ItemId, Amount, Bind} | Mail], Update}
    end;

%% find and lap to old item list
add_lap(RoleId, {ItemId, Amount, Bind}, From, Time, Type, Overlap, Size, [#item{item_id = ItemId, amount = OldAmount, bind = Bind} = H | T], List, Mail, Update) when Overlap > 1 ->
    case OldAmount + Amount =< Overlap of
        true ->
            %% lap all to old
            NewItem = H#item{amount = OldAmount + Amount, flag = update},
            %% log
            log:item_produce_log(RoleId, ItemId, From, lap, Time),
            {lists:reverse([NewItem | T], List), Mail, [NewItem | Update]};
        _ ->
            %% lap to old and remain
            NewItem = H#item{amount = Overlap, flag = update},
            %% log
            log:item_produce_log(RoleId, ItemId, From, lap, Time),
            add_lap(RoleId, {ItemId, Amount - (Overlap - OldAmount), Bind}, From, Time, Type, Overlap, Size, T, [NewItem | List], Mail, [NewItem | Update])
    end;

add_lap(RoleId, {ItemId, Add, Bind}, From, Time, Type, Overlap, Size, [H | T], List, Mail, Update) ->
    add_lap(RoleId, {ItemId, Add, Bind}, From, Time, Type, Overlap, Size, T, [H | List], Mail, Update).

%% @doc reduce unique list
-spec reduce(User :: #user{}, [{UniqueId :: non_neg_integer(), Amount :: non_neg_integer(), Type :: non_neg_integer()}]) -> ok() | error().
reduce(User, List) ->
    case reduce_loop(List, User, [], [], []) of
        {ok, NewUser, Update, Delete, Assets} ->
            user_sender:send(NewUser, ?PROTOCOL_ITEM, Update),
            user_sender:send(NewUser, ?PROTOCOL_ITEM_DELETE, Delete),
            item_sql:delete_in_unique_id(listing:collect(#item.unique_id, Delete)),
            case Assets of
                [] ->
                    ok;
                _ ->
                    user_sender:send(?PROTOCOL_ASSET, [NewUser#user.asset])
            end,
            {ok, NewUser};
        Error ->
            Error
    end.

reduce_loop([], User, Update, Delete, Asset) ->
    {ok, User, Update, Delete, Asset};
reduce_loop([{UniqueId, Amount, ?ITEM_TYPE_COMMON} | T], User = #user{item = ItemList}, Update, Delete, Asset) ->
    case lists:keyfind(UniqueId, #item.unique_id, ItemList) of
        Item = #item{amount = THisAmount} when Amount < THisAmount ->
            NewList = lists:keydelete(UniqueId, #item.unique_id, ItemList),
            reduce_loop(T, User#user{item = NewList}, [Item | Update], Delete, Asset);
        #item{amount = Amount} ->
            NewList = lists:keydelete(UniqueId, #item.unique_id, ItemList),
            reduce_loop(T, User#user{item = NewList}, Update, [{UniqueId, ?ITEM_TYPE_COMMON} | Delete], Asset);
        _ ->
            {error, 0}
    end;
reduce_loop([{UniqueId, Amount, ?ITEM_TYPE_EQUIPMENT} | T], User = #user{bag = BagList}, Update, Delete, Asset) ->
    case lists:keyfind(UniqueId, #item.unique_id, BagList) of
        Item = #item{amount = THisAmount} when Amount < THisAmount ->
            NewList = lists:keydelete(UniqueId, #item.unique_id, BagList),
            reduce_loop(T, User#user{item = NewList}, [Item | Update], Delete, Asset);
        #item{amount = Amount} ->
            NewList = lists:keydelete(UniqueId, #item.unique_id, BagList),
            reduce_loop(T, User#user{item = NewList}, Update, [{UniqueId, ?ITEM_TYPE_EQUIPMENT} | Delete], Asset);
        _ ->
            {error, 0}
    end;
reduce_loop([{gold, Amount, ?ITEM_TYPE_GOLD} | T], User, Update, Delete, Asset) ->
    Add = {gold, Amount},
    case asset:cost(User, [Add]) of
        {ok, NewUser} ->
            reduce_loop(T, NewUser, Update, Delete, [Add | Asset]);
        Error ->
            Error
    end;
reduce_loop([{sliver, Amount, ?ITEM_TYPE_SLIVER} | T], User, Update, Delete, Asset) ->
    Add = {sliver, Amount},
    case asset:cost(User, [Add]) of
        {ok, NewUser} ->
            reduce_loop(T, NewUser, Update, Delete, [Add | Asset]);
        Error ->
            Error
    end;
reduce_loop([{copper, Amount, ?ITEM_TYPE_COPPER} | T], User, Update, Delete, Asset) ->
    Add = {copper, Amount},
    case asset:cost(User, [Add]) of
        {ok, NewUser} ->
            reduce_loop(T, NewUser, Update, Delete, [Add | Asset]);
        Error ->
            Error
    end.

%% @doc validate list by unique id
%% attention !!! merge list is need
-spec validate(User :: #user{}, [{UniqueId :: non_neg_integer(), Amount :: non_neg_integer(), Type :: non_neg_integer()}]) -> ok() | error().
validate(User, List) ->
    validate_loop(List, User).

validate_loop([], _) ->
    {ok, 1};
validate_loop([{UniqueId, Amount, ?ITEM_TYPE_COMMON} | T], User = #user{item = ItemList}) ->
    case lists:keyfind(UniqueId, #item.unique_id, ItemList) of
        #item{amount = ThisAmount} when Amount =< ThisAmount ->
            validate_loop(T, User);
        _ ->
            {error, 0}
    end;
validate_loop([{UniqueId, Amount, ?ITEM_TYPE_EQUIPMENT} | T], User = #user{bag = BagList}) ->
    case lists:keyfind(UniqueId, #item.unique_id, BagList) of
        #item{amount = ThisAmount} when Amount =< ThisAmount ->
            validate_loop(T, User);
        _ ->
            {error, 0}
    end;
validate_loop([{gold, Amount, ?ITEM_TYPE_GOLD} | T], User) ->
    Add = {gold, Amount},
    case asset:cost(User, [Add]) of
        ok ->
            validate_loop(T, User);
        Error ->
            Error
    end;
validate_loop([{sliver, Amount, ?ITEM_TYPE_SLIVER} | T], User) ->
    Add = {sliver, Amount},
    case asset:check(User, [Add]) of
        ok ->
            validate_loop(T, User);
        Error ->
            Error
    end;
validate_loop([{copper, Amount, ?ITEM_TYPE_COPPER} | T], User) ->
    Add = {copper, Amount},
    case asset:check(User, [Add]) of
        ok ->
            validate_loop(T, User);
        Error ->
            Error
    end.

%% @doc check list by item id
%% attention !!! merge list is need
-spec check(User :: #user{}, [{ItemId :: non_neg_integer(), Amount :: non_neg_integer()}]) -> ok() | error().
check(User, List) ->
    check_loop(List, User, []).

check_loop([], _, Result) ->
    {ok, Result};
check_loop([H = {ItemId, NeedAmount} | T], User = #user{item = ItemList, bag = BagList}, Result) ->
    case item_data:get(ItemId) of
        #item_data{type = ?ITEM_TYPE_COMMON} ->
            case check_one_loop(ItemList, H, Result) of
                {ok, NewResult} ->
                    check_loop(T, User, NewResult);
                Error ->
                    Error
            end;
        #item_data{type = ?ITEM_TYPE_EQUIPMENT} ->
            case check_one_loop(BagList, H, Result) of
                {ok, NewResult} ->
                    check_loop(T, User, NewResult);
                Error ->
                    Error
            end;
        #item_data{type = ?ITEM_TYPE_GOLD} ->
            Asset = {gold, NeedAmount},
            case asset:check(User, [Asset]) of
                ok ->
                    check_loop(T, User, [Asset | Result]);
                Error ->
                    Error
            end;
        #item_data{type = ?ITEM_TYPE_SLIVER} ->
            Asset = {sliver, NeedAmount},
            case asset:check(User, [Asset]) of
                ok ->
                    check_loop(T, User, [Asset | Result]);
                Error ->
                    Error
            end;
        #item_data{type = ?ITEM_TYPE_COPPER} ->
            Asset = {copper, NeedAmount},
            case asset:check(User, [Asset]) of
                ok ->
                    check_loop(T, User, [Asset | Result]);
                Error ->
                    Error
            end;
        _ ->
            {error, 1}
    end.

check_one_loop([], _, _) ->
    %% no enough item
    {error, 0};
check_one_loop([#item{unique_id = UniqueId, item_id = ItemId, amount = Amount, type = Type} | _], {ItemId, NeedAmount}, Result) when NeedAmount =< Amount->
    %% enough
    {ok, [{UniqueId, NeedAmount, Type} | Result]};
check_one_loop([#item{unique_id = UniqueId, item_id = ItemId, amount = Amount, type = Type} | T], {ItemId, NeedAmount}, Result) when Amount < NeedAmount->
    %% not enough
    check_one_loop(T, {ItemId, NeedAmount - Amount}, [{UniqueId, Amount, Type} | Result]);
check_one_loop([_ | T], {ItemId, NeedAmount}, Result) ->
    %% not need item
    check_one_loop(T, {ItemId, NeedAmount}, Result).


%% @doc empty grid
-spec empty_grid(User :: #user{}, Type :: non_neg_integer()) ->non_neg_integer().
empty_grid(#user{role = #role{item_size = ItemSize}, item = Items}, ?ITEM_TYPE_COMMON) ->
    ItemSize - length(Items);
empty_grid(#user{role = #role{bag_size = BagSize}, bag = Bag}, ?ITEM_TYPE_EQUIPMENT) ->
    BagSize - length(Bag);
empty_grid(#user{role = #role{store_size = StoreSize}, store = Store}, ?ITEM_TYPE_STORE) ->
    StoreSize - length(Store).
%%%===================================================================
%%% Internal functions
%%%===================================================================

