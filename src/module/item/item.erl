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
    NewItem = item_sql:update_into(Items),
    NewBag = item_sql:update_into(Bag),
    NewStore = item_sql:update_into(Store),
    User#user{item = NewItem, bag = NewBag, store = NewStore}.

%% @doc push item
-spec push_item(User :: #user{}) -> {reply, list()}.
push_item(#user{item = Item}) ->
    {reply, [Item]}.

%% @doc push bag
-spec push_bag(User :: #user{}) -> {reply, list()}.
push_bag(#user{bag = Bag}) ->
    {reply, [Bag]}.

%% @doc push store
-spec push_store(User :: #user{}) -> {reply, list()}.
push_store(#user{store = Store}) ->
    {reply, [Store]}.

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
    add(User, List, From, push).

%% @doc add item list
-spec add(User :: #user{}, List :: list(), From :: term(), Push :: push | keep) -> {ok, NewUser :: #user{}} | {ok, NewUser :: #user{}, Binary :: binary()}.
add(User, List, From, push) ->
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
add_loop(User = #user{role_id = RoleId, role = #role{item_size = ItemSize, bag_size = BagSize}, item = ItemList, bag = BagList}, [{ItemId, Amount, Bind} = H | T], From, Time, List, Mail, Assets) ->
    case item_data:get(ItemId) of
        #item_data{type = Type = ?ITEM_TYPE_COMMON, overlap = 1} ->
            {NewList, NewMail, Update} = add_lap(RoleId, H, From, Time, Type, 1, ItemSize, [], ItemList, Mail, List),
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
        #item_data{type = Type = ?ITEM_TYPE_EQUIPMENT, overlap = Overlap} ->
            {NewList, NewMail, Update} = add_lap(RoleId, H, From, Time, Type, Overlap, BagSize, BagList, [], Mail, List),
            NewUser = User#user{bag = NewList},
            add_loop(NewUser, T, From, Time, Update, NewMail, Assets);
        #item_data{type = 11} ->
            Add = {gold, Amount, Bind},
            {ok, NewUser} = asset:add(User, [Add]),
            add_loop(NewUser, T, From, Time, List, Mail, [Add | Assets]);
        #item_data{type = 12} ->
            Add = {silver, Amount, Bind},
            {ok, NewUser} = asset:add(User, [Add]),
            add_loop(NewUser, T, From, Time, List, Mail, [Add | Assets]);
        #item_data{type = 13} ->
            Add = {copper, Amount, Bind},
            {ok, NewUser} = asset:add(User, [Add]),
            add_loop(NewUser, T, From, Time, List, Mail, [Add | Assets]);
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
            {merge([NewItem | T], List), Mail, [NewItem | Update]};
        _ ->
            %% lap to old and remain
            NewItem = H#item{amount = Overlap, flag = update},
            %% log
            log:item_produce_log(RoleId, ItemId, From, lap, Time),
            add_lap(RoleId, {ItemId, Amount - (Overlap - OldAmount), Bind}, From, Time, Type, Overlap, Size, T, [NewItem | List], Mail, [NewItem | Update])
    end;

add_lap(RoleId, {ItemId, Add, Bind}, From, Time, Type, Overlap, Size, [H | T], List, Mail, Update) ->
    add_lap(RoleId, {ItemId, Add, Bind}, From, Time, Type, Overlap, Size, T, [H | List], Mail, Update).

%% merge two list
merge([], List) ->
    List;
merge([H | T], List) ->
    merge(T, [H | List]).


%% @doc empty grid
-spec empty_grid(User :: #user{}, Type :: ok) ->non_neg_integer().
empty_grid(#user{role = #role{item_size = ItemSize}, item = Items}, ?ITEM_TYPE_COMMON) ->
    ItemSize - length(Items);
empty_grid(#user{role = #role{bag_size = BagSize}, bag = Bag}, ?ITEM_TYPE_EQUIPMENT) ->
    BagSize - length(Bag);
empty_grid(#user{role = #role{store_size = StoreSize}, store = Store}, ?ITEM_TYPE_STORE) ->
    StoreSize - length(Store).
%%%===================================================================
%%% Internal functions
%%%===================================================================

