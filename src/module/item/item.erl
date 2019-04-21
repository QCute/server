%%%-------------------------------------------------------------------
%%% @doc
%%% module item base, manager item bag
%%% @end
%%%-------------------------------------------------------------------
-module(item).
%% API
-export([load/1, save/1]).
-export([add/2, add/3]).
-export([empty_grid/2]).
-include("common.hrl").
-include("player.hrl").
-include("item.hrl").
-include("protocol.hrl").
%%%===================================================================
%%% API
%%%===================================================================
%% @doc load user items
-spec load(User :: #user{}) -> NewUser :: #user{}.
load(User = #user{id = UserId}) ->
    Data = item_sql:select(UserId),
    List = data_tool:load(Data, item),
    %% split diff type
    {Items, Bag, Store} = classify(List),
    User#user{item = Items, bag = Bag, store = Store}.

%% @doc save user items
-spec save(User :: #user{}) -> NewUser :: #user{}.
save(User = #user{item = Items, bag = Bag, store = Store}) ->
    NewItem = item_sql:update_into(Items),
    NewBag = item_sql:update_into(Bag),
    NewStore = item_sql:update_into(Store),
    User#user{item = NewItem, bag = NewBag, store = NewStore}.

%% @doc classify
-spec classify(List :: [#item{}]) -> {list(), list(), list()}.
classify(List) ->
    F = fun
        (X = #item{type = ?ITEM_TYPE_COMMON}, [I, B, S]) -> [[X | I], B, S];
        (X = #item{type = ?ITEM_TYPE_EQUIPMENT}, [I, B, S]) -> [I, [X | B], S];
        (X = #item{type = ?ITEM_TYPE_STORE}, [I, B, S]) -> [I, B, [X | S]]
    end,
    lists:foldl(F, {[], [], []}, List).

%% @doc add item list
-spec add(User :: #user{}, List :: list()) -> {ok, NewUser :: #user{}}.
add(User, List) ->
    add(User, List, push).

%% @doc add item list
-spec add(User :: #user{}, List :: list(), Push :: push | keep) -> {ok, NewUser :: #user{}} | {ok, NewUser :: #user{}, Binary :: binary()}.
add(User, List, push) ->
    {ok, NewUser, Binary} = do_add(User, List),
    player_sender:send(NewUser, Binary),
    {ok, NewUser};
add(User, List, _) ->
    do_add(User, List).

%% do add
do_add(User, List) ->
    {NewUser, NewList, MailItem, Assets} = add_loop(User, List, [], [], []),
    case NewList of
        [] ->
            NewListBinary = <<>>;
        _ ->
            {ok, NewListBinary} = player_route:write(?CMD_ITEM, [NewList])
    end,
    case Assets of
        [] ->
            AssetsBinary = <<>>;
        _ ->
            {ok, AssetsBinary} = player_route:write(?CMD_PLAYER_ASSETS, [User])
    end,
    case MailItem of
        [] ->
            FinalUser = NewUser;
        _ ->
            FinalUser = mail:add(NewUser, data_text:get(add_item_title), data_text:get(add_item_content), item, MailItem)
    end,
    {ok, FinalUser, <<NewListBinary/binary, AssetsBinary/binary>>}.

%% add loop
add_loop(User, [], List, Mail, Assets) ->
    {User, List, Mail, Assets};
add_loop(User = #user{id = UserId, player = #player{item_size = ItemSize, bag_size = BagSize}, item = ItemList, bag = BagList}, [{DataId, Amount, Bind} = H | T], List, Mail, Assets) ->
    case data_item:get(DataId) of
        #data_item{type = Type = ?ITEM_TYPE_COMMON, overlap = Overlap} ->
            {NewList, NewMail, Update} = add_lap(UserId, H, Type, Overlap, ItemSize, ItemList, [], Mail, List),
            NewUser = User#user{item = NewList},
            add_loop(NewUser, T, Update, NewMail, Assets);
        #data_item{type = Type = ?ITEM_TYPE_EQUIPMENT, overlap = Overlap} ->
            {NewList, NewMail, Update} = add_lap(UserId, H, Type, Overlap, BagSize, BagList, [], Mail, List),
            NewUser = User#user{bag = NewList},
            add_loop(NewUser, T, Update, NewMail, Assets);
        #data_item{type = 11} ->
            Add = {gold, Amount, Bind},
            {ok, NewUser} = player_assets:add(User, [Add]),
            add_loop(NewUser, T, List, Mail, [Add | Assets]);
        #data_item{type = 12} ->
            Add = {silver, Amount, Bind},
            {ok, NewUser} = player_assets:add(User, [Add]),
            add_loop(NewUser, T, List, Mail, [Add | Assets]);
        #data_item{type = 13} ->
            Add = {copper, Amount, Bind},
            {ok, NewUser} = player_assets:add(User, [Add]),
            add_loop(NewUser, T, List, Mail, [Add | Assets]);
        _ ->
            add_loop(User, T, List, Mail, Assets)
    end.

%% max overlap 1 add directly
add_lap(UserId, {DataId, Amount, Bind}, Type, 1, Size, List, [], Mail, Update) ->
    add_lap(UserId, {DataId, Amount, Bind}, Type, 1, Size, [], List, Mail, Update);

%% add new item list
add_lap(UserId, {DataId, Amount, Bind}, Type, Overlap, Size, [], List, Mail, Update) ->
    case length(List) < Size of
        true ->
            case Amount =< Overlap of
                true ->
                    Item = #item{user_id = UserId, data_id = DataId, amount = Amount, bind = Bind, type = Type},
                    Id = item_sql:insert(Item),
                    NewItem = Item#item{id = Id},
                    {[NewItem | List], Mail, [NewItem | Update]};
                false ->
                    %% capacity enough but produce multi item
                    Item = #item{user_id = UserId, data_id = DataId, amount = Overlap, bind = Bind, type = Type},
                    Id = item_sql:insert(Item),
                    NewItem = Item#item{id = Id},
                    add_lap(UserId, {DataId, Amount - Overlap, Bind}, Type, Overlap, Size, [], [NewItem | List], Mail, [NewItem | Update])
            end;
        false ->
            %% capacity not enough add to mail
            {List, [{DataId, Amount, Bind} | Mail], Update}
    end;

%% find and lap to old item list
add_lap(UserId, {DataId, Amount, Bind}, Type, Overlap, Size, [#item{data_id = DataId, amount = OldAmount, bind = Bind} = H | T], List, Mail, Update) when Overlap > 1 ->
    case OldAmount + Amount =< Overlap of
        true ->
            %% lap all to old
            NewItem = H#item{amount = OldAmount + Amount, flag = update},
            {merge([NewItem | T], List), Mail, [NewItem | Update]};
        _ ->
            %% lap to old and remain
            NewItem = H#item{amount = Overlap, flag = update},
            add_lap(UserId, {DataId, Amount - (Overlap - OldAmount), Bind}, Type, Overlap, Size, T, [NewItem | List], Mail, [NewItem | Update])
    end;

add_lap(UserId, {DataId, Add, Bind}, Type, Overlap, Size, [H | T], List, Mail, Update) ->
    add_lap(UserId, {DataId, Add, Bind}, Type, Overlap, Size, T, [H | List], Mail, Update).

%% merge two list
merge([], List) ->
    List;
merge([H | T], List) ->
    merge(T, [H | List]).


%% @doc empty grid
-spec empty_grid(User :: #user{}, Type :: ok) ->non_neg_integer().
empty_grid(#user{player = #player{item_size = ItemSize}, item = Items}, ?ITEM_TYPE_COMMON) ->
    ItemSize - length(Items);
empty_grid(#user{player = #player{bag_size = BagSize}, bag = Bag}, ?ITEM_TYPE_EQUIPMENT) ->
    BagSize - length(Bag);
empty_grid(#user{player = #player{store_size = StoreSize}, store = Store}, ?ITEM_TYPE_STORE) ->
    StoreSize - length(Store).
%%%===================================================================
%%% Internal functions
%%%===================================================================

