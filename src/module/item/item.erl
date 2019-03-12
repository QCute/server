%%%-------------------------------------------------------------------
%%% @doc
%%% module item base, manager item bag
%%% @end
%%%-------------------------------------------------------------------
-module(item).
%% API
-export([load/1, save/1]).
-export([add/2, add/3]).
-include("common.hrl").
-include("player.hrl").
-include("item.hrl").
%%%===================================================================
%%% API
%%%===================================================================
%% @doc load user items
-spec load(User :: #user{}) -> NewUser :: #user{}.
load(User = #user{id = UserId}) ->
    Data = item_sql:select(UserId),
    Items = data_tool:load(Data, item),
    User#user{item = Items}.

%% @doc save user items
-spec save(User :: #user{}) -> NewUser :: #user{}.
save(User = #user{item = Item}) ->
    NewItem = item_sql:update_into(Item),
    User#user{item = NewItem}.

%% @doc add item list
-spec add(User :: #user{}, List :: list()) -> {ok, NewUser :: #user{}}.
add(User, List) ->
    add(User, List, push).

%% @doc add item list
-spec add(User :: #user{}, List :: list(), Push :: push | no_push) -> {ok, NewUser :: #user{}} | {ok, NewUser :: #user{}, Binary :: binary()}.
add(User, List, push) ->
    {ok, NewUser, Binary} = do_add(User, List),
    player_sender:send(NewUser, Binary),
    {ok, NewUser};
add(User, List, _) ->
    do_add(User, List).

%% do add
do_add(User, List) ->
    {NewUser, NewList, Assets} = add_loop(User, List, [], []),
    case NewList of
        [] ->
            NewListBinary = <<>>;
        _ ->
            {ok, NewListBinary} = player_route:write(897456, [NewList])
    end,
    case Assets of
        [] ->
            AssetsBinary = <<>>;
        _ ->
            {ok, AssetsBinary} = player_route:write(899825, [User])
    end,
    {ok, NewUser, <<NewListBinary/binary, AssetsBinary/binary>>}.

%% add loop
add_loop(User, [], List, Assets) ->
    {User, List, Assets};
add_loop(User = #user{id = UserId, item = ItemList, bag = BagList, store = StoreList}, [{DataId, Amount, Bind} = H | T], List, Assets) ->
    case data_item:get(DataId) of
        #data_item{type = 1, overlap = Overlap} ->
            {NewList, Update} = add_lap(UserId, H, Overlap, ItemList, List),
            NewUser = User#user{item = NewList},
            add_loop(NewUser, T, Update, Assets);
        #data_item{type = 2, overlap = Overlap} ->
            {NewList, Update} = add_lap(UserId, H, Overlap, BagList, List),
            NewUser = User#user{bag = NewList},
            add_loop(NewUser, T, Update, Assets);
        #data_item{type = 3, overlap = Overlap} ->
            {NewList, Update} = add_lap(UserId, H, Overlap, StoreList, List),
            NewUser = User#user{store = NewList},
            add_loop(NewUser, T, Update, Assets);
        #data_item{type = 11} ->
            Add = {gold, Amount, Bind},
            {ok, NewUser} = player_assets:add(User, [Add]),
            add_loop(NewUser, T, List, [Add | Assets]);
        #data_item{type = 12} ->
            Add = {silver, Amount, Bind},
            {ok, NewUser} = player_assets:add(User, [Add]),
            add_loop(NewUser, T, List, [Add | Assets]);
        #data_item{type = 13} ->
            Add = {copper, Amount, Bind},
            {ok, NewUser} = player_assets:add(User, [Add]),
            add_loop(NewUser, T, List, [Add | Assets]);
        _ ->
            add_loop(User, T, List, Assets)
    end.

%% add lap
add_lap(UserId, {DataId, Add, Bind}, 1, List, Update) ->
    add_lap(UserId, {DataId, Add, Bind}, 1, [], List, Update);
add_lap(UserId, {DataId, Add, Bind}, Max, List, Update) ->
    add_lap(UserId, {DataId, Add, Bind}, Max, List, [], Update).

%% lap to new item list
add_lap(UserId, {DataId, Add, Bind}, Max, [], List, Update) when Add =< Max ->
    Item = #item{user_id = UserId, data_id = DataId, amount = Add, bind = Bind},
    Id = item_sql:insert(Item),
    NewItem = Item#item{id = Id},
    {[NewItem | List], [NewItem | Update]};

add_lap(UserId, {DataId, Add, Bind}, Max, [], List, Update) when Max < Add ->
    Item = #item{user_id = UserId, data_id = DataId, amount = Max, bind = Bind},
    Id = item_sql:insert(Item),
    NewItem = Item#item{id = Id},
    add_lap(UserId, {DataId, Add - Max, Bind}, Max, [], [NewItem | List], [NewItem | Update]);

%% lap to old item list
add_lap(UserId, {DataId, Add, Bind}, Max, [], List, Update) ->
    add_lap(UserId, {DataId, Add, Bind}, Max, [], List, Update);

add_lap(_UserId, {DataId, Add, Bind}, Max, [#item{data_id = DataId, amount = Amount, bind = Bind} = H | T], List, Update) when Amount + Add =< Max ->
    NewItem = H#item{amount = Amount + Add},
    {merge([NewItem | T], List), [NewItem | Update]};

add_lap(UserId, {DataId, Add, Bind}, Max, [#item{data_id = DataId, amount = Amount, bind = Bind} = H | T], List, Update) when Max < Amount + Add ->
    NewItem = H#item{amount = Max},
    add_lap(UserId, {DataId, Add - (Max - Amount), Bind}, Max, T, [NewItem | List], [NewItem | Update]);

add_lap(UserId, {DataId, Add, Bind}, Max, [H | T], List, Update) ->
    add_lap(UserId, {DataId, Add, Bind}, Max, T, [H | List], Update).

%% merge two list
merge([], List) ->
    List;
merge([H | T], List) ->
    merge(T, [H | List]).
%%%===================================================================
%%% Internal functions
%%%===================================================================

