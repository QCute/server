%%%-------------------------------------------------------------------
%%% @doc
%%% module item base, manager item bag
%%% @end
%%%-------------------------------------------------------------------
-module(item).
%% API
-export([load/1, save/1]).
-export([add/2]).
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
    player_server:send(NewUser, <<NewListBinary/binary, AssetsBinary/binary>>),
    {ok, NewUser}.

add_loop(User, [], List, Assets) ->
    {User, List, Assets};
add_loop(User = #user{id = UserId, item = ItemList, bag = BagList, store = StoreList}, [{DataId, Amount, Bind} | T], List, Assets) ->
    case data_item:get(DataId) of
        #data_item{type = 1} ->
            Item = #item{user_id = UserId, data_id = DataId, amount = Amount, bind = Bind},
            Id = item_sql:insert(Item),
            NewItem = Item#item{id = Id},
            NewUser = User#user{item = [NewItem | ItemList]},
            add_loop(NewUser, T, [NewItem | List], Assets);
        #data_item{type = 2} ->
            Item = #item{user_id = UserId, data_id = DataId, amount = Amount, bind = Bind},
            Id = item_sql:insert(Item),
            NewItem = Item#item{id = Id},
            NewUser = User#user{bag = [NewItem | BagList]},
            add_loop(NewUser, T, [NewItem | List], Assets);
        #data_item{type = 3} ->
            Item = #item{user_id = UserId, data_id = DataId, amount = Amount, bind = Bind},
            Id = item_sql:insert(Item),
            NewItem = Item#item{id = Id},
            NewUser = User#user{store = [NewItem | StoreList]},
            add_loop(NewUser, T, [NewItem | List], Assets);
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
%%%===================================================================
%%% Internal functions
%%%===================================================================

