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
	{NewUser, NewList} = add_loop(User, List, []),
	{ok, Binary} = player_route:write(897456, [NewList]),
	player_server:send(NewUser, Binary),
	{ok, NewUser}.

add_loop(User, [], List) ->
	{User, List};
add_loop(User = #user{id = UserId, item = ItemList, bag = BagList, store = StoreList}, [{DataId, Amount, Bind} | T], List) ->
	case data_item:get(DataId) of
		#data_item{type = 1} ->
			Item = #item{user_id = UserId, data_id = DataId, amount = Amount, bind = Bind},
			Id = item_sql:insert(Item),
			NewItem = Item#item{id = Id},
			NewUser = User#user{item = [NewItem | ItemList]},
			add_loop(NewUser, T, [NewItem | List]);
		#data_item{type = 2} ->
			Item = #item{user_id = UserId, data_id = DataId, amount = Amount, bind = Bind},
			Id = item_sql:insert(Item),
			NewItem = Item#item{id = Id},
			NewUser = User#user{bag = [NewItem | BagList]},
			add_loop(NewUser, T, [NewItem | List]);
		#data_item{type = 3} ->
			Item = #item{user_id = UserId, data_id = DataId, amount = Amount, bind = Bind},
			Id = item_sql:insert(Item),
			NewItem = Item#item{id = Id},
			NewUser = User#user{store = [NewItem | StoreList]},
			add_loop(NewUser, T, [NewItem | List]);
		_ ->
			add_loop(User, T, List)
	end.
%%%===================================================================
%%% Internal functions
%%%===================================================================

