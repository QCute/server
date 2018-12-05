%%%-------------------------------------------------------------------
%%% @doc
%%% module item base, manager item bag
%%% @end
%%%-------------------------------------------------------------------
-module(item).
%% API
-export([load/1, save/1]).
-include("common.hrl").
-include("player.hrl").
-include("item.hrl").
%%%===================================================================
%%% API
%%%===================================================================
%% @doc load user items
load(User = #user{id = UserId}) ->
    Data = item_sql:select(UserId),
    Items = data_tool:load(Data, item),
    User#user{item = Items}.

%% @doc save user items
save(User = #user{item = Item}) ->
    NewItem = item_sql:update_into(Item),
    User#user{item = NewItem}.
%%%===================================================================
%%% Internal functions
%%%===================================================================

