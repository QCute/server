%%%-------------------------------------------------------------------
%%% @doc
%%% module item handle
%%% @end
%%%-------------------------------------------------------------------
-module(item_handle).
%% export API functions
-export([handle/3]).
-include("player.hrl").
-include("protocol.hrl").

%%%===================================================================
%%% API
%%%===================================================================
handle(?CMD_ITEM, #user{item = Item}, []) ->
    {reply, Item};

handle(?CMD_ITEM_EQUIP, #user{item = Item}, []) ->
    {reply, Item};

handle(?CMD_ITEM_USE, #user{item = Item}, []) ->
    {reply, Item};

%% @doc 容错
handle(Protocol, _User, Data) ->
    {error, Protocol, Data}.

