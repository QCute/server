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
handle(?PP_ITEM_INFO, #user{item = Item}, []) ->
    {reply, Item};

%% @doc 容错
handle(Protocol, _User, Data) ->
    {error, Protocol, Data}.

