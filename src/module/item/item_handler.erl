%%%-------------------------------------------------------------------
%%% @doc
%%% module item handle
%%% @end
%%%-------------------------------------------------------------------
-module(item_handler).
%% API
-export([handle/3]).
%% Includes
-include("user.hrl").
-include("role.hrl").
-include("protocol.hrl").

%%%===================================================================
%%% API
%%%===================================================================
handle(?PROTOCOL_ITEM, #user{item = Item}, []) ->
    {reply, Item};

handle(?PROTOCOL_ITEM_EQUIP, #user{item = Item}, []) ->
    {reply, Item};

handle(?PROTOCOL_ITEM_USE, #user{item = Item}, []) ->
    {reply, Item};

%% @doc 容错
handle(Protocol, _User, Data) ->
    {error, Protocol, Data}.

