%%%-------------------------------------------------------------------
%%% @doc
%%% module key handle
%%% @end
%%%-------------------------------------------------------------------
-module(shop_handle).
%% API
-export([handle/3]).
%% Includes
-include("user.hrl").
-include("player.hrl").
-include("protocol.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%% @doc shop list
handle(?CMD_SHOP, #user{shop = Shop}, []) ->
    {reply, Shop};

%% @doc shop buy
handle(?CMD_SHOP_BUY, User, [ShopId, Amount]) ->
    case shop:buy(User, ShopId, Amount) of
        {ok, NewUser} ->
            {reply, [1], NewUser};
        {error, Code} ->
            {reply, [Code]}
    end;

%% @doc 容错
handle(Protocol, _User, Data) ->
    {error, Protocol, Data}.


