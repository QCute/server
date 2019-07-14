%%%-------------------------------------------------------------------
%%% @doc
%%% module shop handle
%%% @end
%%%-------------------------------------------------------------------
-module(shop_handler).
%% API
-export([handle/3]).
%% Includes
-include("user.hrl").
-include("role.hrl").
-include("protocol.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%% @doc shop list
handle(?PROTOCOL_SHOP, #user{shop = Shop}, []) ->
    {reply, Shop};

%% @doc shop buy
handle(?PROTOCOL_SHOP_BUY, User, [ShopId, Amount]) ->
    case shop:buy(User, ShopId, Amount) of
        {ok, NewUser} ->
            {reply, [1], NewUser};
        {error, Code} ->
            {reply, [Code]}
    end;

%% @doc 容错
handle(Protocol, _User, Data) ->
    {error, Protocol, Data}.


