%%%-------------------------------------------------------------------
%%% @doc
%%% module auction handle
%%% @end
%%%-------------------------------------------------------------------
-module(auction_handler).
%% API
-export([handle/3]).
%% Includes
-include("protocol.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%% 列表
handle(?PROTOCOL_AUCTION_LIST, User, []) ->
    {reply, [auction_server], User};

%% 拍卖
handle(?PROTOCOL_AUCTION_BID, User, [UniqueId]) ->
    case auction_server:bid(User, UniqueId) of
        {ok, Reply, NewUser} ->
            {reply, Reply, NewUser};
        {error, Reason} ->
            {reply, Reason}
    end;

%% 容错
handle(Protocol, _User, Data) ->
    {error, Protocol, Data}.
