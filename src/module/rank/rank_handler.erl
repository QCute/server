%%%-------------------------------------------------------------------
%%% @doc
%%% module rank handle
%%% @end
%%%-------------------------------------------------------------------
-module(rank_handler).
%% API
-export([handle/3]).
%% Includes
-include("protocol.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%% @doc 排行榜列表
handle(?PROTOCOL_RANK, _State, [Type]) ->
    {reply, [rank_server:rank(Type)]};

%% @doc 容错
handle(Protocol, _User, Data) ->
    {error, Protocol, Data}.


