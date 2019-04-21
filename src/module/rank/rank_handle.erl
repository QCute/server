%%%-------------------------------------------------------------------
%%% @doc
%%% module account handle
%%% @end
%%%-------------------------------------------------------------------
-module(rank_handle).
%% export API functions
-export([handle/3]).
-include("protocol.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%% @doc 排行榜列表
handle(?CMD_RANK, _State, [Type]) ->
    {reply, [rank_server:rank(Type)]};

%% @doc 容错
handle(Protocol, _User, Data) ->
    {error, Protocol, Data}.


