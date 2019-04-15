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
%% @doc 创建角色
handle(30001, _State, [Type]) ->
    {reply, [rank_server:rank(Type)]};

%% @doc 容错
handle(Protocol, _User, Data) ->
    {error, Protocol, Data}.


