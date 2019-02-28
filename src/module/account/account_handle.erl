%%%-------------------------------------------------------------------
%%% @doc
%%% module account handle
%%% @end
%%%-------------------------------------------------------------------
-module(account_handle).
%% export API functions
-export([handle/3]).
-include("protocol.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%% @doc 创建角色
handle(?PP_ACCOUNT_CREATE, State, Data = []) ->
    account:create(State, Data);

%% @doc 登陆验证
handle(?PP_ACCOUNT_LOGIN, State, Data = [_ServerId, _Name]) ->
    account:login(State, Data);

%% @doc 心跳包
handle(?PP_ACCOUNT_HEARTBEAT, State, Data) ->
    account:heart_beat(State, Data);

%% @doc 玩家移动
handle(?PP_PLAYER_MOVE, State, Data) ->
    account:move(State, Data);

%% @doc 发包速度控制
handle(_, State, Data) ->
    account:packet_speed(State, Data).


