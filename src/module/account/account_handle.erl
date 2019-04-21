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
%% @doc 心跳包
handle(?CMD_ACCOUNT_HEARTBEAT, State, Data) ->
    account:heartbeat(State, Data);

%% @doc 创建角色
handle(?CMD_ACCOUNT_CREATE, State, [AccountName, ServerId, NickName, Sex, Classes, AgentId, Device, Mac, DeviceType]) ->
    account:create(State, AccountName, ServerId, NickName, Sex, Classes, AgentId, Device, Mac, DeviceType);

%% @doc 登陆验证
handle(?CMD_ACCOUNT_LOGIN, State, Data = [_ServerId, _Name]) ->
    account:login(State, Data);

%% @doc 发包速度控制
handle(_, State, Data) ->
    account:packet_speed(State, Data).


