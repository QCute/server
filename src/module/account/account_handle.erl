%%%-------------------------------------------------------------------
%%% @doc
%%% module account handle
%%% @end
%%%-------------------------------------------------------------------
-module(account_handle).
%% API
-export([handle/3]).
%% Includes
-include("protocol.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%% @doc 心跳包
handle(?CMD_ACCOUNT_HEARTBEAT, State, Data) ->
    account:heartbeat(State, Data);

%% @doc 登陆验证
handle(?CMD_ACCOUNT_LOGIN, State, [ServerId, Name]) ->
    account:login(State, ServerId, Name);

%% @doc 创建角色
handle(?CMD_ACCOUNT_CREATE, State, [AccountName, ServerId, NickName, Sex, Classes, AgentId, Device, Mac, DeviceType]) ->
    account:create(State, AccountName, ServerId, NickName, Sex, Classes, AgentId, Device, Mac, DeviceType);

%% @doc 查询角色
handle(?CMD_ACCOUNT_QUERY, State, [Name]) ->
    account:query(State, Name);

%% @doc 发包速度控制
handle(_, State, Data) ->
    account:packet_speed(State, Data).


