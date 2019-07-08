%%%-------------------------------------------------------------------
%%% @doc
%%% module role data convert
%%% @end
%%%-------------------------------------------------------------------
-module(user_convert).
%% API
-export([to/2]).
%% Includes
-include("common.hrl").
-include("user.hrl").
-include("map.hrl").
%%%===================================================================
%%% API
%%%===================================================================
%% @doc 将角色数据转换成其它格式的数据
%% 容错
to(#user{id = Id, pid = Pid, pid_sender = SenderPid, map = #map{x = X, y = Y}}, map) ->
    #fighter{id = Id, pid = Pid, pid_sender = SenderPid, x = X, y = Y};
to(_Type, _R) ->
    {error, users_convert_unknow_type}.
