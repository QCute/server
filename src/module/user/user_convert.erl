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
-include("role.hrl").
-include("attribute.hrl").
-include("map.hrl").
%%%===================================================================
%%% API
%%%===================================================================
%% @doc 将角色数据转换成其它格式的数据
%% 容错
to(#user{role_id = RoleId, pid = Pid, sender_pid = SenderPid, role = #role{map = #map{x = X, y = Y}}}, map) ->
    #fighter{id = RoleId, pid = Pid, sender_pid = SenderPid, x = X, y = Y, attribute = #attribute{}};
to(_Type, _R) ->
    {error, users_convert_unknow_type}.
