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
-include("online.hrl").
-include("role.hrl").
-include("map.hrl").
-include("attribute.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc 将角色数据转换成其它格式的数据
-spec to(User :: #user{}, Type :: term()) -> term().
to(#user{role_id = RoleId, pid = Pid, sender_pid = SenderPid, receiver_pid = ReceiverPid}, online) ->
    #online{role_id = RoleId, pid = Pid, sender_pid = SenderPid, receiver_pid = ReceiverPid, status = online};
to(#user{role_id = RoleId, pid = Pid}, hosting) ->
    #online{role_id = RoleId, pid = Pid, sender_pid = undefined, receiver_pid = undefined, status = hosting};
to(#user{role_id = RoleId, role_name = RoleName, pid = Pid, sender_pid = SenderPid, role = #role{map = #map{x = X, y = Y}}, skill = Skill}, map) ->
    Attribute = #attribute{fc = 13112757923, hp = 13112757923, health = 13112757923},
    Skills = skill:to_battle_skill(Skill),
    #fighter{id = RoleId, name = RoleName, pid = Pid, sender_pid = SenderPid, x = X, y = Y, type = ?MAP_OBJECT_ROLE, attribute = Attribute, skills = Skills};
to(_Type, _R) ->
    {error, users_convert_unknown_type}.
