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
%% @doc convert user data to other data
-spec to(User :: #user{}, Type :: term()) -> term().
to(#user{role_id = RoleId, pid = Pid, sender_pid = SenderPid, receiver_pid = ReceiverPid}, online) ->
    #online{role_id = RoleId, pid = Pid, sender_pid = SenderPid, receiver_pid = ReceiverPid, status = online};
to(#user{role_id = RoleId, pid = Pid}, hosting) ->
    #online{role_id = RoleId, pid = Pid, sender_pid = undefined, receiver_pid = undefined, status = hosting};
to(#user{role_id = RoleId, role_name = RoleName, pid = Pid, sender_pid = SenderPid, role = #role{map = #map{x = X, y = Y}}, skill = Skill, buff = Buff, total_attribute = TotalAttribute}, map) ->
    Skills = skill:to_battle_skill(Skill),
    Buffs = buff:to_battle_buff(Buff),
    #fighter{id = RoleId, name = RoleName, pid = Pid, sender_pid = SenderPid, x = X, y = Y, type = ?MAP_OBJECT_ROLE, attribute = TotalAttribute, skill = Skills, buff = Buffs};
to(_Type, _R) ->
    {error, users_convert_unknown_type}.
