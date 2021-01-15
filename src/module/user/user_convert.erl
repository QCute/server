%%%-------------------------------------------------------------------
%%% @doc
%%% user convert
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
to(#user{role_id = RoleId, sender_pid = SenderPid}, online) ->
    #online{role_id = RoleId, pid = self(), sender_pid = SenderPid, state = online};
to(#user{role_id = RoleId}, hosting) ->
    #online{role_id = RoleId, pid = self(), sender_pid = undefined, state = hosting};
to(#user{role_id = RoleId, role_name = RoleName, sender_pid = SenderPid, role = #role{level = Level, sex = Sex, classes = Classes, map = #map{x = X, y = Y}}, skill = Skill, buff = Buff, total_attribute = TotalAttribute}, map) ->
    Skills = skill:to_battle_skill(Skill),
    Buffs = buff:to_battle_buff(Buff),
    Data = #fighter_role{role_name = RoleName, level = Level, sex = Sex, classes = Classes, pid = self(), sender_pid = SenderPid},
    #fighter{id = RoleId, type = ?MAP_OBJECT_ROLE, attribute = TotalAttribute, skill = Skills, buff = Buffs, x = X, y = Y, data = Data};
to(_Type, _R) ->
    {error, users_convert_unknown_type}.
