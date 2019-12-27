-module(guild_protocol).
-export([read/2, write/2]).
-include("guild.hrl").


read(30101, <<>>) ->
    {ok, []};

read(30102, <<GuildId:64>>) ->
    {ok, GuildId};

read(30103, <<GuildId:64>>) ->
    {ok, GuildId};

read(30104, <<>>) ->
    {ok, []};

read(30105, <<>>) ->
    {ok, []};

read(30106, <<>>) ->
    {ok, []};

read(Code, Binary) ->
    {error, Code, Binary}.



write(30101, List) ->
    ListBinary = protocol:write_ets(fun([#guild{guild_id = GuildId, create_time = CreateTime, guild_name = GuildName, leader_id = LeaderId, leader_name = LeaderName}]) -> <<GuildId:64, CreateTime:32, (byte_size(GuildName)):16, (GuildName)/binary, LeaderId:64, (byte_size(LeaderName)):16, (LeaderName)/binary>> end, List),
    {ok, protocol:pack(30101, <<ListBinary/binary>>)};

write(30102, List) ->
    ListBinary = protocol:write_ets(fun([#guild_role{role_id = RoleId, job = Job, join_time = JoinTime, role_name = RoleName, sex = Sex, classes = Classes, vip_level = VipLevel}]) -> <<RoleId:64, Job:8, JoinTime:32, (byte_size(RoleName)):16, (RoleName)/binary, Sex:8, Classes:8, VipLevel:8>> end, List),
    {ok, protocol:pack(30102, <<ListBinary/binary>>)};

write(30103, List) ->
    ListBinary = protocol:write_ets(fun([#guild_apply{role_id = RoleId, apply_time = ApplyTime, role_name = RoleName, sex = Sex, classes = Classes, vip_level = VipLevel}]) -> <<RoleId:64, ApplyTime:32, (byte_size(RoleName)):16, (RoleName)/binary, Sex:8, Classes:8, VipLevel:8>> end, List),
    {ok, protocol:pack(30103, <<ListBinary/binary>>)};

write(30104, #guild{guild_id = GuildId, exp = Exp, wealth = Wealth, level = Level, create_time = CreateTime, guild_name = GuildName, notice = Notice, leader_id = LeaderId, leader_name = LeaderName}) ->
    {ok, protocol:pack(30104, <<GuildId:64, Exp:32, Wealth:32, Level:8, CreateTime:32, (byte_size(GuildName)):16, (GuildName)/binary, (byte_size(Notice)):16, (Notice)/binary, LeaderId:64, (byte_size(LeaderName)):16, (LeaderName)/binary>>)};

write(30105, #guild_role{role_id = RoleId, job = Job, join_time = JoinTime, role_name = RoleName, sex = Sex, classes = Classes, vip_level = VipLevel}) ->
    {ok, protocol:pack(30105, <<RoleId:64, Job:8, JoinTime:32, (byte_size(RoleName)):16, (RoleName)/binary, Sex:8, Classes:8, VipLevel:8>>)};

write(30106, #guild_apply{guild_id = GuildId}) ->
    {ok, protocol:pack(30106, <<GuildId:64>>)};

write(Code, Content) ->
    {error, Code, Content}.

