-module(guild_protocol).
-export([read/2, write/2]).
-include("guild.hrl").


-spec read(Protocol :: non_neg_integer(), Binary :: binary()) -> {ok, [integer() | binary() | list()]} | {error, Protocol :: non_neg_integer(), Binary :: binary()}.
read(30101, <<>>) ->
    {ok, []};

read(30102, <<>>) ->
    {ok, []};

read(30103, <<>>) ->
    {ok, []};

read(30104, <<>>) ->
    {ok, []};

read(30105, <<>>) ->
    {ok, []};

read(30106, <<>>) ->
    {ok, []};

read(30107, <<Type:8, GuildNameLength:16, GuildName:GuildNameLength/binary>>) ->
    {ok, [Type, GuildName]};

read(30108, <<GuildId:64>>) ->
    {ok, GuildId};

read(30109, <<GuildId:64>>) ->
    {ok, GuildId};

read(30110, <<>>) ->
    {ok, []};

read(30111, <<RoleId:64>>) ->
    {ok, RoleId};

read(30112, <<>>) ->
    {ok, []};

read(30113, <<RoleId:64>>) ->
    {ok, RoleId};

read(30114, <<>>) ->
    {ok, []};

read(30115, <<>>) ->
    {ok, []};

read(30116, <<>>) ->
    {ok, []};

read(30117, <<RoleId:64>>) ->
    {ok, RoleId};

read(30118, <<RoleId:64, Job:8>>) ->
    {ok, [RoleId, Job]};

read(30119, <<>>) ->
    {ok, []};

read(30120, <<NoticeLength:16, Notice:NoticeLength/binary>>) ->
    {ok, Notice};

read(Protocol, Binary) ->
    {error, Protocol, Binary}.


-spec write(Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()) -> {ok, binary()} | {error, Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()}.
write(30101, List) ->
    ListBinary = protocol:write_ets(fun([#guild{guild_id = GuildId, guild_name = GuildName, create_time = CreateTime, leader_role_id = LeaderRoleId, leader_name = LeaderName}]) -> <<GuildId:64, (byte_size(GuildName)):16, (GuildName)/binary, CreateTime:32, LeaderRoleId:64, (byte_size(LeaderName)):16, (LeaderName)/binary>> end, List),
    {ok, protocol:pack(30101, <<ListBinary/binary>>)};

write(30102, List) ->
    ListBinary = protocol:write_ets(fun([#guild_role{role_id = RoleId, job = Job, join_time = JoinTime, role_name = RoleName, sex = Sex, classes = Classes, vip_level = VipLevel}]) -> <<RoleId:64, Job:8, JoinTime:32, (byte_size(RoleName)):16, (RoleName)/binary, Sex:8, Classes:8, VipLevel:8>> end, List),
    {ok, protocol:pack(30102, <<ListBinary/binary>>)};

write(30103, List) ->
    ListBinary = protocol:write_ets(fun([#guild_apply{role_id = RoleId, apply_time = ApplyTime, role_name = RoleName, sex = Sex, classes = Classes, vip_level = VipLevel}]) -> <<RoleId:64, ApplyTime:32, (byte_size(RoleName)):16, (RoleName)/binary, Sex:8, Classes:8, VipLevel:8>> end, List),
    {ok, protocol:pack(30103, <<ListBinary/binary>>)};

write(30104, #guild{guild_id = GuildId, guild_name = GuildName, exp = Exp, wealth = Wealth, level = Level, create_time = CreateTime, notice = Notice, leader_role_id = LeaderRoleId, leader_name = LeaderName}) ->
    {ok, protocol:pack(30104, <<GuildId:64, (byte_size(GuildName)):16, (GuildName)/binary, Exp:32, Wealth:32, Level:8, CreateTime:32, (byte_size(Notice)):16, (Notice)/binary, LeaderRoleId:64, (byte_size(LeaderName)):16, (LeaderName)/binary>>)};

write(30105, #guild_role{role_id = RoleId, job = Job, join_time = JoinTime, role_name = RoleName, sex = Sex, classes = Classes, vip_level = VipLevel}) ->
    {ok, protocol:pack(30105, <<RoleId:64, Job:8, JoinTime:32, (byte_size(RoleName)):16, (RoleName)/binary, Sex:8, Classes:8, VipLevel:8>>)};

write(30106, List) ->
    ListBinary = protocol:write_list(fun(#guild_apply{guild_id = GuildId, apply_time = ApplyTime, guild_name = GuildName}) -> <<GuildId:64, ApplyTime:32, (byte_size(GuildName)):16, (GuildName)/binary>> end, List),
    {ok, protocol:pack(30106, <<ListBinary/binary>>)};

write(30107, Result) ->
    {ok, protocol:pack(30107, <<(protocol:text(Result))/binary>>)};

write(30108, Result) ->
    {ok, protocol:pack(30108, <<(protocol:text(Result))/binary>>)};

write(30109, Result) ->
    {ok, protocol:pack(30109, <<(protocol:text(Result))/binary>>)};

write(30110, Result) ->
    {ok, protocol:pack(30110, <<(protocol:text(Result))/binary>>)};

write(30111, Result) ->
    {ok, protocol:pack(30111, <<(protocol:text(Result))/binary>>)};

write(30112, Result) ->
    {ok, protocol:pack(30112, <<(protocol:text(Result))/binary>>)};

write(30113, Result) ->
    {ok, protocol:pack(30113, <<(protocol:text(Result))/binary>>)};

write(30114, Result) ->
    {ok, protocol:pack(30114, <<(protocol:text(Result))/binary>>)};

write(30115, Result) ->
    {ok, protocol:pack(30115, <<(protocol:text(Result))/binary>>)};

write(30116, Result) ->
    {ok, protocol:pack(30116, <<(protocol:text(Result))/binary>>)};

write(30117, Result) ->
    {ok, protocol:pack(30117, <<(protocol:text(Result))/binary>>)};

write(30118, Result) ->
    {ok, protocol:pack(30118, <<(protocol:text(Result))/binary>>)};

write(30119, Result) ->
    {ok, protocol:pack(30119, <<(protocol:text(Result))/binary>>)};

write(30120, Result) ->
    {ok, protocol:pack(30120, <<(protocol:text(Result))/binary>>)};

write(Protocol, Data) ->
    {error, Protocol, Data}.


