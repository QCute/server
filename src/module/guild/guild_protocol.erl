-module(guild_protocol).
-export([read/2, write/2]).
-include("guild.hrl").


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

read(30116, <<RoleId:64>>) ->
    {ok, RoleId};

read(30117, <<>>) ->
    {ok, []};

read(30118, <<RoleId:64, Job:8>>) ->
    {ok, [RoleId, Job]};

read(30119, <<>>) ->
    {ok, []};

read(30120, <<Type:8>>) ->
    {ok, Type};

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

write(30106, List) ->
    {ok, protocol:pack(30106, <<(length(List)):16, <<<<GuildId:64, ApplyTime:32, (byte_size(GuildName)):16, (GuildName)/binary>> || #guild_apply{guild_id = GuildId, apply_time = ApplyTime, guild_name = GuildName} <- List>>/binary>>)};

write(30107, Result) ->
    {ok, protocol:pack(30107, <<(text(30107, Result))/binary>>)};

write(30108, Result) ->
    {ok, protocol:pack(30108, <<(text(30108, Result))/binary>>)};

write(30109, Result) ->
    {ok, protocol:pack(30109, <<(text(30109, Result))/binary>>)};

write(30110, Result) ->
    {ok, protocol:pack(30110, <<(text(30110, Result))/binary>>)};

write(30111, Result) ->
    {ok, protocol:pack(30111, <<(text(30111, Result))/binary>>)};

write(30112, Result) ->
    {ok, protocol:pack(30112, <<(text(30112, Result))/binary>>)};

write(30113, Result) ->
    {ok, protocol:pack(30113, <<(text(30113, Result))/binary>>)};

write(30114, Result) ->
    {ok, protocol:pack(30114, <<(text(30114, Result))/binary>>)};

write(30115, Result) ->
    {ok, protocol:pack(30115, <<(text(30115, Result))/binary>>)};

write(30116, Result) ->
    {ok, protocol:pack(30116, <<(text(30116, Result))/binary>>)};

write(30117, Result) ->
    {ok, protocol:pack(30117, <<(text(30117, Result))/binary>>)};

write(30118, Result) ->
    {ok, protocol:pack(30118, <<(text(30118, Result))/binary>>)};

write(30119, Result) ->
    {ok, protocol:pack(30119, <<(text(30119, Result))/binary>>)};

write(30120, Result) ->
    {ok, protocol:pack(30120, <<(text(30120, Result))/binary>>)};

write(Code, Content) ->
    {error, Code, Content}.



text(30107, condition_not_enough) ->
    <<12:16, "条件不足"/utf8>>;
text(30107, duplicate) ->
    <<12:16, "名字重复"/utf8>>;
text(30107, length) ->
    <<12:16, "长度不对"/utf8>>;
text(30107, not_utf8) ->
    <<12:16, "未知字符"/utf8>>;
text(30107, sensitive) ->
    <<15:16, "包含敏感词"/utf8>>;
text(30107, time_in_join_cd) ->
    <<27:16, "创建公会时间冷却中"/utf8>>;
text(30107, timeout) ->
    <<12:16, "请求超时"/utf8>>;
text(30108, already_join_guild) ->
    <<27:16, "你已经加入过公会了"/utf8>>;
text(30108, condition_not_enough) ->
    <<12:16, "条件不足"/utf8>>;
text(30108, no_such_guild) ->
    <<15:16, "没有此公会"/utf8>>;
text(30108, time_in_join_cd) ->
    <<27:16, "创建公会时间冷却中"/utf8>>;
text(30108, timeout) ->
    <<12:16, "请求超时"/utf8>>;
text(30109, timeout) ->
    <<12:16, "请求超时"/utf8>>;
text(30110, timeout) ->
    <<12:16, "请求超时"/utf8>>;
text(30111, already_join_guild) ->
    <<21:16, "已加入其它公会"/utf8>>;
text(30111, member_number_limit) ->
    <<21:16, "已达到成员上限"/utf8>>;
text(30111, no_such_apply) ->
    <<15:16, "没有此申请"/utf8>>;
text(30111, no_such_guild) ->
    <<15:16, "没有此公会"/utf8>>;
text(30111, permission_denied) ->
    <<12:16, "权限不足"/utf8>>;
text(30111, timeout) ->
    <<12:16, "请求超时"/utf8>>;
text(30112, permission_denied) ->
    <<12:16, "权限不足"/utf8>>;
text(30112, timeout) ->
    <<12:16, "请求超时"/utf8>>;
text(30113, permission_denied) ->
    <<12:16, "权限不足"/utf8>>;
text(30113, timeout) ->
    <<12:16, "请求超时"/utf8>>;
text(30113, you_not_join_guild) ->
    <<27:16, "你没有加入任何公会"/utf8>>;
text(30114, permission_denied) ->
    <<12:16, "权限不足"/utf8>>;
text(30114, timeout) ->
    <<12:16, "请求超时"/utf8>>;
text(30115, timeout) ->
    <<12:16, "请求超时"/utf8>>;
text(30115, you_not_join_guild) ->
    <<27:16, "你没有加入任何公会"/utf8>>;
text(30116, cannot_kick_self) ->
    <<18:16, "不可剔除自己"/utf8>>;
text(30116, he_not_join_guild) ->
    <<24:16, "此人没有加入公会"/utf8>>;
text(30116, permission_denied) ->
    <<12:16, "权限不足"/utf8>>;
text(30116, timeout) ->
    <<12:16, "请求超时"/utf8>>;
text(30116, you_not_join_guild) ->
    <<27:16, "你没有加入任何公会"/utf8>>;
text(30117, permission_denied) ->
    <<12:16, "权限不足"/utf8>>;
text(30117, timeout) ->
    <<12:16, "请求超时"/utf8>>;
text(30117, you_not_join_guild) ->
    <<27:16, "你没有加入任何公会"/utf8>>;
text(30118, cannot_update_self) ->
    <<18:16, "不可升级自己"/utf8>>;
text(30118, he_not_join_guild) ->
    <<24:16, "此人没有加入公会"/utf8>>;
text(30118, job_invalid) ->
    <<12:16, "位置无效"/utf8>>;
text(30118, permission_denied) ->
    <<12:16, "权限不足"/utf8>>;
text(30118, timeout) ->
    <<12:16, "请求超时"/utf8>>;
text(30118, you_not_join_guild) ->
    <<27:16, "你没有加入任何公会"/utf8>>;
text(_, ok) ->
    <<0:16>>;
text(_, Reason) ->
    protocol:write_bit_string(type:to_binary(Reason)).

