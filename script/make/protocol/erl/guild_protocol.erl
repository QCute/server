-module(guild_protocol).
-export([decode/2, encode/2]).
-include("guild.hrl").

-spec decode(Protocol :: non_neg_integer(), Binary :: binary()) -> {ok, [integer() | binary() | list()]} | {error, Protocol :: non_neg_integer(), Binary :: binary()}.
decode(30101, _Rest_ = <<_/binary>>) ->
    {ok, {}};

decode(30102, _Rest_ = <<_/binary>>) ->
    {ok, {}};

decode(30103, _Rest_ = <<_/binary>>) ->
    {ok, {}};

decode(30104, _Rest_ = <<_/binary>>) ->
    {ok, {}};

decode(30105, _Rest_ = <<_/binary>>) ->
    {ok, {}};

decode(30106, _Rest_ = <<_/binary>>) ->
    {ok, {}};

decode(30107, _Rest_ = <<_/binary>>) ->
    <<Type:8, _TypeRest_/binary>> = _Rest_,
    <<GuildNameLength:16, GuildName:GuildNameLength/binary, _GuildNameRest_/binary>> = _TypeRest_,
    {ok, {Type, GuildName}};

decode(30108, _Rest_ = <<_/binary>>) ->
    <<:64, _Rest_/binary>> = _Rest_,
    {ok, };

decode(30109, _Rest_ = <<_/binary>>) ->
    <<:64, _Rest_/binary>> = _Rest_,
    {ok, };

decode(30110, _Rest_ = <<_/binary>>) ->
    {ok, {}};

decode(30111, _Rest_ = <<_/binary>>) ->
    <<:64, _Rest_/binary>> = _Rest_,
    {ok, };

decode(30112, _Rest_ = <<_/binary>>) ->
    {ok, {}};

decode(30113, _Rest_ = <<_/binary>>) ->
    <<:64, _Rest_/binary>> = _Rest_,
    {ok, };

decode(30114, _Rest_ = <<_/binary>>) ->
    {ok, {}};

decode(30115, _Rest_ = <<_/binary>>) ->
    {ok, {}};

decode(30116, _Rest_ = <<_/binary>>) ->
    {ok, {}};

decode(30117, _Rest_ = <<_/binary>>) ->
    <<:64, _Rest_/binary>> = _Rest_,
    {ok, };

decode(30118, _Rest_ = <<_/binary>>) ->
    <<RoleId:64, _RoleIdRest_/binary>> = _Rest_,
    <<Job:8, _JobRest_/binary>> = _RoleIdRest_,
    {ok, {RoleId, Job}};

decode(30119, _Rest_ = <<_/binary>>) ->
    {ok, {}};

decode(30120, _Rest_ = <<_/binary>>) ->
    <<Length:16, :Length/binary, _Rest_/binary>> = _Rest_,
    {ok, };

decode(Protocol, Binary) ->
    {error, Protocol, Binary}.


-spec encode(Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()) -> {ok, binary()} | {error, Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()}.
encode(30101, ) ->
    Data30101 = <<(encode__30101(<<>>, 0, ets:safe_fixtable(, true) andalso , ets:first()))/binary>>,
    {ok, <<(byte_size(Data30101)):16, 30101:16, Data30101/binary>>};

encode(30102, ) ->
    Data30102 = <<(encode__30102(<<>>, 0, ets:safe_fixtable(, true) andalso , ets:first()))/binary>>,
    {ok, <<(byte_size(Data30102)):16, 30102:16, Data30102/binary>>};

encode(30103, ) ->
    Data30103 = <<(encode__30103(<<>>, 0, ets:safe_fixtable(, true) andalso , ets:first()))/binary>>,
    {ok, <<(byte_size(Data30103)):16, 30103:16, Data30103/binary>>};

encode(30104, #guild{guild_id = GuildId, guild_name = GuildName, exp = Exp, wealth = Wealth, level = Level, create_time = CreateTime, notice = Notice, leader_role_id = LeaderRoleId, leader_name = LeaderName}) ->
    Data30104 = <<GuildId:64, (byte_size(GuildName)):16, (GuildName)/binary, Exp:32, Wealth:32, Level:8, CreateTime:32, (byte_size(Notice)):16, (Notice)/binary, LeaderRoleId:64, (byte_size(LeaderName)):16, (LeaderName)/binary>>,
    {ok, <<(byte_size(Data30104)):16, 30104:16, Data30104/binary>>};

encode(30105, #guild_role{role_id = RoleId, job = Job, join_time = JoinTime, role_name = RoleName, sex = Sex, classes = Classes, vip_level = VipLevel}) ->
    Data30105 = <<RoleId:64, Job:8, JoinTime:32, (byte_size(RoleName)):16, (RoleName)/binary, Sex:8, Classes:8, VipLevel:8>>,
    {ok, <<(byte_size(Data30105)):16, 30105:16, Data30105/binary>>};

encode(30106, ) ->
    Data30106 = <<(encode__30106(<<>>, 0, ))/binary>>,
    {ok, <<(byte_size(Data30106)):16, 30106:16, Data30106/binary>>};

encode(30107, ) ->
    Data30107 = <<(protocol:text())/binary>>,
    {ok, <<(byte_size(Data30107)):16, 30107:16, Data30107/binary>>};

encode(30108, ) ->
    Data30108 = <<(protocol:text())/binary>>,
    {ok, <<(byte_size(Data30108)):16, 30108:16, Data30108/binary>>};

encode(30109, ) ->
    Data30109 = <<(protocol:text())/binary>>,
    {ok, <<(byte_size(Data30109)):16, 30109:16, Data30109/binary>>};

encode(30110, ) ->
    Data30110 = <<(protocol:text())/binary>>,
    {ok, <<(byte_size(Data30110)):16, 30110:16, Data30110/binary>>};

encode(30111, ) ->
    Data30111 = <<(protocol:text())/binary>>,
    {ok, <<(byte_size(Data30111)):16, 30111:16, Data30111/binary>>};

encode(30112, ) ->
    Data30112 = <<(protocol:text())/binary>>,
    {ok, <<(byte_size(Data30112)):16, 30112:16, Data30112/binary>>};

encode(30113, ) ->
    Data30113 = <<(protocol:text())/binary>>,
    {ok, <<(byte_size(Data30113)):16, 30113:16, Data30113/binary>>};

encode(30114, ) ->
    Data30114 = <<(protocol:text())/binary>>,
    {ok, <<(byte_size(Data30114)):16, 30114:16, Data30114/binary>>};

encode(30115, ) ->
    Data30115 = <<(protocol:text())/binary>>,
    {ok, <<(byte_size(Data30115)):16, 30115:16, Data30115/binary>>};

encode(30116, ) ->
    Data30116 = <<(protocol:text())/binary>>,
    {ok, <<(byte_size(Data30116)):16, 30116:16, Data30116/binary>>};

encode(30117, ) ->
    Data30117 = <<(protocol:text())/binary>>,
    {ok, <<(byte_size(Data30117)):16, 30117:16, Data30117/binary>>};

encode(30118, ) ->
    Data30118 = <<(protocol:text())/binary>>,
    {ok, <<(byte_size(Data30118)):16, 30118:16, Data30118/binary>>};

encode(30119, ) ->
    Data30119 = <<(protocol:text())/binary>>,
    {ok, <<(byte_size(Data30119)):16, 30119:16, Data30119/binary>>};

encode(30120, ) ->
    Data30120 = <<(protocol:text())/binary>>,
    {ok, <<(byte_size(Data30120)):16, 30120:16, Data30120/binary>>};

encode(Protocol, Data) ->
    {error, Protocol, Data}.

encode__30101(Acc = <<_/binary>>, Length, Tab, '$end_of_table') ->
    ets:safe_fixtable(Tab, false),
    <<Length:16, Acc/binary>>;
encode__30101(Acc = <<_/binary>>, Length, Tab, Key) ->
    case ets:lookup(Tab, Key) of
        [] ->
            encode__30101(Acc, Length, Tab, ets:next(Tab, Key));
        [#guild{guild_id = GuildId, guild_name = GuildName, create_time = CreateTime, leader_role_id = LeaderRoleId, leader_name = LeaderName}] ->
            encode__30101(<<Acc/binary, GuildId:64, (byte_size(GuildName)):16, (GuildName)/binary, CreateTime:32, LeaderRoleId:64, (byte_size(LeaderName)):16, (LeaderName)/binary>>, Length + 1, Tab, ets:next(Tab, Key))
    end.

encode__30102(Acc = <<_/binary>>, Length, Tab, '$end_of_table') ->
    ets:safe_fixtable(Tab, false),
    <<Length:16, Acc/binary>>;
encode__30102(Acc = <<_/binary>>, Length, Tab, Key) ->
    case ets:lookup(Tab, Key) of
        [] ->
            encode__30102(Acc, Length, Tab, ets:next(Tab, Key));
        [#guild_role{role_id = RoleId, job = Job, join_time = JoinTime, role_name = RoleName, sex = Sex, classes = Classes, vip_level = VipLevel}] ->
            encode__30102(<<Acc/binary, RoleId:64, Job:8, JoinTime:32, (byte_size(RoleName)):16, (RoleName)/binary, Sex:8, Classes:8, VipLevel:8>>, Length + 1, Tab, ets:next(Tab, Key))
    end.

encode__30103(Acc = <<_/binary>>, Length, Tab, '$end_of_table') ->
    ets:safe_fixtable(Tab, false),
    <<Length:16, Acc/binary>>;
encode__30103(Acc = <<_/binary>>, Length, Tab, Key) ->
    case ets:lookup(Tab, Key) of
        [] ->
            encode__30103(Acc, Length, Tab, ets:next(Tab, Key));
        [#guild_apply{role_id = RoleId, apply_time = ApplyTime, role_name = RoleName, sex = Sex, classes = Classes, vip_level = VipLevel}] ->
            encode__30103(<<Acc/binary, RoleId:64, ApplyTime:32, (byte_size(RoleName)):16, (RoleName)/binary, Sex:8, Classes:8, VipLevel:8>>, Length + 1, Tab, ets:next(Tab, Key))
    end.

encode__30106(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode__30106(Acc = <<_/binary>>, Length, [#guild_apply{guild_id = GuildId, apply_time = ApplyTime, guild_name = GuildName} | ]) ->
    encode__30106(<<Acc/binary, GuildId:64, ApplyTime:32, (byte_size(GuildName)):16, (GuildName)/binary>>, Length + 1, ).

