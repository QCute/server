%%%-------------------------------------------------------------------
%%% @doc
%%% module protocol read write
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_guild).
-export([main/1]).
-include("../../../include/serialize.hrl").
-include("../../../include/guild.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
main([]) ->
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    io:format("~p~n", [catch protocol_maker:start(protocol())]);
main(_) ->
    io:format("invalid argument~n").

%%%===================================================================
%%% protocol config
%%%===================================================================
protocol() ->
    [#protocol{
        number = 301,
        handler = "src/module/guild/guild_handler.erl",
        erl = "src/module/guild/guild_protocol.erl",
        lua = "script/make/protocol/lua/GuildProtocol.lua",
        json = "script/make/protocol/json/GuildProtocol.js",
        includes = ["guild.hrl"],
        io = [
            #io{
                protocol = 30101,
                comment = "公会列表",
                handler = #handler{module = guild_server, function = query_guild, arg = []},
                read = [],
                write = [
                    #ets{name = list, comment = "公会列表", explain = #guild{
                        guild_id = #u64{comment = "公会ID"},
                        guild_name = #bst{comment = "公会名字"},
                        leader_id = #u64{comment = "会长ID"},
                        leader_name = #bst{comment = "会长名字"},
                        create_time = #u32{comment = "创建时间"}
                    }}
                ]
            },
            #io{
                comment = "成员列表",
                protocol = 30102,
                handler = #handler{module = guild_server, function = query_role},
                read = [],
                write = [
                    #ets{name = list, comment = "成员列表", explain = #guild_role{
                        role_id = #u64{comment = "成员ID"},
                        role_name = #bst{comment = "成员名字"},
                        join_time = #u32{comment = "加入时间"},
                        job = #u8{comment = "职位"},
                        sex = #u8{comment = "性别"},
                        classes = #u8{comment = "职业"},
                        vip_level = #u8{comment = "Vip等级"}
                    }}
                ]
            },
            #io{
                protocol = 30103,
                comment = "申请列表",
                handler = #handler{module = guild_server, function = query_apply},
                read = [],
                write = [
                    #ets{name = list, comment = "申请列表", explain = #guild_apply{
                        role_id = #u64{comment = "申请ID"},
                        role_name = #bst{comment = "申请名字"},
                        apply_time = #u32{comment = "申请时间"},
                        sex = #u8{comment = "性别"},
                        classes = #u8{comment = "职业"},
                        vip_level = #u8{comment = "Vip等级"}
                    }}
                ]
            },
            #io{
                protocol = 30104,
                comment = "自身公会信息",
                handler = #handler{module = guild_server, function = query_self_guild},
                read = [],
                write = [
                    #guild{
                        guild_id = #u64{comment = "公会ID"},
                        guild_name = #bst{comment = "公会名字"},
                        leader_id = #u64{comment = "会长ID"},
                        leader_name = #bst{comment = "会长名字"},
                        create_time = #u32{comment = "创建时间"},
                        exp = #u32{comment = "经验"},
                        wealth = #u32{comment = "财富"},
                        level = #u8{comment = "等级"},
                        notice = #bst{comment = "公告"}
                    }
                ]
            },
            #io{
                protocol = 30105,
                comment = "自身成员信息",
                handler = #handler{module = guild_server, function = query_self_role},
                read = [],
                write = [
                    #guild_role{
                        role_id = #u64{comment = "成员ID"},
                        role_name = #bst{comment = "成员名字"},
                        join_time = #u32{comment = "加入时间"},
                        job = #u8{comment = "职位"},
                        sex = #u8{comment = "性别"},
                        classes = #u8{comment = "职业"},
                        vip_level = #u8{comment = "Vip等级"}
                    }
                ]
            },
            #io{
                protocol = 30106,
                comment = "自身申请信息",
                handler = #handler{module = guild_server, function = query_self_apply},
                read = [],
                write = [
                    #list{name = list, explain = #guild_apply{
                        guild_id = #u64{comment = "公会ID"},
                        guild_name = #bst{comment = "公会名字"},
                        apply_time = #u32{comment = "申请时间"}
                    }}
                ]
            },
            #io{
                protocol = 30107,
                comment = "创建公会",
                handler = #handler{module = guild_server, function = create},
                text = [{timeout, "请求超时"}, {condition_not_met, "条件不足"}, {time_in_join_cd, "创建公会时间冷却中"}, {length, "长度不对"}, {not_utf8, "未知字符"}, {sensitive, "包含敏感词"}, {duplicate, "名字重复"}],
                read = [
                    #u8{name = type, comment = "类型"},
                    #bst{name = guild_name, comment = "公会名"}
                ],
                write = [
                    #rst{name = result, comment = "结果"}
                ]
            },
            #io{
                protocol = 30108,
                comment = "申请",
                handler = #handler{module = guild_server, function = apply},
                text = [{timeout, "请求超时"}, {condition_not_met, "条件不足"}, {time_in_join_cd, "创建公会时间冷却中"}, {no_such_guild, "没有此公会"}, {already_join_guild, "你已经加入过公会了"}],
                read = [
                    #u64{name = guild_id, comment = "公会ID"}
                ],
                write = [
                    #rst{name = result, comment = "结果"}
                ]
            },
            #io{
                protocol = 30109,
                comment = "取消申请",
                handler = #handler{module = guild_server, function = cancel_apply},
                text = [{timeout, "请求超时"}],
                read = [
                    #u64{name = guild_id, comment = "公会ID"}
                ],
                write = [
                    #rst{name = result, comment = "结果"}
                ]
            },
            #io{
                protocol = 30110,
                comment = "取消全部申请",
                handler = #handler{module = guild_server, function = cancel_all_apply},
                text = [{timeout, "请求超时"}],
                read = [],
                write = [
                    #rst{name = result, comment = "结果"}
                ]
            },
            #io{
                protocol = 30111,
                comment = "允许申请",
                handler = #handler{module = guild_server, function = approve_apply},
                text = [{timeout, "请求超时"}, {permission_denied, "权限不足"}, {no_such_apply, "没有此申请"}, {already_join_guild, "已加入其它公会"}, {no_such_guild, "没有此公会"}, {member_number_limit, "已达到成员上限"}],
                read = [
                    #u64{name = role_id, comment = "角色ID"}
                ],
                write = [
                    #rst{name = result, comment = "结果"}
                ]
            },
            #io{
                protocol = 30112,
                comment = "允许全部申请",
                handler = #handler{module = guild_server, function = approve_all_apply},
                text = [{timeout, "请求超时"}, {permission_denied, "权限不足"}],
                read = [],
                write = [
                    #rst{name = result, comment = "结果"}
                ]
            },
            #io{
                protocol = 30113,
                comment = "拒绝申请",
                handler = #handler{module = guild_server, function = reject_apply},
                text = [{timeout, "请求超时"}, {permission_denied, "权限不足"}, {you_not_join_guild, "你没有加入任何公会"}],
                read = [
                    #u64{name = role_id, comment = "角色ID"}
                ],
                write = [
                    #rst{name = result, comment = "结果"}
                ]
            },
            #io{
                protocol = 30114,
                comment = "拒绝全部申请",
                handler = #handler{module = guild_server, function = reject_all_apply},
                text = [{timeout, "请求超时"}, {permission_denied, "权限不足"}],
                read = [],
                write = [
                    #rst{name = result, comment = "结果"}
                ]
            },
            #io{
                protocol = 30115,
                comment = "退出",
                handler = #handler{module = guild_server, function = leave},
                text = [{timeout, "请求超时"}, {you_not_join_guild, "你没有加入任何公会"}],
                read = [],
                write = [
                    #rst{name = result, comment = "结果"}
                ]
            },
            #io{
                protocol = 30116,
                comment = "踢出",
                handler = #handler{module = guild_server, function = kick},
                text = [{timeout, "请求超时"}, {permission_denied, "权限不足"}, {cannot_kick_self, "不可剔除自己"}, {you_not_join_guild, "你没有加入任何公会"}, {he_not_join_guild, "此人没有加入公会"}],
                read = [
                    #u64{name = role_id, comment = "角色ID"}
                ],
                write = [
                    #rst{name = result, comment = "结果"}
                ]
            },
            #io{
                protocol = 30117,
                comment = "解散",
                handler = #handler{module = guild_server, function = dismiss},
                text = [{timeout, "请求超时"}, {permission_denied, "权限不足"}, {you_not_join_guild, "你没有加入任何公会"}],
                read = [],
                write = [
                    #rst{name = result, comment = "结果"}
                ]
            },
            #io{
                protocol = 30118,
                comment = "调整位置",
                handler = #handler{module = guild_server, function = update_job},
                text = [{timeout, "请求超时"}, {permission_denied, "权限不足"}, {you_not_join_guild, "你没有加入任何公会"}, {he_not_join_guild, "此人没有加入公会"}, {cannot_update_self, "不可升级自己"}, {job_invalid, "位置无效"}],
                read = [
                    #u64{name = role_id, comment = "角色ID"},
                    #u8{name = job, comment = "位置"}
                ],
                write = [
                    #rst{name = result, comment = "结果"}
                ]
            },
            #io{
                protocol = 30119,
                comment = "升级",
                handler = #handler{module = guild_server, function = upgrade_level},
                text = [{timeout, "请求超时"}],
                read = [],
                write = [
                    #rst{name = result, comment = "结果"}
                ]
            },
            #io{
                protocol = 30120,
                comment = "捐献",
                handler = #handler{module = guild_server, function = devote},
                text = [{timeout, "请求超时"}],
                read = [
                    #u8{name = type, comment = "类型"}
                ],
                write = [
                    #rst{name = result, comment = "结果"}
                ]
            }
        ]
    }].
