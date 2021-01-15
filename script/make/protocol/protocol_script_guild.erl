%%%-------------------------------------------------------------------
%%% @doc
%%% protocol read write define
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
        js = "script/make/protocol/js/GuildProtocol.js",
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
                read = [
                    #u8{name = type, comment = "类型"},
                    #qst{name = guild_name, comment = "公会名"}
                ],
                write = [
                    #rst{name = result, comment = "结果"}
                ]
            },
            #io{
                protocol = 30108,
                comment = "申请",
                handler = #handler{module = guild_server, function = apply},
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
                read = [],
                write = [
                    #rst{name = result, comment = "结果"}
                ]
            },
            #io{
                protocol = 30111,
                comment = "允许申请",
                handler = #handler{module = guild_server, function = approve_apply},
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
                read = [],
                write = [
                    #rst{name = result, comment = "结果"}
                ]
            },
            #io{
                protocol = 30113,
                comment = "拒绝申请",
                handler = #handler{module = guild_server, function = reject_apply},
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
                read = [],
                write = [
                    #rst{name = result, comment = "结果"}
                ]
            },
            #io{
                protocol = 30115,
                comment = "退出",
                handler = #handler{module = guild_server, function = leave},
                read = [],
                write = [
                    #rst{name = result, comment = "结果"}
                ]
            },
            #io{
                protocol = 30116,
                comment = "解散",
                handler = #handler{module = guild_server, function = dismiss},
                read = [],
                write = [
                    #rst{name = result, comment = "结果"}
                ]
            },
            #io{
                protocol = 30117,
                comment = "踢出",
                handler = #handler{module = guild_server, function = kick},
                read = [
                    #u64{name = role_id, comment = "角色ID"}
                ],
                write = [
                    #rst{name = result, comment = "结果"}
                ]
            },
            #io{
                protocol = 30118,
                comment = "调整位置",
                handler = #handler{module = guild_server, function = update_job},
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
                read = [],
                write = [
                    #rst{name = result, comment = "结果"}
                ]
            },
            #io{
                protocol = 30120,
                comment = "更改公告",
                handler = #handler{module = guild_server, function = change_notice},
                read = [
                    #qst{name = notice, comment = "公告"}
                ],
                write = [
                    #rst{name = result, comment = "结果"}
                ]
            }
        ]
    }].
