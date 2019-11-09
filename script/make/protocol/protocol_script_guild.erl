%%%------------------------------------------------------------------
%%% @doc
%%% module protocol read write
%%% @end
%%%------------------------------------------------------------------
-module(protocol_script_guild).
-export([main/1]).
-include("../../../include/serialize.hrl").
-include("../../../include/guild.hrl").
%%%==================================================================
%%% API functions
%%%==================================================================
main([]) ->
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    console:stacktrace(catch protocol_maker:start([protocol()]));
main(_) ->
    io:format("invalid argument~n").

%%%==================================================================
%%% protocol config
%%%==================================================================
protocol() ->
    #protocol{
        name = 301,
        handler = "src/module/guild/guild_handler.erl",
        erl = "src/module/guild/guild_protocol.erl",
        lua = "script/make/protocol/lua/GuildProtocol.lua",
        json = "script/make/protocol/json/GuildProtocol.js",
        includes = ["guild.hrl"],
        io = [
            #io{
                name = 30101,
                comment = "公会列表",
                handler = #handler{module = guild_server, function = query_guild, arg = []},
                read = [],
                write = [
                    #list{name = list, comment = "公会列表", explain = #guild{
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
                name = 30102,
                handler = #handler{module = guild_server, function = query_role, arg = []},
                read = [
                    #u64{name = guild_id, comment = "公会ID"}
                ],
                write = [
                    #list{name = list, comment = "成员列表", explain = #guild_role{
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
                name = 30103,
                comment = "申请列表",
                handler = #handler{module = guild_server, function = query_apply, arg = []},
                read = [
                    #u64{name = guild_id, comment = "公会ID"}
                ],
                write = [
                    #list{name = list, comment = "申请列表", explain = #guild_apply{
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
                name = 30104,
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
                name = 30105,
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
                name = 30106,
                comment = "自身成员信息",
                handler = #handler{module = guild_server, function = query_self_apply},
                read = [],
                write = [
                    #guild_apply{
                        guild_id = #u64{comment = "公会ID"}
                    }
                ]
            }
        ]
    }.
