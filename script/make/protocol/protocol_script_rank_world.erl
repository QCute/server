%%%-------------------------------------------------------------------
%%% @doc
%%% protocol read write define
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_rank_world).
-export([main/1]).
-include("../../../include/serialize.hrl").
-include("../../../include/rank.hrl").
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
        number = 192,
        handler = "src/module/rank/rank_world_handler.erl",
        erl = "src/module/rank/rank_world_protocol.erl",
        js = "script/make/protocol/js/RankWorldProtocol.js",
        lua = "script/make/protocol/lua/RankWorldProtocol.lua",
        includes = ["rank.hrl"],
        io = [
            #io{
                protocol = 19201,
                comment = "Rank",
                handler = #handler{module = rank_server, function = query_world, protocol = true, alias = false},
                read = [],
                write = [
                    #list{name = list, comment = "排行榜", explain = #rank{
                        type = #u16{comment = "类型"},
                        key = #u64{comment = "键"},
                        value = #u64{comment = "值"},
                        time = #u32{comment = "时间"},
                        order = #u64{comment = "排名"},
                        name = #bst{comment = "名字"},
                        server_id = #u16{comment = "服务器ID"}
                    }}
                ]
            },
            #io{
                protocol = 19202,
                comment = "Rank",
                handler = #handler{module = rank_server, function = query_world, protocol = true, alias = false},
                read = [],
                write = [
                    #list{name = list, comment = "排行榜", explain = #rank{
                        type = #u16{comment = "类型"},
                        key = #u64{comment = "键"},
                        value = #u64{comment = "值"},
                        time = #u32{comment = "时间"},
                        order = #u64{comment = "排名"},
                        name = #bst{comment = "名字"},
                        server_id = #u16{comment = "服务器ID"},
                        other = #tuple{explain = {
                            #u16{name = level, comment = "等级"},
                            #u8{name = classes, comment = "职业"}
                        }}
                    }}
                ]
            },
            #io{
                protocol = 19203,
                comment = "Rank",
                handler = #handler{module = rank_server, function = query_world, protocol = true, alias = false},
                read = [],
                write = [
                    #list{name = list, comment = "排行榜", explain = #rank{
                        type = #u16{comment = "类型"},
                        key = #u64{comment = "键"},
                        value = #u64{comment = "值"},
                        time = #u32{comment = "时间"},
                        order = #u64{comment = "排名"},
                        name = #bst{comment = "名字"},
                        server_id = #u16{comment = "服务器ID"},
                        other = #tuple{explain = {
                            #u16{name = level, comment = "等级"},
                            #u8{name = classes, comment = "职业"},
                            #u8{name = sex, comment = "性别"}
                        }}
                    }}
                ]
            },
            #io{
                protocol = 19204,
                comment = "Rank",
                handler = #handler{module = rank_server, function = query_world, protocol = true, alias = false},
                read = [],
                write = [
                    #list{name = list, comment = "排行榜", explain = #rank{
                        type = #u16{comment = "类型"},
                        key = #u64{comment = "键"},
                        value = #u64{comment = "值"},
                        time = #u32{comment = "时间"},
                        order = #u64{comment = "排名"},
                        name = #bst{comment = "名字"},
                        server_id = #u16{comment = "服务器ID"},
                        other = #tuple{explain = {
                            #u16{name = level, comment = "等级"},
                            #u8{name = classes, comment = "职业"},
                            #u8{name = sex, comment = "性别"},
                            #u8{name = vip_level, comment = "VIP等级"}
                        }}
                    }}
                ]
            },
            #io{
                protocol = 19205,
                comment = "Rank",
                handler = #handler{module = rank_server, function = query_world, protocol = true, alias = false},
                read = [],
                write = [
                    #list{name = list, comment = "排行榜", explain = #rank{
                        type = #u16{comment = "类型"},
                        key = #u64{comment = "键"},
                        value = #u64{comment = "值"},
                        time = #u32{comment = "时间"},
                        order = #u64{comment = "排名"},
                        name = #bst{comment = "名字"},
                        server_id = #u16{comment = "服务器ID"},
                        other = #tuple{explain = {
                            #u16{name = level, comment = "等级"},
                            #u8{name = classes, comment = "职业"},
                            #u8{name = sex, comment = "性别"},
                            #u8{name = vip_level, comment = "VIP等级"},
                            #u8{name = avatar, comment = "头像"}
                        }}
                    }}
                ]
            }
        ]
    }].
