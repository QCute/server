%%%-------------------------------------------------------------------
%%% @doc
%%% module protocol read write
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_rank_center).
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
        number = 191,
        handler = "src/module/rank/rank_center_handler.erl",
        erl = "src/module/rank/rank_center_protocol.erl",
        js = "script/make/protocol/js/RankCenterProtocol.js",
        lua = "script/make/protocol/lua/RankCenterProtocol.lua",
        includes = ["rank.hrl"],
        io = [
            #io{
                protocol = 19101,
                comment = "Rank",
                handler = #handler{module = rank_server, function = query_center, protocol = 1},
                read = [],
                write = [
                    #list{name = list, comment = "排行榜", explain = #rank{
                        type = #u16{comment = "类型"},
                        key = #u64{comment = "键"},
                        value = #u64{comment = "值"},
                        time = #u32{comment = "时间"},
                        rank = #u64{comment = "排名"},
                        name = #bst{comment = "名字"},
                        server_id = #u16{comment = "服务器ID"}
                    }}
                ]
            },
            #io{
                protocol = 19102,
                comment = "Rank",
                handler = #handler{module = rank_server, function = query_center, protocol = 1},
                read = [],
                write = [
                    #list{name = list, comment = "排行榜", explain = #rank{
                        type = #u16{comment = "类型"},
                        key = #u64{comment = "键"},
                        value = #u64{comment = "值"},
                        time = #u32{comment = "时间"},
                        rank = #u64{comment = "排名"},
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
                protocol = 19103,
                comment = "Rank",
                handler = #handler{module = rank_server, function = query_center, protocol = 1},
                read = [],
                write = [
                    #list{name = list, comment = "排行榜", explain = #rank{
                        type = #u16{comment = "类型"},
                        key = #u64{comment = "键"},
                        value = #u64{comment = "值"},
                        time = #u32{comment = "时间"},
                        rank = #u64{comment = "排名"},
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
                protocol = 19104,
                comment = "Rank",
                handler = #handler{module = rank_server, function = query_center, protocol = 1},
                read = [],
                write = [
                    #list{name = list, comment = "排行榜", explain = #rank{
                        type = #u16{comment = "类型"},
                        key = #u64{comment = "键"},
                        value = #u64{comment = "值"},
                        time = #u32{comment = "时间"},
                        rank = #u64{comment = "排名"},
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
                protocol = 19105,
                comment = "Rank",
                handler = #handler{module = rank_server, function = query_center, protocol = 1},
                read = [],
                write = [
                    #list{name = list, comment = "排行榜", explain = #rank{
                        type = #u16{comment = "类型"},
                        key = #u64{comment = "键"},
                        value = #u64{comment = "值"},
                        time = #u32{comment = "时间"},
                        rank = #u64{comment = "排名"},
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
