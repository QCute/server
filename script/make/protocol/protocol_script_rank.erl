%%%------------------------------------------------------------------
%%% @doc
%%% module protocol read write
%%% @end
%%%------------------------------------------------------------------
-module(protocol_script_rank).
-export([main/1]).
-include("../../../include/serialize.hrl").
-include("../../../include/rank.hrl").
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
        name = 190,
        handler = "src/module/rank/rank_handler.erl",
        erl = "src/module/rank/rank_protocol.erl",
        json = "script/make/protocol/json/RankProtocol.js",
        lua = "script/make/protocol/lua/RankProtocol.lua",
        includes = ["rank.hrl"],
        io = [
            #io{
                name = 19001,
                comment = "Rank",
                handler = #handler{arg = [], module = rank_server, function = query},
                read = [
                    #u8{name = rank_type}
                ],
                write = [
                    #list{name = list, comment = "排行榜", explain = #rank{
                        type = #u16{comment = "类型"},
                        key = #u64{comment = "键"},
                        value = #u64{comment = "值"},
                        time = #u32{comment = "时间"},
                        rank = #u64{comment = "排名"},
                        name = #bst{comment = "名字"}
                    }}
                ]
            },
            #io{
                name = 19002,
                comment = "Rank",
                handler = #handler{arg = [], module = rank_server, function = query},
                read = [
                    #u8{name = rank_type}
                ],
                write = [
                    #list{name = list, comment = "排行榜", explain = #rank{
                        type = #u16{comment = "类型"},
                        key = #u64{comment = "键"},
                        value = #u64{comment = "值"},
                        time = #u32{comment = "时间"},
                        rank = #u64{comment = "排名"},
                        name = #bst{comment = "名字"},
                        other = #tuple{explain = {
                            #u16{name = level, comment = "等级"},
                            #u8{name = classes, comment = "职业"}
                        }}
                    }}
                ]
            },
            #io{
                name = 19003,
                comment = "Rank",
                handler = #handler{arg = [], module = rank_server, function = query},
                read = [
                    #u8{name = rank_type}
                ],
                write = [
                    #list{name = list, comment = "排行榜", explain = #rank{
                        type = #u16{comment = "类型"},
                        key = #u64{comment = "键"},
                        value = #u64{comment = "值"},
                        time = #u32{comment = "时间"},
                        rank = #u64{comment = "排名"},
                        name = #bst{comment = "名字"},
                        other = #tuple{explain = {
                            #u16{name = level, comment = "等级"},
                            #u8{name = classes, comment = "职业"},
                            #u8{name = sex, comment = "性别"}
                        }}
                    }}
                ]
            },
            #io{
                name = 19004,
                comment = "Rank",
                handler = #handler{arg = [], module = rank_server, function = query},
                read = [
                    #u8{name = rank_type}
                ],
                write = [
                    #list{name = list, comment = "排行榜", explain = #rank{
                        type = #u16{comment = "类型"},
                        key = #u64{comment = "键"},
                        value = #u64{comment = "值"},
                        time = #u32{comment = "时间"},
                        rank = #u64{comment = "排名"},
                        name = #bst{comment = "名字"},
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
                name = 19005,
                comment = "Rank",
                handler = #handler{arg = [], module = rank_server, function = query},
                read = [
                    #u8{name = rank_type}
                ],
                write = [
                    #list{name = list, comment = "排行榜", explain = #rank{
                        type = #u16{comment = "类型"},
                        key = #u64{comment = "键"},
                        value = #u64{comment = "值"},
                        time = #u32{comment = "时间"},
                        rank = #u64{comment = "排名"},
                        name = #bst{comment = "名字"},
                        digest = #binary{name = well, comment = "嗯", explain = 64},
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
    }.
