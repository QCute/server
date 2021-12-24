%%%-------------------------------------------------------------------
%%! +pc unicode
%%% @doc
%%% protocol read write define
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_rank).
-export([main/1]).
-include("../../../include/journal.hrl").
-include("../../../include/serialize.hrl").
-include("../../../include/rank.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
main(_) ->
    io:setopts([{encoding, unicode}]),
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    try
        io:format("~tp~n", [protocol_maker:start(protocol())])
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?HALT(Class, Reason, Stacktrace)
    end.

%%%===================================================================
%%% protocol config
%%%===================================================================
protocol() ->
    #protocol{
        number = 190,
        handler = "src/module/rank/rank_handler.erl",
        erl = "src/module/rank/rank_protocol.erl",
        js = "script/make/protocol/js/RankProtocol.js",
        lua = "script/make/protocol/lua/RankProtocol.lua",
        includes = ["rank.hrl"],
        io = [
            #io{
                protocol = 19001,
                comment = "Rank",
                handler = #handler{module = rank_server, function = query, arg = [], protocol = true, alias = false},
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
                protocol = 19002,
                comment = "Rank",
                handler = #handler{module = rank_server, function = query, arg = [], protocol = true, alias = false},
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
                protocol = 19003,
                comment = "Rank",
                handler = #handler{module = rank_server, function = query, arg = [], protocol = true, alias = false},
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
                protocol = 19004,
                comment = "Rank",
                handler = #handler{module = rank_server, function = query, arg = [], protocol = true, alias = false},
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
                protocol = 19005,
                comment = "Rank",
                handler = #handler{module = rank_server, function = query, arg = [], protocol = true, alias = false},
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
    }.
