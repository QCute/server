%%%-------------------------------------------------------------------
%%! +pc unicode -pa beam
%%% @doc
%%% protocol read write define
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_rank_center).
-export([main/1]).
-include("../../../include/journal.hrl").
-include("../../../include/serialize.hrl").
-include("../../../include/rank.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
main(_) ->
    io:setopts([{encoding, unicode}]),
    io:setopts(standard_error, [{encoding, unicode}]),
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
        number = 191,
        comment = "排行榜-中心服",
        erl = "script/make/protocol/erl/rank_center_protocol.erl",
        html = "script/make/protocol/html/RankCenterProtocol.html",
        lua = "script/make/protocol/lua/RankCenterProtocol.lua",
        js = "script/make/protocol/js/RankCenterProtocol.js",
        cs = "script/make/protocol/cs/RankCenterProtocol.cs",
        io = [
            #io{
                number = 19101,
                comment = "等级榜",
                handler = #handler{module = rank_server, function = query_center, alias = level, protocol = true},
                decode = [],
                encode = [
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
                number = 19102,
                comment = "战力榜",
                handler = #handler{module = rank_server, function = query_center, alias = fight, protocol = true},
                decode = [],
                encode = [
                    #list{name = list, comment = "排行榜", explain = #rank{
                        type = #u16{comment = "类型"},
                        key = #u64{comment = "键"},
                        value = #u64{comment = "值"},
                        time = #u32{comment = "时间"},
                        order = #u64{comment = "排名"},
                        name = #bst{comment = "名字"},
                        server_id = #u16{comment = "服务器ID"},
                        other = #tuple{
                            name = other,
                            explain = {
                                #u16{name = level, comment = "等级"},
                                #u8{name = classes, comment = "职业"}
                            }
                        }
                    }}
                ]
            },
            #io{
                number = 19103,
                comment = "成就榜",
                handler = #handler{module = rank_server, function = query_center, alias = achievement, protocol = true},
                decode = [],
                encode = [
                    #list{name = list, comment = "排行榜", explain = #rank{
                        type = #u16{comment = "类型"},
                        key = #u64{comment = "键"},
                        value = #u64{comment = "值"},
                        time = #u32{comment = "时间"},
                        order = #u64{comment = "排名"},
                        name = #bst{comment = "名字"},
                        server_id = #u16{comment = "服务器ID"},
                        other = #tuple{
                            name = other,
                            explain = {
                                #u16{name = level, comment = "等级"},
                                #u8{name = classes, comment = "职业"},
                                #u8{name = sex, comment = "性别"}
                            }
                        }
                    }}
                ]
            },
            #io{
                number = 19104,
                comment = "财富榜",
                handler = #handler{module = rank_server, function = query_center, alias = wealth, protocol = true},
                decode = [],
                encode = [
                    #list{name = list, comment = "排行榜", explain = #rank{
                        type = #u16{comment = "类型"},
                        key = #u64{comment = "键"},
                        value = #u64{comment = "值"},
                        time = #u32{comment = "时间"},
                        order = #u64{comment = "排名"},
                        name = #bst{comment = "名字"},
                        server_id = #u16{comment = "服务器ID"},
                        other = #tuple{
                            name = other,
                            explain = {
                                #u16{name = level, comment = "等级"},
                                #u8{name = classes, comment = "职业"},
                                #u8{name = sex, comment = "性别"},
                                #u8{name = vip_level, comment = "VIP等级"}
                            }
                        }
                    }}
                ]
            },
            #io{
                number = 19105,
                comment = "职业榜",
                handler = #handler{module = rank_server, function = query_center, alias = classes, protocol = true},
                decode = [],
                encode = [
                    #list{name = list, comment = "排行榜", explain = #rank{
                        type = #u16{comment = "类型"},
                        key = #u64{comment = "键"},
                        value = #u64{comment = "值"},
                        time = #u32{comment = "时间"},
                        order = #u64{comment = "排名"},
                        name = #bst{comment = "名字"},
                        server_id = #u16{comment = "服务器ID"},
                        other = #tuple{
                            name = other,
                            explain = {
                                #u16{name = level, comment = "等级"},
                                #u8{name = classes, comment = "职业"},
                                #u8{name = sex, comment = "性别"},
                                #u8{name = vip_level, comment = "VIP等级"},
                                #u8{name = avatar, comment = "头像"}
                            }
                        }
                    }}
                ]
            }
        ]
    }.
