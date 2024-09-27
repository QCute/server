%%%-------------------------------------------------------------------
%%! +pc unicode
%%% @doc
%%% protocol read write define
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_item).
-export([main/1]).
-include("../../../include/journal.hrl").
-include("../../../include/serialize.hrl").
-include("../../../include/item.hrl").
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
        number = 111,
        comment = "物品",
        erl = "script/make/protocol/erl/item_protocol.erl",
        html = "script/make/protocol/html/ItemProtocol.html",
        lua = "script/make/protocol/lua/ItemProtocol.lua",
        js = "script/make/protocol/js/ItemProtocol.js",
        cs = "script/make/protocol/cs/ItemProtocol.cs",
        io = [
            #io{
                number = 11101,
                comment = "道具列表",
                handler = #handler{module = item, function = query_item},
                decode = [],
                encode = [
                    #item{
                        item_no = u64(),   %% 物品编号
                        item_id = u64(),   %% 物品ID
                        type = u8(),       %% 类型
                        number = u16()     %% 数量
                    }
                ],
                encode = [
                    #list{name = list, comment = "道具列表", explain = #item{
                        item_no = #u64{comment = "物品编号"},
                        item_id = #u32{comment = "物品ID"},
                        type = #u8{comment = "类型"},
                        number = #u16{comment = "数量"}
                    }}
                ]
            },
            #io{
                number = 11102,
                comment = "背包列表",
                handler = #handler{module = item, function = query_bag},
                decode = [],
                encode = [
                    #list{name = list, comment = "背包列表", explain = #item{
                        item_no = #u64{comment = "物品编号"},
                        item_id = #u32{comment = "物品ID"},
                        type = #u8{comment = "类型"},
                        number = #u16{comment = "数量"}
                    }}
                ]
            },
            #io{
                number = 11103,
                comment = "仓库列表",
                handler = #handler{module = item, function = query_store},
                decode = [],
                encode = [
                    #list{name = list, comment = "仓库列表", explain = #item{
                        item_no = #u64{comment = "物品编号"},
                        item_id = #u32{comment = "物品ID"},
                        type = #u8{comment = "类型"},
                        number = #u16{comment = "数量"}
                    }}
                ]
            },
            #io{
                number = 11104,
                comment = "删除物品",
                handler = #handler{alias = "delete"},
                encode = [
                    #list{name = list, comment = "删除列表", explain = #item{
                        item_no = #u64{comment = "物品编号"},
                        type = #u8{comment = "类型"}
                    }}
                ]
            },
            #io{
                number = 11106,
                comment = "使用物品",
                handler = #handler{module = item_use, function = use},
                decode = [
                    #u64{name = item_no, comment = "物品编号"},
                    #u16{name = number, comment = "数量"},
                    #u8{name = type, comment = "类型"}
                ],
                encode = [
                    #rst{name = result, comment = "结果"}
                ]
            }
        ]
    }.
