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
        handler = "src/module/item/item_handler.erl",
        erl = "src/module/item/item_protocol.erl",
        js = "script/make/protocol/js/ItemProtocol.js",
        lua = "script/make/protocol/lua/ItemProtocol.lua",
        includes = ["item.hrl"],
        io = [
            #io{
                protocol = 11101,
                comment = "道具列表",
                handler = #handler{module = item, function = query_item},
                read = [],
                write = [
                    #list{name = list, comment = "道具列表", explain = #item{
                        item_no = #u64{comment = "物品编号"},
                        item_id = #u32{comment = "物品ID"},
                        type = #u8{comment = "类型"},
                        number = #u16{comment = "数量"}
                    }}
                ]
            },
            #io{
                protocol = 11102,
                comment = "背包列表",
                handler = #handler{module = item, function = query_bag},
                read = [],
                write = [
                    #list{name = list, comment = "背包列表", explain = #item{
                        item_no = #u64{comment = "物品编号"},
                        item_id = #u32{comment = "物品ID"},
                        type = #u8{comment = "类型"},
                        number = #u16{comment = "数量"}
                    }}
                ]
            },
            #io{
                protocol = 11103,
                comment = "仓库列表",
                handler = #handler{module = item, function = query_store},
                read = [],
                write = [
                    #list{name = list, comment = "仓库列表", explain = #item{
                        item_no = #u64{comment = "物品编号"},
                        item_id = #u32{comment = "物品ID"},
                        type = #u8{comment = "类型"},
                        number = #u16{comment = "数量"}
                    }}
                ]
            },
            #io{
                protocol = 11104,
                comment = "删除物品",
                handler = #handler{alias = "delete"},
                write = [
                    #list{name = list, comment = "删除列表", explain = #item{
                        item_no = #u64{comment = "物品编号"},
                        type = #u8{comment = "类型"}
                    }}
                ]
            },
            #io{
                protocol = 11106,
                comment = "使用物品",
                handler = #handler{module = item_use, function = use},
                read = [
                    #u64{name = item_no, comment = "物品编号"},
                    #u16{name = number, comment = "数量"},
                    #u8{name = type, comment = "类型"}
                ],
                write = [
                    #rst{name = result, comment = "结果"}
                ]
            }
        ]
    }.
