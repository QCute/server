%%%------------------------------------------------------------------
%%% @doc
%%% module protocol read write
%%% @end
%%%------------------------------------------------------------------
-module(protocol_script_item).
-export([main/1]).
-include("../../../include/serialize.hrl").
-include("../../../include/item.hrl").
%%%==================================================================
%%% API functions
%%%==================================================================
main([]) ->
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    io:format("~p~n", [catch protocol_maker:start(protocol())]);
main(_) ->
    io:format("invalid argument~n").

%%%==================================================================
%%% protocol config
%%%==================================================================
protocol() ->
    [#protocol{
        number = 111,
        handler = "src/module/item/item_handler.erl",
        erl = "src/module/item/item_protocol.erl",
        json = "script/make/protocol/json/ItemProtocol.js",
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
                        unique_id = #u64{comment = "唯一ID"},
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
                        unique_id = #u64{comment = "唯一ID"},
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
                        unique_id = #u64{comment = "唯一ID"},
                        item_id = #u32{comment = "物品ID"},
                        type = #u8{comment = "类型"},
                        number = #u16{comment = "数量"}
                    }}
                ]
            },
            #io{
                protocol = 11104,
                comment = "删除物品",
                write = [
                    #list{name = list, comment = "删除列表", explain = #item{
                        unique_id = #u64{comment = "唯一ID"},
                        type = #u8{comment = "类型"}
                    }}
                ]
            }
        ]
    }].
