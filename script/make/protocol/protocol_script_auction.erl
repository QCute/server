%%%------------------------------------------------------------------
%%% @doc
%%% module protocol read write
%%% @end
%%%------------------------------------------------------------------
-module(protocol_script_auction).
-export([main/1]).
-include("../../../include/serialize.hrl").
-include("../../../include/auction.hrl").
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
        number = 161,
        handler = "src/module/auction/auction_handler.erl",
        erl = "src/module/auction/auction_protocol.erl",
        json = "script/make/protocol/json/AuctionProtocol.js",
        lua = "script/make/protocol/lua/AuctionProtocol.lua",
        includes = ["auction.hrl"],
        io = [
            #io{
                protocol = 16101,
                comment = "拍品列表",
                handler = #handler{arg = [], module = auction_server, function = query},
                read = [],
                write = [
                    #ets{name = list, comment = "拍品列表", explain = #auction{
                        unique_id = #u64{comment = "唯一ID"},
                        auction_id = #u32{comment = "拍品ID"},
                        number = #u16{comment = "数量"},
                        price = #u32{comment = "价格"},
                        end_time = #u32{comment = "结束时间"},
                        role_id = #u64{comment = "竞拍者"},
                        role_name = #bst{comment = "竞拍者名"}
                    }}
                ]
            },
            #io{
                protocol = 16102,
                comment = "竞价",
                handler = #handler{module = auction_server, function = bid},
                text = [{gold_not_enough, "元宝不足"}, {timeout, "请求超时"}, {price_change, "价格已变化"}, {no_such_auction, "没有此拍品"}],
                read = [
                    #u64{name = unique_id, comment = "唯一ID"}
                ],
                write = [
                    #rst{name = result, comment = "结果"},
                    #u32{name = new_price, comment = "新的价格"},
                    #auction{
                        unique_id = #u64{comment = "唯一ID"},
                        auction_id = #u32{comment = "拍品ID"},
                        number = #u16{comment = "数量"},
                        price = #u32{comment = "价格"},
                        end_time = #u32{comment = "结束时间"},
                        role_id = #u64{comment = "竞拍者"},
                        role_name = #bst{comment = "竞拍者名"}
                    }
                ]
            }
        ]
    }].
