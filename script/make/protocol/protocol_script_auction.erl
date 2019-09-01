%%%-------------------------------------------------------------------
%%% @doc
%%% module protocol read write
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_auction).
-export([main/1]).
-include("../../../include/serialize.hrl").
-include("../../../include/auction.hrl").
%%%===================================================================
%%% API
%%%===================================================================
main([]) ->
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    console:stacktrace(catch protocol_maker:start([protocol()]));
main(_) ->
    io:format("invail argument~n").

%%%===================================================================
%%% protocol config
%%%===================================================================
protocol() ->
    #protocol{
        name = 161,
        handler = "src/module/auction/auction_handler.erl",
        erl = "src/module/auction/auction_protocol.erl",
        json = "script/make/protocol/json/AuctionProtocol.js",
        lua = "script/make/protocol/lua/AuctionProtocol.lua",
        includes = ["auction.hrl"],
        io = [
            #io{
                name = 16101,
                comment = "list",
                read = [],
                write = [
                    #ets{name = list, comment = "拍品列表", explain = #auction{
                        unique_id = #u64{comment = "唯一ID"},
                        auction_id = #u32{comment = "拍品ID"},
                        number = #u16{comment = "数量"},
                        price = #u32{comment = "价格"},
                        end_time = #u32{comment = "结束时间"},
                        bidder_id = #u64{comment = "竞拍者"},
                        bidder_name = #bst{comment = "竞拍者名"}
                    }}
                ],
                handler = #handler{
                    state_name = [],
                    module = auction_server,
                    function = query
                }
            },
            #io{
                name = 16102,
                comment = "bid",
                read = [
                    #u64{name = unique_id, comment = "唯一ID"}
                ],
                write = [
                    #u8{name = result, comment = "结果"},
                    #u32{name = new_price, comment = "新的价格"},
                    #auction{
                        unique_id = #u64{comment = "唯一ID"},
                        auction_id = #u32{comment = "拍品ID"},
                        number = #u16{comment = "数量"},
                        price = #u32{comment = "价格"},
                        end_time = #u32{comment = "结束时间"},
                        bidder_id = #u64{comment = "竞拍者"},
                        bidder_name = #bst{comment = "竞拍者名"}
                    }
                ],
                handler = #handler{
                    module = auction_server,
                    function = bid
                }
            }
        ]
    }.
