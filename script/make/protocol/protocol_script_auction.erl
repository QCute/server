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
        number = 161,
        handler = "src/module/auction/auction_handler.erl",
        erl = "src/module/auction/auction_protocol.erl",
        js = "script/make/protocol/js/AuctionProtocol.js",
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
                        auction_no = #u64{comment = "拍品编号"},
                        auction_id = #u32{comment = "拍品ID"},
                        type = #u8{comment = "拍卖类型(1:全服/2:公会)"},
                        number = #u16{comment = "数量"},
                        now_price = #u32{comment = "当前价格"},
                        next_price = #u32{comment = "下次出价的价格"},
                        end_time = #u32{comment = "结束时间"}
                    }}
                ]
            },
            #io{
                protocol = 16102,
                comment = "竞价",
                handler = #handler{module = auction_server, function = bid},
                read = [
                    #u64{name = auction_no, comment = "拍品编号"},
                    #u32{name = next_price, comment = "新的价格"}
                ],
                write = [
                    #rst{name = result, comment = "结果"},
                    #u32{name = new_price, comment = "新的价格"},
                    #auction{
                        auction_no = #u64{comment = "拍品编号"},
                        auction_id = #u32{comment = "拍品ID"},
                        type = #u8{comment = "拍卖类型(1:全服/2:公会)"},
                        now_price = #u32{comment = "当前价格"},
                        next_price = #u32{comment = "下次出价的价格"},
                        end_time = #u32{comment = "结束时间"}
                    }
                ]
            }
        ]
    }].
