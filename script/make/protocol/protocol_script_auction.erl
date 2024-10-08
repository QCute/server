%%%-------------------------------------------------------------------
%%! +pc unicode -pa beam
%%% @doc
%%% protocol read write define
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_auction).
-export([main/1]).
-include("../../../include/journal.hrl").
-include("../../../include/serialize.hrl").
-include("../../../include/auction.hrl").
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
        number = 161,
        comment = "拍卖",
        erl = "script/make/protocol/erl/auction_protocol.erl",
        html = "script/make/protocol/html/AuctionProtocol.html",
        lua = "script/make/protocol/lua/AuctionProtocol.lua",
        js = "script/make/protocol/js/AuctionProtocol.js",
        cs = "script/make/protocol/cs/AuctionProtocol.cs",
        io = [
            #io{
                number = 16101,
                comment = "拍品列表",
                handler = #handler{module = auction_server, function = query},
                decode = [],
                encode = [
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
                number = 16102,
                comment = "竞价",
                handler = #handler{module = auction_server, function = bid},
                decode = [
                    #u64{name = auction_no, comment = "拍品编号"},
                    #u32{name = next_price, comment = "新的价格"}
                ],
                encode = [
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
    }.
