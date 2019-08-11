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
    Protocol = #protocol{file = File} = protocol(),
    console:stacktrace(catch protocol_maker:start([{File, Protocol}]));
main(_) ->
    io:format("invail argument~n").

%%%===================================================================
%%% protocol config
%%%===================================================================
protocol() ->
    #protocol{
        name = 161,
        file = "src/module/auction/auction_protocol.erl",
        include = ["auction.hrl"],
        io = [
            #io{
                name = 16101,
                comment = "list",
                read = [],
                write = [
                    #ets{name = list, desc = #auction{
                        unique_id = #u64{},
                        auction_id = #u32{},
                        number = #u16{},
                        price = #u32{},
                        end_time = #u32{},
                        bidder_id = #u64{},
                        bidder_name = #bst{}
                    }}
                ]
            },
            #io{
                name = 16102,
                comment = "bid",
                read = [
                    #u64{}                        %% 唯一ID
                ],
                write = [
                    #u8{name = result},           %% 错误码 (0)错误 (1)成功 (2)价钱变化 (3)无此拍卖 (4)钱不够 (5)超时
                    #u32{name = new_price},
                    #auction{
                        unique_id = #u64{},
                        auction_id = #u32{},
                        number = #u16{},
                        price = #u32{},
                        end_time = #u32{},
                        bidder_id = #u64{},
                        bidder_name = #bst{}
                    }
                ]
            }
        ]
    }.
