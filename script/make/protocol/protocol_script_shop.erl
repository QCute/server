%%%-------------------------------------------------------------------
%%% @doc
%%% module protocol read write
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_shop).
-export([main/1]).
-include("../../../include/serialize.hrl").
-include("../../../include/shop.hrl").
%%%===================================================================
%%% API
%%%===================================================================
main([]) ->
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    console:stacktrace(catch protocol_maker:start([protocol()]));
main(_) ->
    io:format("invalid argument~n").

%%%===================================================================
%%% protocol config
%%%===================================================================
protocol() ->
    #protocol{
        name = 113,
        handler = "src/module/shop/shop_handler.erl",
        erl = "src/module/shop/shop_protocol.erl",
        json = "script/make/protocol/json/ShopProtocol.js",
        lua = "script/make/protocol/lua/ShopProtocol.lua",
        includes = ["shop.hrl"],
        io = [
            #io{
                name = 11301,
                comment = "已购列表",
                handler = #handler{module = shop, function = query},
                read = [],
                write = [
                    #list{name = list, comment = "已购买列表", explain = #shop{
                        shop_id = #u32{comment = "商店ID"},
                        amount = #u16{comment = "数量"}
                    }}
                ]
            },
            #io{
                name = 11302,
                comment = "购买",
                handler = #handler{module = shop, function = buy},
                read = [
                    #u32{name = shop_id, comment = "商店ID"},
                    #u16{name = amount, comment = "数量"}
                ],
                write = [
                    #u8{name = result, comment = "结果"}
                ]
            }
        ]
    }.
