%%%------------------------------------------------------------------
%%% @doc
%%% module protocol read write
%%% @end
%%%------------------------------------------------------------------
-module(protocol_script_shop).
-export([main/1]).
-include("../../../include/serialize.hrl").
-include("../../../include/shop.hrl").
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
                        number = #u16{comment = "数量"}
                    }}
                ]
            },
            #io{
                name = 11302,
                comment = "购买",
                handler = #handler{module = shop, function = buy},
                text = [{number_invalid, "购买数量错误"}, {configure_not_found, "配置错误"}, {level_not_enough, "等级不满足"}, {vip_level_not_enough, "Vip等级不满足"}, {buy_max, "已达到购买上限"}, {asset_not_enough, "资产不足"}],
                read = [
                    #u32{name = shop_id, comment = "商店ID"},
                    #u16{name = number, comment = "数量"}
                ],
                write = [
                    #rst{name = result, comment = "结果"}
                ]
            }
        ]
    }].
