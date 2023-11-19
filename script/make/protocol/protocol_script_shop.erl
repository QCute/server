%%%-------------------------------------------------------------------
%%! +pc unicode -pa beam
%%% @doc
%%% protocol read write define
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_shop).
-mode(compile).
-compile({parse_transform, protocol_maker_transform}).
-export([main/1]).
-include("../../../include/journal.hrl").
-include("../../../include/serialize.hrl").
-include("../../../include/shop.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
main(_) ->
    io:setopts([{encoding, unicode}]),
    io:setopts(standard_error, [{encoding, unicode}]),
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    ets:insert(ets:new(shell_records, [set, public]), [{Tag, Form} || Form = {attribute, _, record, {Tag, _}} <- lists:append([element(2, epp:parse_file(Header, [], [])) || Header <- filelib:wildcard(filename:dirname(escript:script_name()) ++ "/../../../include/*.hrl")])]),
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
        number = 113,
        comment = "商店",
        erl = "script/make/protocol/erl/shop_protocol.erl",
        html = "script/make/protocol/html/ShopProtocol.html",
        lua = "script/make/protocol/lua/ShopProtocol.lua",
        js = "script/make/protocol/js/ShopProtocol.js",
        cs = "script/make/protocol/cs/ShopProtocol.cs",
        io = [
            #io{
                number = 11301,
                comment = "已购列表",
                handler = #handler{module = shop, function = query},
                decode = {},
                encode = [                                 %% 已购买列表
                    #shop{
                        shop_id = u32(),                   %% 商店ID
                        number = u16()                     %% 数量
                    }
                ]
            },
            #io{
                number = 11302,
                comment = "购买",
                handler = #handler{module = shop, function = buy},
                decode = {
                    shop_id = u32(),                       %% 商店ID
                    number = u16()                         %% 数量
                },
                encode = ast()                             %% 结果
            }
        ]
    }.
