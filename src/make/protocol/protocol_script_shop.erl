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
    Protocol = #protocol{file = File} = protocol(),
    console:stacktrace(catch maker:start(fun protocol_maker:parse/2, [{File, Protocol}])),
    ok;
main(_) ->
    io:format("invail argument~n").

%%%===================================================================
%%% protocol config
%%%===================================================================
protocol() ->
    #protocol{
        name = 113,
        file = "src/module/shop/shop_protocol.erl",
        include = ["shop.hrl"],
        io = [
            #io{
                name = 11301,
                comment = "Mail",
                read = [],
                write = [
                    #list{name = list, desc = #shop{                         %% 已购买列表
                        shop_id = #u32{},                                    %% |-- 商店ID
                        amount = #u16{}                                      %% |-- 数量
                    }}
                ]
            }
        ]
    }.
