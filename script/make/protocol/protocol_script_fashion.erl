%%%-------------------------------------------------------------------
%%% @doc
%%% protocol read write define
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_fashion).
-export([main/1]).
-include("../../../include/serialize.hrl").
-include("../../../include/fashion.hrl").
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
        number = 120,
        handler = "src/module/fashion/fashion_handler.erl",
        erl = "src/module/fashion/fashion_protocol.erl",
        js = "script/make/protocol/js/FashionProtocol.js",
        lua = "script/make/protocol/lua/FashionProtocol.lua",
        includes = ["fashion.hrl"],
        io = [
            #io{
                protocol = 12001,
                comment = "时装列表",
                handler = #handler{module = fashion, function = query},
                read = [],
                write = [
                    #list{name = list, comment = "时装列表", explain = #fashion{
                        fashion_id = #u32{comment = "时装ID"},
                        expire_time = #u32{comment = "过期时间"}
                    }}
                ]
            },
            #io{
                protocol = 12002,
                handler = #handler{alias = "delete"},
                comment = "删除时装",
                write = [
                    #list{name = list, comment = "时装ID列表", explain = #fashion{
                        fashion_id = #u32{comment = "时装ID"}
                    }}
                ]
            }
        ]
    }].
