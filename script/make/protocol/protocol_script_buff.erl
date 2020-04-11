%%%-------------------------------------------------------------------
%%% @doc
%%% module protocol read write
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_buff).
-export([main/1]).
-include("../../../include/serialize.hrl").
-include("../../../include/buff.hrl").
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
        number = 118,
        handler = "src/module/buff/buff_handler.erl",
        erl = "src/module/buff/buff_protocol.erl",
        json = "script/make/protocol/json/BuffProtocol.js",
        lua = "script/make/protocol/lua/BuffProtocol.lua",
        includes = ["buff.hrl"],
        io = [
            #io{
                protocol = 11801,
                comment = "Buff列表",
                handler = #handler{module = buff, function = query},
                read = [],
                write = [
                    #list{name = list, comment = "Buff列表", explain = #buff{
                        buff_id = #u32{comment = "BuffID"},
                        expire_time = #u32{comment = "结束时间"},
                        overlap = #u16{comment = "叠加数量"}
                    }}
                ]
            },
            #io{
                protocol = 11802,
                comment = "删除Buff列表",
                write = [
                    #list{name = list, comment = "Buff列表", explain = #buff{
                        buff_id = #u32{comment = "BuffID"}
                    }}
                ]
            }
        ]
    }].
