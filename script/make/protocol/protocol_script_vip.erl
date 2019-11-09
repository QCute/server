%%%------------------------------------------------------------------
%%% @doc
%%% module protocol read write
%%% @end
%%%------------------------------------------------------------------
-module(protocol_script_vip).
-export([main/1]).
-include("../../../include/serialize.hrl").
-include("../../../include/vip.hrl").
%%%==================================================================
%%% API functions
%%%==================================================================
main([]) ->
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    console:stacktrace(catch protocol_maker:start([protocol()]));
main(_) ->
    io:format("invalid argument~n").

%%%==================================================================
%%% protocol config
%%%==================================================================
protocol() ->
    #protocol{
        name = 103,
        handler = "src/module/vip/vip_handler.erl",
        erl = "src/module/vip/vip_protocol.erl",
        json = "script/make/protocol/json/VipProtocol.js",
        lua = "script/make/protocol/lua/VipProtocol.lua",
        includes = ["vip.hrl"],
        io = [
            #io{
                name = 10301,
                comment = "vip",
                handler = #handler{module = vip, function = query},
                read = [],
                write = [
                    #vip{
                        vip_level = #u8{comment = "等级"},
                        exp = #u64{comment = "经验"},
                        expire_time = #u32{comment = "过期时间"}
                    }
                ]
            }
        ]
    }.
