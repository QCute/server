%%%------------------------------------------------------------------
%%% @doc
%%% module protocol read write
%%% @end
%%%------------------------------------------------------------------
-module(protocol_script_key).
-export([main/1]).
-include("../../../include/serialize.hrl").
-include("../../../include/rank.hrl").
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
        name = 150,
        handler = "src/module/key/key_handler.erl",
        erl = "src/module/key/key_protocol.erl",
        json = "script/make/protocol/json/KeyProtocol.js",
        lua = "script/make/protocol/lua/KeyProtocol.lua",
        includes = [],
        io = [
            #io{
                name = 15001,
                comment = "Key Award",
                handler = #handler{module = key_server, function = award},
                read = [
                    #bst{name = key, comment = "兑换码"}
                ],
                write = [
                    #u8{name = result, comment = "结果"}
                ]
            }
        ]
    }.
