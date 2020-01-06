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
    io:format("~p~n", [catch protocol_maker:start(protocol())]);
main(_) ->
    io:format("invalid argument~n").

%%%==================================================================
%%% protocol config
%%%==================================================================
protocol() ->
    [#protocol{
        number = 150,
        handler = "src/module/key/key_handler.erl",
        erl = "src/module/key/key_protocol.erl",
        json = "script/make/protocol/json/KeyProtocol.js",
        lua = "script/make/protocol/lua/KeyProtocol.lua",
        includes = [],
        io = [
            #io{
                protocol = 15001,
                comment = "Key Award",
                handler = #handler{module = key_server, function = award},
                text = [{key_already_active, "此兑换码已经兑换过了"}, {timeout, "请求超时"}],
                read = [
                    #bst{name = key, comment = "兑换码"}
                ],
                write = [
                    #rst{name = result, comment = "结果"}
                ]
            }
        ]
    }].
