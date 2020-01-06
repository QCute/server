%%%------------------------------------------------------------------
%%% @doc
%%% module protocol read write
%%% @end
%%%------------------------------------------------------------------
-module(protocol_script_cheat).
-export([main/1]).
-include("../../../include/serialize.hrl").
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
        number = 600,
        handler = "src/module/cheat/cheat_handler.erl",
        erl = "src/module/cheat/cheat_protocol.erl",
        lua = "script/make/protocol/lua/CheatProtocol.lua",
        json = "script/make/protocol/json/CheatProtocol.js",
        includes = [],
        io = [
            #io{
                protocol = 60000,
                comment = "秘籍",
                handler = #handler{module = cheat, function = cheat},
                text = [{no_such_command, "没有找到命令"}],
                read = [
                    #str{name = command, comment = "命令"}
                ],
                write = [
                    #rst{name = result, comment = "结果"},
                    #str{name = command, comment = "命令"}
                ]
            }
        ]
    }].
