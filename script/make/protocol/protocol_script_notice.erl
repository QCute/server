%%%------------------------------------------------------------------
%%% @doc
%%% module protocol read write
%%% @end
%%%------------------------------------------------------------------
-module(protocol_script_notice).
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
        name = 500,
        erl = "src/module/notice/notice_protocol.erl",
        json = "script/make/protocol/json/NoticeProtocol.js",
        lua = "script/make/protocol/lua/NoticeProtocol.lua",
        includes = [],
        io = [
            #io{
                name = 50001,
                comment = "Notice",
                write = [
                    #u8{name = scope, comment = "范围"},
                    #u8{name = type, comment = "类型"},
                    #str{name = msg, comment = "消息"}
                ]
            }
        ]
    }].
