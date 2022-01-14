%%%-------------------------------------------------------------------
%%! +pc unicode
%%% @doc
%%% protocol read write define
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_notice).
-export([main/1]).
-include("../../../include/journal.hrl").
-include("../../../include/serialize.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
main(_) ->
    io:setopts([{encoding, unicode}]),
    io:setopts(standard_error, [{encoding, unicode}]),
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
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
        number = 500,
        erl = "src/module/notice/notice_protocol.erl",
        js = "script/make/protocol/js/NoticeProtocol.js",
        lua = "script/make/protocol/lua/NoticeProtocol.lua",
        includes = [],
        io = [
            #io{
                protocol = 50001,
                handler = #handler{alias = "broadcast"},
                comment = "公告",
                write = [
                    #u8{name = scope, comment = "范围"},
                    #u8{name = type, comment = "类型"},
                    #bst{name = title, comment = "标题"},
                    #bst{name = msg, comment = "消息"}
                ]
            }
        ]
    }.
