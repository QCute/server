%%%-------------------------------------------------------------------
%%% @doc
%%% module protocol read write
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_chat).
-export([main/1]).
-include("../../../include/serialize.hrl").
%%%===================================================================
%%% API
%%%===================================================================
main([]) ->
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    console:stacktrace(catch protocol_maker:start([protocol()]));
main(_) ->
    io:format("invalid argument~n").

%%%===================================================================
%%% protocol config
%%%===================================================================
protocol() ->
    #protocol{
        name = 116,
        handler = "src/module/chat/chat_handler.erl",
        erl = "src/module/chat/chat_protocol.erl",
        json = "script/make/protocol/json/ChatProtocol.js",
        lua = "script/make/protocol/lua/ChatProtocol.lua",
        includes = [],
        io = [
            #io{
                name = 11601,
                comment = "Chat World",
                handler = #handler{module = chat, function = world},
                read = [
                    #bst{name = msg, comment = "消息"}
                ],
                write = [
                    #u64{name = user_id, comment = "角色ID"},
                    #bst{name = user_name, comment = "角色名字"},
                    #bst{name = msg, comment = "消息"}
                ]
            },
            #io{
                name = 11602,
                comment = "Chat Guild",
                handler = #handler{module = chat, function = guild},
                read = [
                    #bst{name = msg, comment = "消息"}
                ],
                write = [
                    #u64{name = user_id, comment = "角色ID"},
                    #bst{name = user_name, comment = "角色名字"},
                    #bst{name = msg, comment = "消息"}
                ]
            },
            #io{
                name = 11603,
                comment = "私聊",
                handler = #handler{module = chat, function = private},
                read = [
                    #u64{name = user_id, comment = "角色ID"},
                    #bst{name = msg, comment = "消息"}
                ],
                write = [
                    #u64{name = user_id, comment = "角色ID"},
                    #bst{name = user_name, comment = "角色名字"},
                    #bst{name = msg, comment = "消息"}
                ]
            }
        ]
    }.
