%%%-------------------------------------------------------------------
%%% @doc
%%% module protocol read write
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_chat).
-export([main/1]).
-include("../../../include/serialize.hrl").
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
        number = 116,
        handler = "src/module/chat/chat_handler.erl",
        erl = "src/module/chat/chat_protocol.erl",
        js = "script/make/protocol/js/ChatProtocol.js",
        lua = "script/make/protocol/lua/ChatProtocol.lua",
        includes = [],
        io = [
            #io{
                protocol = 11601,
                comment = "世界聊天",
                handler = #handler{module = chat, function = world},
                text = [{level_not_enough, "等级不足"}, {time_in_cd, "时间冷却中"}],
                read = [
                    #bst{name = msg, comment = "消息"}
                ],
                write = [
                    #rst{name = result, comment = "结果"},
                    #u64{name = user_id, comment = "角色ID"},
                    #bst{name = user_name, comment = "角色名字"},
                    #bst{name = msg, comment = "消息"}
                ]
            },
            #io{
                protocol = 11602,
                comment = "公会聊天",
                handler = #handler{module = chat, function = guild},
                text = [{level_not_enough, "等级不足"}, {no_guild, "没加入公会"}, {time_in_cd, "时间冷却中"}],
                read = [
                    #bst{name = msg, comment = "消息"}
                ],
                write = [
                    #rst{name = result, comment = "结果"},
                    #u64{name = user_id, comment = "角色ID"},
                    #bst{name = user_name, comment = "角色名字"},
                    #bst{name = msg, comment = "消息"}
                ]
            },
            #io{
                protocol = 11603,
                comment = "私聊",
                handler = #handler{module = chat, function = private},
                text = [{level_not_enough, "等级不足"}, {user_offline, "对方不在线"}],
                read = [
                    #u64{name = user_id, comment = "角色ID"},
                    #bst{name = msg, comment = "消息"}
                ],
                write = [
                    #rst{name = result, comment = "结果"},
                    #u64{name = user_id, comment = "角色ID"},
                    #bst{name = user_name, comment = "角色名字"},
                    #bst{name = msg, comment = "消息"}
                ]
            }
        ]
    }].
