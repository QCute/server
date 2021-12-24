%%%-------------------------------------------------------------------
%%! +pc unicode
%%% @doc
%%% protocol read write define
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_chat).
-export([main/1]).
-include("../../../include/journal.hrl").
-include("../../../include/serialize.hrl").
-include("../../../include/chat.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
main(_) ->
    io:setopts([{encoding, unicode}]),
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
        number = 116,
        handler = "src/module/chat/chat_handler.erl",
        erl = "src/module/chat/chat_protocol.erl",
        js = "script/make/protocol/js/ChatProtocol.js",
        lua = "script/make/protocol/lua/ChatProtocol.lua",
        includes = ["chat.hrl"],
        io = [
            #io{
                protocol = 11602,
                comment = "系统公告列表",
                handler = #handler{module = chat_server, function = get_system_list, arg = []},
                read = [
                    #u16{name = page, comment = "页"}
                ],
                write = [
                    #list{name = list, explain = #system_chat{
                        id = #u64{comment = "ID"},
                        role_id = #u64{comment = "角色ID"},
                        role_name = #bst{comment = "角色名字"},
                        type = #u8{comment = "类型"},
                        message = #bst{comment = "消息内容"}
                    }}
                ]
            },
            #io{
                protocol = 11603,
                comment = "世界聊天",
                handler = #handler{module = chat, function = world},
                read = [
                    #u8{name = type, comment = "类型"},
                    #bst{name = message, comment = "消息"}
                ],
                write = [
                    #rst{name = result, comment = "结果"},
                    #world_chat{
                        id = #u64{comment = "ID"},
                        role_id = #u64{comment = "角色ID"},
                        role_name = #bst{comment = "角色名字"},
                        type = #u8{comment = "类型"},
                        message = #bst{comment = "消息内容"}
                    }
                ]
            },
            #io{
                protocol = 11604,
                comment = "世界聊天列表",
                handler = #handler{module = chat_server, function = get_world_list, arg = []},
                read = [
                    #u16{name = page, comment = "页"}
                ],
                write = [
                    #list{name = list, explain = #world_chat{
                        id = #u64{comment = "ID"},
                        role_id = #u64{comment = "角色ID"},
                        role_name = #bst{comment = "角色名字"},
                        type = #u8{comment = "类型"},
                        message = #bst{comment = "消息内容"}
                    }}
                ]
            },
            #io{
                protocol = 11605,
                comment = "公会聊天",
                handler = #handler{module = chat, function = guild},
                read = [
                    #u8{name = type, comment = "类型"},
                    #bst{name = message, comment = "消息"}
                ],
                write = [
                    #rst{name = result, comment = "结果"},
                    #guild_chat{
                        id = #u64{comment = "ID"},
                        role_id = #u64{comment = "角色ID"},
                        role_name = #bst{comment = "角色名字"},
                        type = #u8{comment = "类型"},
                        message = #bst{comment = "消息内容"}
                    }
                ]
            },
            #io{
                protocol = 11606,
                comment = "公会聊天列表",
                handler = #handler{module = chat_server, function = get_guild_list},
                read = [
                    #u16{name = page, comment = "页"}
                ],
                write = [
                    #list{name = list, explain = #guild_chat{
                        id = #u64{comment = "ID"},
                        role_id = #u64{comment = "角色ID"},
                        role_name = #bst{comment = "角色名字"},
                        type = #u8{comment = "类型"},
                        message = #bst{comment = "消息内容"}
                    }}
                ]
            },
            #io{
                protocol = 11607,
                comment = "私聊",
                handler = #handler{module = chat, function = private},
                read = [
                    #u64{name = role_id, comment = "角色ID"},
                    #u8{name = type, comment = "类型"},
                    #bst{name = message, comment = "消息"}
                ],
                write = [
                    #rst{name = result, comment = "结果"},
                    #private_chat{
                        sender_id = #u64{comment = "发送者角色ID"},
                        receiver_id = #u64{comment = "接收者角色ID"},
                        type = #u8{comment = "类型"},
                        message = #bst{comment = "消息内容"}
                    }
                ]
            },
            #io{
                protocol = 11608,
                comment = "私聊列表",
                handler = #handler{module = chat_server, function = get_private_list},
                read = [
                    #u64{name = role_id, comment = "角色ID"},
                    #u16{name = page, comment = "页"}
                ],
                write = [
                    #list{name = list, explain = #private_chat{
                        sender_id = #u64{comment = "发送者角色ID"},
                        receiver_id = #u64{comment = "接收者角色ID"},
                        type = #u8{comment = "类型"},
                        message = #bst{comment = "消息内容"}
                    }}
                ]
            }
        ]
    }.
