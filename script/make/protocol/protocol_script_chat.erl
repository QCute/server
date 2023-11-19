%%%-------------------------------------------------------------------
%%! +pc unicode -pa beam
%%% @doc
%%% protocol read write define
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_chat).
-mode(compile).
-compile({parse_transform, protocol_maker_transform}).
-export([main/1]).
-include("../../../include/journal.hrl").
-include("../../../include/serialize.hrl").
-include("../../../include/chat.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
main(_) ->
    io:setopts([{encoding, unicode}]),
    io:setopts(standard_error, [{encoding, unicode}]),
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    ets:insert(ets:new(shell_records, [set, public]), [{Tag, Form} || Form = {attribute, _, record, {Tag, _}} <- lists:append([element(2, epp:parse_file(Header, [], [])) || Header <- filelib:wildcard(filename:dirname(escript:script_name()) ++ "/../../../include/*.hrl")])]),
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
        comment = "聊天",
        erl = "script/make/protocol/erl/chat_protocol.erl",
        html = "script/make/protocol/html/ChatProtocol.html",
        lua = "script/make/protocol/lua/ChatProtocol.lua",
        js = "script/make/protocol/js/ChatProtocol.js",
        cs = "script/make/protocol/cs/ChatProtocol.cs",
        io = [
            #io{
                number = 11602,
                comment = "系统公告列表",
                handler = #handler{module = chat_server, function = get_system_list},
                decode = u16(),                            %% 页
                encode = [
                    #system_chat{
                        id = u64(),                        %% ID
                        role_id = u64(),                   %% 角色ID
                        role_name = bst(),                 %% 角色名字
                        type = u8(),                       %% 类型
                        message = bst()                    %% 消息内容
                    }
                ]
            },
            #io{
                number = 11603,
                comment = "世界聊天",
                handler = #handler{module = chat, function = world},
                decode = {
                    type = u8(),                           %% 类型
                    message = bst()                        %% 消息
                },
                encode = {
                    result = ast(),                        %% 结果
                    world_chat = #world_chat{
                        id = u64(),                        %% ID
                        role_id = u64(),                   %% 角色ID
                        role_name = bst(),                 %% 角色名字
                        type = u8(),                       %% 类型
                        message = bst()                    %% 消息内容
                    }
                }
            },
            #io{
                number = 11604,
                comment = "世界聊天列表",
                handler = #handler{module = chat_server, function = get_world_list},
                decode = u16(),                            %% 页
                encode = [
                    #world_chat{
                        id = u64(),                        %% ID
                        role_id = u64(),                   %% 角色ID
                        role_name = bst(),                 %% 角色名字
                        type = u8(),                       %% 类型
                        message = bst()                    %% 消息内容
                    }
                ]
            },
            #io{
                number = 11605,
                comment = "公会聊天",
                handler = #handler{module = chat, function = guild},
                decode = {
                    type = u8(),                           %% 类型
                    message = bst()                        %% 消息
                },
                encode = {
                    result = ast(),                        %% 结果
                    guild_chat = #guild_chat{
                        id = u64(),                        %% ID
                        role_id = u64(),                   %% 角色ID
                        role_name = bst(),                 %% 角色名字
                        type = u8(),                       %% 类型
                        message = bst()                    %% 消息内容
                    }
                }
            },
            #io{
                number = 11606,
                comment = "公会聊天列表",
                handler = #handler{module = chat_server, function = get_guild_list},
                decode = u16(),                            %% 页
                encode = [
                    #guild_chat{
                        id = u64(),                        %% ID
                        role_id = u64(),                   %% 角色ID
                        role_name = bst(),                 %% 角色名字
                        type = u8(),                       %% 类型
                        message = bst()                    %% 消息内容
                    }
                ]
            },
            #io{
                number = 11607,
                comment = "私聊",
                handler = #handler{module = chat, function = private},
                decode = {
                    role_id = u64(),                       %% 角色ID
                    type = u8(),                           %% 类型
                    message = bst()                        %% 消息
                },
                encode = {
                    result = ast(),                        %% 结果
                    private_chat = #private_chat{
                        sender_id = u64(),                 %% 发送者角色ID
                        receiver_id = u64(),               %% 接收者角色ID
                        type = u8(),                       %% 类型
                        message = bst()                    %% 消息内容
                    }
                }
            },
            #io{
                number = 11608,
                comment = "私聊列表",
                handler = #handler{module = chat_server, function = get_private_list},
                decode = {
                    role_id = u64(),                       %% 角色ID
                    page = u16()                           %% 页
                },
                encode = [
                    #private_chat{
                        sender_id = u64(),                 %% 发送者角色ID
                        receiver_id = u64(),               %% 接收者角色ID
                        type = u8(),                       %% 类型
                        message = bst()                    %% 消息内容
                    }
                ]
            }
        ]
    }.
