%%%-------------------------------------------------------------------
%%% @doc
%%% module protocol read write
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_account).
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
        number = 100,
        handler = "src/module/account/account_handler.erl",
        erl = "src/module/account/account_protocol.erl",
        lua = "script/make/protocol/lua/AccountProtocol.lua",
        js = "script/make/protocol/js/AccountProtocol.js",
        includes = [],
        io = [
            #io{
                protocol = 10000,
                comment = "心跳包",
                handler = #handler{arg = state, module = account, function = heartbeat},
                read = [],
                write = []
            },
            #io{
                comment = "登录",
                protocol = 10001,
                handler = #handler{arg = state, module = account, function = login},
                text = [{server_update, "服务器更新"}, {refuse, "禁止登录"}, {server_id_not_match, "服务器ID不匹配"}, {no_such_name, "没有此用户名"}, {permission_denied, "权限不够"}, {duplicate, "重复登录"}],
                read = [
                    #u16{name = server_id, comment = "服务器ID"},
                    #bst{name = account, comment = "账户"}
                ],
                write = [
                    #rst{name = result, comment = "结果"}
                ]
            },
            #io{
                comment = "退出",
                protocol = 10002,
                handler = #handler{arg = state, module = account, function = logout},
                text = [{server_id_not_match, "服务器ID不匹配"}, {no_such_name, "没有此用户名"}, {server_update, "服务器更新"}],
                read = [
                    #u16{name = server_id, comment = "服务器ID"},
                    #bst{name = account, comment = "账户"}
                ],
                write = [
                    #rst{name = result, comment = "结果"}
                ]
            },
            #io{
                protocol = 10003,
                comment = "创建账户",
                handler = #handler{arg = state, module = account, function = create},
                text = [{length, "长度不对"}, {not_utf8, "未知字符"}, {sensitive, "包含敏感词"}, {duplicate, "名字重复"}],
                read = [
                    #bst{name = account, comment = "账户"},
                    #bst{name = role_name, comment = "角色名"},
                    #u16{name = server_id, comment = "服务器ID"},
                    #u8{name = sex, comment = "性别"},
                    #u8{name = classes, comment = "职业"},
                    #u16{name = channel_id, comment = "渠道ID"},
                    #bst{name = device_id, comment = "设备"},
                    #bst{name = mac, comment = "mac地址"},
                    #bst{name = device_type, comment = "设备类型"}
                ],
                write = [
                    #rst{name = result, comment = "结果"}
                ]
            },
            #io{
                protocol = 0,
                comment = "包控制",
                handler = #handler{arg = state, module = account, function = handle_packet}
            }
        ]
    }].
