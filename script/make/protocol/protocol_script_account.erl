%%%-------------------------------------------------------------------
%%% @doc
%%% protocol read write define
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
                handler = #handler{module = account, function = heartbeat, arg = state},
                read = [],
                write = []
            },
            #io{
                protocol = 10001,
                comment = "查询账户",
                handler = #handler{module = account, function = query, arg = state},
                read = [
                    #u16{name = server_id, comment = "服务器ID"},
                    #bst{name = account_name, comment = "账户名"}
                ],
                write = [
                    #list{name = list, comment = "角色名列表", explain = {
                        #u64{name = role_id, comment = "角色ID"},
                        #bst{name = role_name, comment = "角色名"}
                    }}
                ]
            },
            #io{
                protocol = 10002,
                comment = "创建账户",
                handler = #handler{module = account, function = create, arg = state},
                read = [
                    #bst{name = role_name, comment = "角色名"},
                    #u16{name = server_id, comment = "服务器ID"},
                    #bst{name = account_name, comment = "账户名"},
                    #u8{name = sex, comment = "性别"},
                    #u8{name = classes, comment = "职业"},
                    #bst{name = channel, comment = "渠道"},
                    #bst{name = device_id, comment = "设备"},
                    #bst{name = mac, comment = "mac地址"},
                    #bst{name = device_type, comment = "设备类型"}
                ],
                write = [
                    #rst{name = result, comment = "结果"},
                    #u64{name = role_id, comment = "角色ID"},
                    #bst{name = role_name, comment = "角色名"}
                ]
            },
            #io{
                protocol = 10003,
                comment = "登录",
                handler = #handler{module = account, function = login, arg = state},
                read = [
                    #u64{name = role_id, comment = "角色ID"},
                    #bst{name = role_name, comment = "角色名"},
                    #u16{name = server_id, comment = "服务器ID"},
                    #bst{name = account_name, comment = "账户名"}
                ],
                write = [
                    #rst{name = result, comment = "结果"}
                ]
            },
            #io{
                protocol = 10004,
                comment = "退出",
                handler = #handler{module = account, function = logout, arg = state},
                read = [],
                write = [
                    #rst{name = result, comment = "结果"}
                ]
            },
            #io{
                protocol = 0,
                comment = "包控制",
                handler = #handler{module = account, function = handle_packet, arg = state}
            }
        ]
    }].
