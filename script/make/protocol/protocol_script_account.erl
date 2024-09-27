%%%-------------------------------------------------------------------
%%! +pc unicode
%%% @doc
%%% protocol read write define
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_account).
-export([main/1]).
-include("../../../include/time.hrl").
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
        number = 100,
        comment = "账户",
        erl = "script/make/protocol/erl/account_protocol.erl",
        html = "script/make/protocol/html/AccountProtocol.html",
        lua = "script/make/protocol/lua/AccountProtocol.lua",
        js = "script/make/protocol/js/AccountProtocol.js",
        cs = "script/make/protocol/cs/AccountProtocol.cs",
        io = [
            #io{
                number = 10000,
                interval = ?SECOND_MILLISECONDS(30),
                comment = "心跳包",
                handler = #handler{module = account, function = heartbeat, state = client, response = send, imp = ""},
                decode = [],
                encode = [
                    #rst{name = result, comment = "结果"}
                ]
            },
            #io{
                number = 10001,
                interval = ?SECOND_MILLISECONDS,
                comment = "查询账户",
                handler = #handler{module = account, function = query, state = client, response = send, imp = ""},
                decode = [
                    #u16{name = server_id, comment = "服务器ID"},
                    #bst{name = account_name, comment = "账户名"}
                ],
                encode = [
                    #rst{name = result, comment = "结果"},
                    #list{name = list, comment = "角色名列表", explain = {
                        #u64{name = role_id, comment = "角色ID"},
                        #bst{name = role_name, comment = "角色名"}
                    }}
                ]
            },
            #io{
                number = 10002,
                interval = ?SECOND_MILLISECONDS,
                comment = "创建账户",
                handler = #handler{module = account, function = create, state = client, response = send, imp = ""},
                decode = [
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
                encode = [
                    #rst{name = result, comment = "结果"},
                    #u64{name = role_id, comment = "角色ID"},
                    #bst{name = role_name, comment = "角色名"}
                ]
            },
            #io{
                number = 10003,
                interval = ?SECOND_MILLISECONDS,
                comment = "登录",
                handler = #handler{module = account, function = login, state = client, response = send, imp = ""},
                decode = [
                    #u64{name = role_id, comment = "角色ID"},
                    #bst{name = role_name, comment = "角色名"},
                    #u16{name = server_id, comment = "服务器ID"},
                    #bst{name = account_name, comment = "账户名"}
                ],
                encode = [
                    #rst{name = result, comment = "结果"}
                ]
            },
            #io{
                number = 10004,
                interval = ?SECOND_MILLISECONDS,
                comment = "退出",
                handler = #handler{module = account, function = logout, state = client, response = send, imp = ""},
                decode = [],
                encode = [
                    #rst{name = result, comment = "结果"}
                ]
            },
            #io{
                number = 0,
                comment = "包控制",
                handler = #handler{module = account, function = handle_packet, state = client}
            }
        ]
    }.
