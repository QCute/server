%%%-------------------------------------------------------------------
%%% @doc
%%% module protocol read write
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_account).
-export([main/1]).
-include("../../../include/serialize.hrl").
%%%===================================================================
%
%%%===================================================================
main([]) ->
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    console:stacktrace( protocol_maker:start([protocol()]));
main(_) ->
    io:format("invail argument~n").

%%%===================================================================
%%% protocol config
%%%===================================================================
protocol() ->
    #protocol{
        name = 100,
        handler = "src/module/account/account_handler.erl",
        erl = "src/module/account/account_protocol.erl",
        lua = "script/make/protocol/lua/AccountProtocol.lua",
        json = "script/make/protocol/json/AccountProtocol.js",
        includes = [],
        io = [
            #io{
                name = 10000,
                comment = "心跳包",
                handler = #handler{arg = state, module = account, function = heartbeat},
                read = [],
                write = []
            },
            #io{
                comment = "登录",
                name = 10001,
                handler = #handler{arg = state, module = account, function = login},
                read = [
                    #u16{name = server_id, comment = "服务器ID"},
                    #bst{name = account_name, comment = "账户名"}
                ],
                write = [
                    #u8{name = result, comment = "结果"}
                ]
            },
            #io{
                name = 10002,
                comment = "创建账户",
                handler = #handler{arg = state, module = account, function = create},
                read = [
                    #u16{name = server_id, comment = "服务器ID"},       
                    #u8{name = sex, comment = "性别"},              
                    #u8{name = career, comment = "职业"},           
                    #u16{name = channel_id, comment = "渠道ID"},
                    #bst{name = name, comment = "名字"},            
                    #bst{name = nick, comment = "昵称"},            
                    #bst{name = device_id, comment = "设备"},
                    #bst{name = mac, comment = "mac地址"},             
                    #bst{name = device_type, comment = "设备类型"}      
                ],
                write = [
                    #u8{name = result, comment = "结果"}
                ]
            },
            #io{
                name = 10003,
                comment = "查询账户",
                handler = #handler{arg = state, module = account, function = query},
                read = [
                    #bst{name = name, comment = "账户名"}
                ],
                write = [
                    #bst{name = name, comment = "角色名"}
                ]
            },
            #io{
                name = 0,
                comment = "包控制",
                handler = #handler{arg = state, module = account, function = handle_packet}
            }
        ]
    }.
