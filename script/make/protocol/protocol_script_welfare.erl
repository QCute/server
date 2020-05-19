%%%-------------------------------------------------------------------
%%% @doc
%%% module protocol read write
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_welfare).
-export([main/1]).
-include("../../../include/serialize.hrl").
-include("../../../include/lucky_money.hrl").
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
        number = 150,
        handler = "src/module/welfare/welfare_handler.erl",
        erl = "src/module/welfare/welfare_protocol.erl",
        js = "script/make/protocol/js/WelfareProtocol.js",
        lua = "script/make/protocol/lua/WelfareProtocol.lua",
        includes = ["lucky_money.hrl"],
        io = [
            #io{
                protocol = 15001,
                comment = "兑换码兑换",
                handler = #handler{module = key_server, function = award},
                text = [{key_already_active, "此兑换码已经兑换过了"}, {timeout, "请求超时"}],
                read = [
                    #bst{name = key, comment = "兑换码"}
                ],
                write = [
                    #rst{name = result, comment = "结果"}
                ]
            },
            #io{
                protocol = 15002,
                comment = "红包列表",
                handler = #handler{module = lucky_money_server, function = query, arg = []},
                read = [],
                write = [
                    #ets{name = list, comment = "红包列表", explain = #lucky_money{
                        lucky_money_id = #u64{comment = "红包Id"},
                        total_gold = #u64{comment = "总金币"},
                        total_number = #u32{comment = "总数量"},
                        receive_number = #u16{comment = "已经领取人数"},
                        receive_list = #list{comment = "领取列表", explain = #lucky_money_role{
                            server_id = #u16{comment = "服务器Id"},
                            role_id = #u64{comment = "角色Id"},
                            role_name = #bst{comment = "角色名"},
                            gold = #u64{comment = "金币"},
                            time = #u32{name = receive_time, comment = "领取时间"}
                        }},
                        time = #u32{name = send_time, comment = "发送时间"}
                    }}
                ]
            },
            #io{
                protocol = 15003,
                comment = "领取红包",
                handler = #handler{module = lucky_money_server, function = receive_lucky_money},
                text = [{no_such_lucky_money, "此兑换码已经兑换过了"}, {lucky_money_already_receive, "红包已领取过"}, {lucky_money_expire, "红包已过期"}, {timeout, "请求超时"}],
                read = [
                    #u64{name = lucky_money_id, comment = "红包Id"}
                ],
                write = [
                    #rst{name = result, comment = "结果"},
                    #u64{name = gold, comment = "金币"}
                ]
            }
        ]
    }].
