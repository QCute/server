%%%-------------------------------------------------------------------
%%! +pc unicode -pa beam
%%% @doc
%%% protocol read write define
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_welfare).
-mode(compile).
-compile({parse_transform, protocol_maker_transform}).
-export([main/1]).
-include("../../../include/journal.hrl").
-include("../../../include/serialize.hrl").
-include("../../../include/lucky_money.hrl").
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
        number = 150,
        comment = "福利",
        erl = "script/make/protocol/erl/welfare_protocol.erl",
        html = "script/make/protocol/html/WelfareProtocol.html",
        lua = "script/make/protocol/lua/WelfareProtocol.lua",
        js = "script/make/protocol/js/WelfareProtocol.js",
        cs = "script/make/protocol/cs/WelfareProtocol.cs",
        io = [
            #io{
                number = 15001,
                comment = "签到",
                handler = #handler{module = sign, function = sign},
                decode = {},
                encode = ast()                             %% 结果
            },
            #io{
                number = 15002,
                comment = "兑换码兑换",
                handler = #handler{module = key_server, function = award},
                decode = bst(),                            %% 兑换码
                encode = ast()                             %% 结果
            },
            #io{
                number = 15003,
                comment = "红包",
                handler = #handler{module = lucky_money_server, function = query, alias = "query_lucky_money"},
                decode = u64(),                            %% 红包编号
                encode = #lucky_money{
                    lucky_money_no = u64(),                %% 红包编号
                    total_gold = u64(),                    %% 总金币
                    total_number = u32(),                  %% 总数量
                    receive_number = u16(),                %% 已经领取人数
                    receive_list = [                       %% 领取列表
                        #lucky_money_role{
                            server_id = u16(),             %% 服务器Id
                            role_id = u64(),               %% 角色Id
                            role_name = bst(),             %% 角色名
                            gold = u64(),                  %% 金币
                            time = u32()                   %% 领取时间
                        }
                    ],
                    time = u32()                           %% 发送时间
                }
            },
            #io{
                number = 15004,
                comment = "领取红包",
                handler = #handler{module = lucky_money_server, function = receive_lucky_money},
                decode = u64(),                            %% 红包编号
                encode = {
                    result = ast(),                        %% 结果
                    gold = u64()                           %% 金币
                }
            },
            #io{
                number = 15005,
                comment = "新到红包",
                handler = #handler{alias = "lucky_money_coming"},
                encode = {}
            }
        ]
    }.
