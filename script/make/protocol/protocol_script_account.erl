%%%-------------------------------------------------------------------
%%! +pc unicode -pa beam
%%% @doc
%%% protocol read write define
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_account).
-mode(compile).
-compile({parse_transform, protocol_maker_transform}).
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
                decode = {},
                encode = ast()                             %% 结果
            },
            #io{
                number = 10001,
                interval = ?SECOND_MILLISECONDS,
                comment = "查询账户",
                handler = #handler{module = account, function = query, state = client, response = send, imp = ""},
                decode = {
                    server_id = u16(),                     %% 服务器ID
                    account_name = bst()                   %% 账户名
                },
                encode = {
                    result = ast(),                        %% 结果
                    list = [                               %% 角色名列表
                        {
                            role_id = u64(),               %% 角色ID
                            role_name = bst()              %% 角色名
                        }
                    ]
                }
            },
            #io{
                number = 10002,
                interval = ?SECOND_MILLISECONDS,
                comment = "创建账户",
                handler = #handler{module = account, function = create, state = client, response = send, imp = ""},
                decode = {
                    role_name = bst(),                     %% 角色名
                    server_id = u16(),                     %% 服务器ID
                    account_name = bst(),                  %% 账户名
                    sex = u8(),                            %% 性别
                    classes = u8(),                        %% 职业
                    channel = bst(),                       %% 渠道
                    device_id = bst(),                     %% 设备
                    mac = bst(),                           %% mac地址
                    device_type = bst()                    %% 设备类型
                },
                encode = {
                    result = ast(),                        %% 结果
                    role_id = u64(),                       %% 角色ID
                    role_name = bst()                      %% 角色名
                }
            },
            #io{
                number = 10003,
                interval = ?SECOND_MILLISECONDS,
                comment = "登录",
                handler = #handler{module = account, function = login, state = client, response = send, imp = ""},
                decode = {
                    role_id = u64(),                       %% 角色ID
                    role_name = bst(),                     %% 角色名
                    server_id = u16(),                     %% 服务器ID
                    account_name = bst()                   %% 账户名
                },
                encode = ast()                             %% 结果
            },
            #io{
                number = 10004,
                interval = ?SECOND_MILLISECONDS,
                comment = "退出",
                handler = #handler{module = account, function = logout, state = client, response = send, imp = ""},
                decode = {},
                encode = ast()                             %% 结果
            },
            #io{
                number = 0,
                comment = "包控制",
                handler = #handler{module = account, function = handle_packet, state = client}
            }
        ]
    }.
