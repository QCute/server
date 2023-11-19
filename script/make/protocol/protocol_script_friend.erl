%%%-------------------------------------------------------------------
%%! +pc unicode -pa beam
%%% @doc
%%% protocol read write define
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_friend).
-mode(compile).
-compile({parse_transform, protocol_maker_transform}).
-export([main/1]).
-include("../../../include/journal.hrl").
-include("../../../include/serialize.hrl").
-include("../../../include/friend.hrl").
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
        number = 115,
        comment = "好友",
        erl = "script/make/protocol/erl/friend_protocol.erl",
        html = "script/make/protocol/html/FriendProtocol.html",
        lua = "script/make/protocol/lua/FriendProtocol.lua",
        js = "script/make/protocol/js/FriendProtocol.js",
        cs = "script/make/protocol/cs/FriendProtocol.cs",
        io = [
            #io{
                number = 11501,
                comment = "好友列表",
                handler = #handler{module = friend, function = query},
                decode = {},
                encode = [
                    #friend{
                        friend_role_id = u64(),            %% 好友角色ID
                        friend_name = bst(),               %% 好友名字
                        relation = u8(),                   %% 关系状态(申请:1/好友:2/黑名单:3)
                        time = u32()                       %% 添加/修改状态时间
                    }
                ]
            },
            #io{
                number = 11502,
                comment = "申请",
                handler = #handler{module = friend, function = apply},
                decode = u64(),                            %% 好友角色ID
                encode = ast()                             %% 结果
            },
            #io{
                number = 11503,
                comment = "同意",
                handler = #handler{module = friend, function = agree},
                decode = u64(),                            %% 好友角色ID
                encode = ast()                             %% 结果
            },
            #io{
                number = 11504,
                comment = "删除",
                handler = #handler{module = friend, function = delete},
                decode = u64(),                            %% 好友角色ID
                encode = {
                    result = ast(),                        %% 结果
                    friend_role_id = u64()                 %% 好友角色ID
                }
            },
            #io{
                number = 11505,
                comment = "拉黑",
                handler = #handler{module = friend, function = block},
                decode = u64(),                            %% 好友角色ID
                encode = {
                    result = ast(),                        %% 结果
                    friend_role_id = u64()                 %% 好友角色ID
                }
            },
            #io{
                number = 11506,
                comment = "取消拉黑",
                handler = #handler{module = friend, function = cancel_block},
                decode = u64(),                            %% 好友角色ID
                encode = {
                    result = ast(),                        %% 结果
                    friend_role_id = u64()                 %% 好友角色ID
                }
            }
        ]
    }.
