%%%-------------------------------------------------------------------
%%! +pc unicode
%%% @doc
%%% protocol read write define
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_friend).
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
        handler = "src/module/friend/friend_handler.erl",
        erl = "src/module/friend/friend_protocol.erl",
        html = "script/make/protocol/html/FriendProtocol.html",
        lua = "script/make/protocol/lua/FriendProtocol.lua",
        js = "script/make/protocol/js/FriendProtocol.js",
        cs = "script/make/protocol/cs/FriendProtocol.cs",
        includes = ["friend.hrl"],
        io = [
            #io{
                protocol = 11501,
                comment = "好友列表",
                handler = #handler{module = friend, function = query},
                read = [],
                write = [
                    #list{name = list, comment = "好友列表", explain = #friend{
                        friend_role_id = #u64{comment = "好友角色ID"},
                        friend_name = #bst{comment = "好友名字"},
                        relation = #u8{comment = "关系状态(申请:1/好友:2/黑名单:3)"},
                        time = #u32{comment = "添加/修改状态时间"}
                    }}
                ]
            },
            #io{
                protocol = 11502,
                comment = "申请",
                handler = #handler{module = friend, function = apply},
                read = [
                    #u64{name = friend_role_id, comment = "好友角色ID"}
                ],
                write = [
                    #rst{name = result, comment = "结果"}
                ]
            },
            #io{
                protocol = 11503,
                comment = "同意",
                handler = #handler{module = friend, function = agree},
                read = [
                    #u64{name = friend_role_id, comment = "好友角色ID"}
                ],
                write = [
                    #rst{name = result, comment = "结果"}
                ]
            },
            #io{
                protocol = 11504,
                comment = "删除",
                handler = #handler{module = friend, function = delete},
                read = [
                    #u64{name = friend_role_id, comment = "好友角色ID"}
                ],
                write = [
                    #rst{name = result, comment = "结果"},
                    #u64{name = friend_role_id, comment = "好友角色ID"}
                ]
            },
            #io{
                protocol = 11505,
                comment = "拉黑",
                handler = #handler{module = friend, function = block},
                read = [
                    #u64{name = friend_role_id, comment = "好友角色ID"}
                ],
                write = [
                    #rst{name = result, comment = "结果"},
                    #u64{name = friend_role_id, comment = "好友角色ID"}
                ]
            },
            #io{
                protocol = 11506,
                comment = "取消拉黑",
                handler = #handler{module = friend, function = cancel_block},
                read = [
                    #u64{name = friend_role_id, comment = "好友角色ID"}
                ],
                write = [
                    #rst{name = result, comment = "结果"},
                    #u64{name = friend_role_id, comment = "好友角色ID"}
                ]
            }
        ]
    }.
