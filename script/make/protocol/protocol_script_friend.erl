%%%-------------------------------------------------------------------
%%% @doc
%%% protocol read write define
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_friend).
-export([main/1]).
-include("../../../include/serialize.hrl").
-include("../../../include/friend.hrl").
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
        number = 115,
        handler = "src/module/friend/friend_handler.erl",
        erl = "src/module/friend/friend_protocol.erl",
        js = "script/make/protocol/js/FriendProtocol.js",
        lua = "script/make/protocol/lua/FriendProtocol.lua",
        includes = ["friend.hrl"],
        io = [
            #io{
                protocol = 11501,
                comment = "好友列表",
                handler = #handler{module = friend, function = query},
                read = [],
                write = [
                    #list{name = list, comment = "好友列表", explain = #friend{
                        friend_id = #u64{comment = "好友ID"},
                        friend_name = #bst{comment = "好友名字"},
                        relation = #u8{comment = "关系状态(申请:0/好友:1/黑名单:2)"},
                        time = #u32{comment = "添加/修改状态时间"}
                    }}
                ]
            },
            #io{
                protocol = 11502,
                comment = "申请",
                handler = #handler{module = friend, function = apply},
                read = [
                    #u64{name = friend_id, comment = "好友ID"}
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
                    #u64{name = friend_id, comment = "好友ID"}
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
                    #u64{name = friend_id, comment = "好友ID"}
                ],
                write = [
                    #rst{name = result, comment = "结果"},
                    #u64{name = friend_id, comment = "好友ID"}
                ]
            }
        ]
    }].
