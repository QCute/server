%%%-------------------------------------------------------------------
%%% @doc
%%% module protocol read write
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_friend).
-export([main/1]).
-include("../../../include/serialize.hrl").
-include("../../../include/friend.hrl").
%%%===================================================================
%%% API
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
        name = 115,
        handler = "src/module/friend/friend_handler.erl",
        erl = "src/module/friend/friend_protocol.erl",
        json = "script/make/protocol/json/FriendProtocol.js",
        lua = "script/make/protocol/lua/FriendProtocol.lua",
        includes = ["friend.hrl"],
        io = [
            #io{
                name = 11501,
                comment = "好友列表",
                handler = #handler{module = friend, function = query},
                read = [],
                write = [
                    #list{name = friend, comment = "好友列表", explain = #friend{
                        friend_id = #u64{comment = "好友ID"},
                        friend_name = #bst{comment = "好友名字"},
                        relation = #u8{comment = "关系状态(申请:0/好友:1/黑名单:2)"},
                        time = #u32{comment = "添加/修改状态时间"}
                    }}
                ]
            },
            #io{
                name = 11502,
                comment = "申请",
                handler = #handler{module = friend, function = apply},
                read = [
                    #u64{name = friend_id, comment = "好友ID"}
                ],
                write = [
                    #u8{name = result, comment = "结果"}
                ]
            },
            #io{
                name = 11503,
                comment = "同意",
                handler = #handler{module = friend, function = agree},
                read = [
                    #u64{name = friend_id, comment = "好友ID"}
                ],
                write = [
                    #u8{name = result, comment = "结果"}
                ]
            },
            #io{
                name = 11504,
                comment = "删除",
                handler = #handler{module = friend, function = delete},
                read = [
                    #u64{name = friend_id, comment = "好友ID"}
                ],
                write = [
                    #u8{name = result, comment = "结果"},
                    #u64{name = friend_id, comment = "好友ID"}
                ]
            }
        ]
    }.
