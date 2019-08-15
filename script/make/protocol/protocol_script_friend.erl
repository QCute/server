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
    Protocol = #protocol{erl = File} = protocol(),
    console:stacktrace(catch protocol_maker:start([{File, Protocol}]));
main(_) ->
    io:format("invail argument~n").

%%%===================================================================
%%% protocol config
%%%===================================================================
protocol() ->
    #protocol{
        name = 115,
        erl = "src/module/friend/friend_protocol.erl",
        includes = ["friend.hrl"],
        io = [
            #io{
                name = 11501,
                comment = "Friend",
                read = [],
                write = [
                    #list{name = friend, explain = #friend{    %% 好友列表
                        friend_id = #u64{},                    %% 好友ID
                        friend_name = #bst{},                  %% 好友名字
                        state = #u8{},                         %% 关系状态
                        time = #u32{}                          %% 添加/修改状态时间
                    }}
                ]
            }
        ]
    }.
