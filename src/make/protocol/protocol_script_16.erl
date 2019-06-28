%%%-------------------------------------------------------------------
%%% @doc
%%% module protocol read write
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_16).
-export([main/1]).
-include("../../../include/serialize.hrl").
%%%===================================================================
%%% API
%%%===================================================================
main([]) ->
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    Protocol = #protocol{file = File} = protocol(),
    console:stacktrace(catch maker:start(fun protocol_maker:parse/2, [{File, Protocol}])),
    ok;
main(_) ->
    io:format("invail argument~n").

%%%===================================================================
%%% protocol config
%%%===================================================================
protocol() ->
    #protocol{
        file = "src/protocol/protocol_16.erl",
        include = [],
        io = [
            #io{
                name = 16001,
                comment = "Chat World",
                read = [
                    #bst{name = msg}                                         %% 消息
                ],
                write = [
                    #u64{name = user_id},                                    %% 玩家ID
                    #bst{name = user_name},                                  %% 玩家名字
                    #bst{name = msg}                                         %% 消息
                ]
            },
            #io{
                name = 16002,
                comment = "Chat Guild",
                read = [
                    #bst{name = msg}                                         %% 消息
                ],
                write = [
                    #u64{name = user_id},                                    %% 玩家ID
                    #bst{name = user_name},                                  %% 玩家名字
                    #bst{name = msg}                                         %% 消息
                ]
            },
            #io{
                name = 16003,
                comment = "Chat Private",
                read = [
                    #u64{name = user_id},                                    %% 玩家ID
                    #bst{name = msg}                                         %% 消息
                ],
                write = [
                    #u64{name = user_id},                                    %% 玩家ID
                    #bst{name = user_name},                                  %% 玩家名字
                    #bst{name = msg}                                         %% 消息
                ]
            }
        ]
    }.
