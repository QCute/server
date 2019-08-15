%%%-------------------------------------------------------------------
%%% @doc
%%% module protocol read write
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_chat).
-export([main/1]).
-include("../../../include/serialize.hrl").
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
        name = 116,
        erl = "src/module/chat/chat_protocol.erl",
        includes = [],
        io = [
            #io{
                name = 11601,
                comment = "Chat World",
                read = [
                    #bst{name = msg}                                         %% 消息
                ],
                write = [
                    #u64{name = user_id},                                    %% 角色ID
                    #bst{name = user_name},                                  %% 角色名字
                    #bst{name = msg}                                         %% 消息
                ]
            },
            #io{
                name = 11602,
                comment = "Chat Guild",
                read = [
                    #bst{name = msg}                                         %% 消息
                ],
                write = [
                    #u64{name = user_id},                                    %% 角色ID
                    #bst{name = user_name},                                  %% 角色名字
                    #bst{name = msg}                                         %% 消息
                ]
            },
            #io{
                name = 11603,
                comment = "Chat Private",
                read = [
                    #u64{name = user_id},                                    %% 角色ID
                    #bst{name = msg}                                         %% 消息
                ],
                write = [
                    #u64{name = user_id},                                    %% 角色ID
                    #bst{name = user_name},                                  %% 角色名字
                    #bst{name = msg}                                         %% 消息
                ]
            }
        ]
    }.
