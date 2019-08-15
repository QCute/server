%%%-------------------------------------------------------------------
%%% @doc
%%% module protocol read write
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_notice).
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
        name = 500,
        erl = "src/module/notice/notice_protocol.erl",
        includes = [],
        io = [
            #io{
                name = 50001,
                comment = "Notice",
                read = [],
                write = [
                    #u8{name = scope},                      %% 范围
                    #u8{name = type},                       %% 类型
                    #str{name = msg}                        %% 消息
                ]
            }
        ]
    }.
