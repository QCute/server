%%%-------------------------------------------------------------------
%%% @doc
%%% module protocol read write
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_key).
-export([main/1]).
-include("../../../include/serialize.hrl").
-include("../../../include/rank.hrl").
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
        name = 150,
        erl = "src/module/key/key_protocol.erl",
        includes = [],
        io = [
            #io{
                name = 15001,
                comment = "Key Award",
                read = [
                    #bst{name = key}                                           %% 兑换码
                ],
                write = [
                    #u8{name = result}                                         %% 领取结果
                ]
            }
        ]
    }.
