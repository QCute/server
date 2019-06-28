%%%-------------------------------------------------------------------
%%% @doc
%%% module protocol read write
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_19).
-export([main/1]).
-include("../../../include/serialize.hrl").
-include("../../../include/rank.hrl").
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
        file = "src/protocol/protocol_19.erl",
        include = [],
        io = [
            #io{
                name = 19001,
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
