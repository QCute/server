%%%-------------------------------------------------------------------
%%% @doc
%%% module protocol read write
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_rank).
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
        name = 190,
        file = "src/module/rank/rank_protocol.erl",
        include = ["rank.hrl"],
        io = [
            #io{
                name = 19001,
                comment = "Rank",
                read = [],
                write = [
                    #list{name = list, desc = #rank{                           %% 排行榜
                        type = #u16{},                                         %% |-- 类型
                        key = #u64{},                                          %% |-- 键
                        value = #u64{},                                        %% |-- 值
                        time = #u32{},                                         %% |-- 时间
                        rank = #u64{},                                         %% |-- 排名
                        name = #bst{}                                          %% |-- 名字(string)
                    }}
                ]
            }
        ]
    }.