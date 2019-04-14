%%%-------------------------------------------------------------------
%%% @doc
%%% module protocol read write
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_30).
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
        file = "src/protocol/protocol_30.erl",
        include = ["rank.hrl"],
        io = [
            #io{
                name = 30001,
                comment = "Rank Fight Effect",
                read = [
                    #u32{name = rank_type}                      %% Rank Type
                ],
                write = [
                    #ets{
                        name = rank_list,
                        desc = {
                            #zero{},
                            #list{
                                name = list,
                                desc = #rank{
                                    type = #u32{},              %% 类型
                                    key = #u64{},               %% 键
                                    value = #u64{},             %% 值
                                    time = #u32{},              %% 时间
                                    rank = #u64{},              %% 排名
                                    name = #btr{}               %% 名字
                                }
                            }
                        }
                    }
                ]
            }
        ]
    }.
