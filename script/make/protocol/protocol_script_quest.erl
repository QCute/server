%%%-------------------------------------------------------------------
%%% @doc
%%% module protocol read write
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_quest).
-export([main/1]).
-include("../../../include/serialize.hrl").
-include("../../../include/quest.hrl").
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
        name = 112,
        erl = "src/module/quest/quest_protocol.erl",
        includes = ["quest.hrl"],
        io = [
            #io{
                name = 11201,
                comment = "Quest List",
                read = [],
                write = [
                    #list{name = list, explain = #quest{                             %% Quest List
                        quest_id = #u32{},                                           %% |-- 任务ID
                        group_id = #u32{},                                           %% |-- 组ID
                        award = #u8{},                                               %% |-- 是否领取奖励
                        progress = #list{name = progress, explain = #quest_progress{ %% |-- 进度(convert)
                            progress_id = #u16{},                                    %% |-- |-- ID
                            value = #u16{}                                           %% |-- |-- 计数
                        }}
                    }}
                ]
            }
        ]
    }.
