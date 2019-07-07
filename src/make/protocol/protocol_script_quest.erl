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
        name = 112,
        file = "src/module/quest/quest_protocol.erl",
        include = ["quest.hrl"],
        io = [
            #io{
                name = 11201,
                comment = "Quest List",
                read = [],
                write = [
                    #list{name = list, desc = #quest{                             %% Quest List
                        quest_id = #u32{},                                        %% |-- 任务ID
                        group_id = #u32{},                                        %% |-- 组ID
                        progress = #list{name = progress, desc = #quest_progress{ %% |-- 进度(convert)
                            id = #u16{},                                          %% |-- |-- ID
                            value = #u16{}}},                                     %% |-- |-- 计数
                        award = #u8{}                                             %% |-- 是否领取奖励
                    }}
                ]
            }
        ]
    }.
