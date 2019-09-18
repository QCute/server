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
    console:stacktrace(catch protocol_maker:start([protocol()]));
main(_) ->
    io:format("invalid argument~n").

%%%===================================================================
%%% protocol config
%%%===================================================================
protocol() ->
    #protocol{
        name = 112,
        handler = "src/module/quest/quest_handler.erl",
        erl = "src/module/quest/quest_protocol.erl",
        json = "script/make/protocol/json/QuestProtocol.js",
        lua = "script/make/protocol/lua/QuestProtocol.lua",
        includes = ["quest.hrl"],
        io = [
            #io{
                name = 11201,
                comment = "任务列表",
                handler = #handler{module = quest, function = query},
                read = [],
                write = [
                    #list{name = list, comment = "任务列表", explain = #quest{
                        quest_id = #u32{comment = "任务ID"},
                        award = #u8{comment = "是否领取奖励"},
                        amount = #u16{comment = "当前数量"}
                    }}
                ]
            },
            #io{
                name = 11202,
                comment = "接收任务",
                handler = #handler{module = quest, function = accept},
                read = [
                    #u8{name = quest_id, comment = "任务ID"}
                ],
                write = [
                    #u8{name = result, comment = "结果"},
                    #quest{
                        quest_id = #u32{comment = "任务ID"},
                        award = #u8{comment = "是否领取奖励"},
                        amount = #u16{comment = "当前数量"}
                    }
                ]
            },
            #io{
                name = 11203,
                comment = "提交任务",
                handler = #handler{module = quest, function = submit},
                read = [
                    #u8{name = quest_id, comment = "任务ID"}
                ],
                write = [
                    #u8{name = result, comment = "结果"}
                ]
            }
        ]
    }.
