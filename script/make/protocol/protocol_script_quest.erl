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
%%% API functions
%%%===================================================================
main([]) ->
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    io:format("~p~n", [catch protocol_maker:start(protocol())]);
main(_) ->
    io:format("invalid argument~n").

%%%===================================================================
%%% protocol config
%%%===================================================================
protocol() ->
    [#protocol{
        number = 112,
        handler = "src/module/quest/quest_handler.erl",
        erl = "src/module/quest/quest_protocol.erl",
        json = "script/make/protocol/json/QuestProtocol.js",
        lua = "script/make/protocol/lua/QuestProtocol.lua",
        includes = ["quest.hrl"],
        io = [
            #io{
                protocol = 11201,
                comment = "任务列表",
                handler = #handler{module = quest, function = query},
                read = [],
                write = [
                    #list{name = list, comment = "任务列表", explain = #quest{
                        quest_id = #u32{comment = "任务ID"},
                        award = #u8{comment = "是否领取奖励"},
                        number = #u16{comment = "当前数量"}
                    }}
                ]
            },
            #io{
                protocol = 11202,
                comment = "接收任务",
                handler = #handler{module = quest, function = accept},
                text = [{configure_not_found, "配置错误"}, {pre_quest_not_complete, "前置任务还没完成"}, {not_next_quest, "请按顺序完成"}, {no_such_quest, "没有此任务"}, {condition_not_met, "条件不满足"}],
                read = [
                    #u32{name = quest_id, comment = "任务ID"}
                ],
                write = [
                    #rst{name = result, comment = "结果"},
                    #quest{
                        quest_id = #u32{comment = "任务ID"},
                        award = #u8{comment = "是否领取奖励"},
                        number = #u16{comment = "当前数量"}
                    }
                ]
            },
            #io{
                protocol = 11203,
                comment = "提交任务",
                handler = #handler{module = quest, function = submit},
                text = [{quest_already_submit, "任务已提交"}, {quest_not_complete, "任务还没完成"}, {no_such_quest, "没有此任务"}, {configure_not_found, "配置错误"}],
                read = [
                    #u32{name = quest_id, comment = "任务ID"}
                ],
                write = [
                    #rst{name = result, comment = "结果"}
                ]
            }
        ]
    }].
