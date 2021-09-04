%%%-------------------------------------------------------------------
%%% @doc
%%% protocol read write define
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_quest).
-export([main/1]).
-include("../../../include/journal.hrl").
-include("../../../include/serialize.hrl").
-include("../../../include/quest.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
main(_) ->
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    try
        io:format("~p~n", [protocol_maker:start(protocol())])
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?ERROR_STACKTRACE(Class, Reason, Stacktrace)
    end.

%%%===================================================================
%%% protocol config
%%%===================================================================
protocol() ->
    [#protocol{
        number = 112,
        handler = "src/module/quest/quest_handler.erl",
        erl = "src/module/quest/quest_protocol.erl",
        js = "script/make/protocol/js/QuestProtocol.js",
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
                        is_award = #u8{comment = "是否领取奖励"},
                        number = #u16{comment = "当前数量"}
                    }}
                ]
            },
            #io{
                protocol = 11202,
                comment = "接收任务",
                handler = #handler{module = quest, function = accept},
                read = [
                    #u32{name = quest_id, comment = "任务ID"}
                ],
                write = [
                    #rst{name = result, comment = "结果"},
                    #quest{
                        quest_id = #u32{comment = "任务ID"},
                        is_award = #u8{comment = "是否领取奖励"},
                        number = #u16{comment = "当前数量"}
                    }
                ]
            },
            #io{
                protocol = 11203,
                comment = "提交任务",
                handler = #handler{module = quest, function = submit},
                read = [
                    #u32{name = quest_id, comment = "任务ID"}
                ],
                write = [
                    #rst{name = result, comment = "结果"}
                ]
            }
        ]
    }].
