%%%-------------------------------------------------------------------
%%! +pc unicode
%%% @doc
%%% protocol read write define
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_task).
-export([main/1]).
-include("../../../include/journal.hrl").
-include("../../../include/serialize.hrl").
-include("../../../include/task.hrl").
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
    #protocol{
        number = 112,
        handler = "src/module/task/task_handler.erl",
        erl = "src/module/task/task_protocol.erl",
        js = "script/make/protocol/js/TaskProtocol.js",
        lua = "script/make/protocol/lua/TaskProtocol.lua",
        includes = ["task.hrl"],
        io = [
            #io{
                protocol = 11201,
                comment = "任务列表",
                handler = #handler{module = task, function = query},
                read = [],
                write = [
                    #list{name = list, comment = "任务列表", explain = #task{
                        task_id = #u32{comment = "任务ID"},
                        is_award = #u8{comment = "是否领取奖励"},
                        number = #u16{comment = "当前数量"}
                    }}
                ]
            },
            #io{
                protocol = 11202,
                comment = "接收任务",
                handler = #handler{module = task, function = accept},
                read = [
                    #u32{name = task_id, comment = "任务ID"}
                ],
                write = [
                    #rst{name = result, comment = "结果"},
                    #task{
                        task_id = #u32{comment = "任务ID"},
                        is_award = #u8{comment = "是否领取奖励"},
                        number = #u16{comment = "当前数量"}
                    }
                ]
            },
            #io{
                protocol = 11203,
                comment = "提交任务",
                handler = #handler{module = task, function = submit},
                read = [
                    #u32{name = task_id, comment = "任务ID"}
                ],
                write = [
                    #rst{name = result, comment = "结果"}
                ]
            }
        ]
    }.
