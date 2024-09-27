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
    io:setopts([{encoding, unicode}]),
    io:setopts(standard_error, [{encoding, unicode}]),
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    try
        io:format("~tp~n", [protocol_maker:start(protocol())])
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?HALT(Class, Reason, Stacktrace)
    end.

%%%===================================================================
%%% protocol config
%%%===================================================================
protocol() ->
    #protocol{
        number = 112,
        comment = "任务",
        erl = "script/make/protocol/erl/task_protocol.erl",
        html = "script/make/protocol/html/TaskProtocol.html",
        lua = "script/make/protocol/lua/TaskProtocol.lua",
        js = "script/make/protocol/js/TaskProtocol.js",
        cs = "script/make/protocol/cs/TaskProtocol.cs",
        io = [
            #io{
                number = 11201,
                comment = "任务列表",
                handler = #handler{module = task, function = query},
                decode = [],
                encode = [
                    #list{name = list, comment = "任务列表", explain = #task{
                        task_id = #u32{comment = "任务ID"},
                        is_award = #u8{comment = "是否领取奖励"},
                        number = #u16{comment = "当前数量"}
                    }}
                ]
            },
            #io{
                number = 11202,
                comment = "接收任务",
                handler = #handler{module = task, function = accept},
                decode = [
                    #u32{name = task_id, comment = "任务ID"}
                ],
                encode = [
                    #rst{name = result, comment = "结果"},
                    #task{
                        task_id = #u32{comment = "任务ID"},
                        is_award = #u8{comment = "是否领取奖励"},
                        number = #u16{comment = "当前数量"}
                    }
                ]
            },
            #io{
                number = 11203,
                comment = "提交任务",
                handler = #handler{module = task, function = submit},
                decode = [
                    #u32{name = task_id, comment = "任务ID"}
                ],
                encode = [
                    #rst{name = result, comment = "结果"}
                ]
            }
        ]
    }.
