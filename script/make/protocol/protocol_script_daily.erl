%%%-------------------------------------------------------------------
%%! +pc unicode
%%% @doc
%%% protocol read write define
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_daily).
-export([main/1]).
-include("../../../include/journal.hrl").
-include("../../../include/serialize.hrl").
-include("../../../include/count.hrl").
-include("../../../include/daily.hrl").
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
        number = 123,
        handler = "src/module/daily/daily_handler.erl",
        erl = "src/module/daily/daily_protocol.erl",
        js = "script/make/protocol/js/DailyProtocol.js",
        lua = "script/make/protocol/lua/DailyProtocol.lua",
        includes = ["count.hrl", "daily.hrl"],
        io = [
            #io{
                protocol = 12301,
                comment = "统计列表",
                handler = #handler{module = daily, function = query_count},
                read = [],
                write = [
                    #list{name = list, comment = "统计列表", explain = #count{
                        type = #u32{comment = "统计类型"},
                        today_number = #u32{comment = "今日数量"}
                    }}
                ]
            },
            #io{
                protocol = 12302,
                comment = "日常列表",
                handler = #handler{module = daily, function = query},
                read = [],
                write = [
                    #list{name = list, comment = "日常列表", explain = #daily{
                        daily_id = #u32{comment = "日常ID"},
                        is_award = #u8{comment = "是否领取奖励"}
                    }},
                    #daily_active{
                        stage_id = #u32{comment = "奖励阶段ID"},
                        score = #u32{comment = "活跃度"}
                    }
                ]
            },
            #io{
                protocol = 12303,
                comment = "领取日常奖励",
                handler = #handler{module = daily, function = award},
                read = [
                    #u32{name = daily_id, comment = "日常ID"}
                ],
                write = [
                    #rst{name = result, comment = "结果"}
                ]
            },
            #io{
                protocol = 12304,
                comment = "领取活跃度阶段奖励",
                handler = #handler{module = daily, function = award_active},
                read = [
                    #u32{name = stage_id, comment = "阶段ID"}
                ],
                write = [
                    #rst{name = result, comment = "结果"}
                ]
            }
        ]
    }.
