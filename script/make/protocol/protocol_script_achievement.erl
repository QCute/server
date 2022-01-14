%%%-------------------------------------------------------------------
%%! +pc unicode
%%% @doc
%%% protocol read write define
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_achievement).
-export([main/1]).
-include("../../../include/journal.hrl").
-include("../../../include/serialize.hrl").
-include("../../../include/count.hrl").
-include("../../../include/achievement.hrl").
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
        number = 122,
        handler = "src/module/achievement/achievement_handler.erl",
        erl = "src/module/achievement/achievement_protocol.erl",
        js = "script/make/protocol/js/AchievementProtocol.js",
        lua = "script/make/protocol/lua/AchievementProtocol.lua",
        includes = ["count.hrl", "achievement.hrl"],
        io = [
            #io{
                protocol = 12301,
                comment = "统计列表",
                handler = #handler{module = achievement, function = query_count},
                read = [],
                write = [
                    #list{name = list, comment = "统计列表", explain = #count{
                        type = #u32{comment = "统计类型"},
                        total_number = #u32{comment = "总数"}
                    }}
                ]
            },
            #io{
                protocol = 12202,
                comment = "成就列表",
                handler = #handler{module = achievement, function = query},
                read = [],
                write = [
                    #list{name = list, comment = "成就列表", explain = #achievement{
                        achievement_id = #u32{comment = "成就ID"},
                        type = #u32{comment = "成就类型"}
                    }}
                ]
            },
            #io{
                protocol = 12203,
                comment = "提交成就",
                handler = #handler{module = achievement, function = award},
                read = [
                    #u32{name = achievement_id, comment = "成就ID"}
                ],
                write = [
                    #rst{name = result, comment = "结果"}
                ]
            }
        ]
    }.
