%%%-------------------------------------------------------------------
%%! +pc unicode -pa beam
%%% @doc
%%% protocol read write define
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_achievement).
-mode(compile).
-compile({parse_transform, protocol_maker_transform}).
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
    ets:insert(ets:new(shell_records, [set, public]), [{Tag, Form} || Form = {attribute, _, record, {Tag, _}} <- lists:append([element(2, epp:parse_file(Header, [], [])) || Header <- filelib:wildcard(filename:dirname(escript:script_name()) ++ "/../../../include/*.hrl")])]),
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
        comment = "成就",
        erl = "script/make/protocol/erl/achievement_protocol.erl",
        html = "script/make/protocol/html/AchievementProtocol.html",
        lua = "script/make/protocol/lua/AchievementProtocol.lua",
        js = "script/make/protocol/js/AchievementProtocol.js",
        cs = "script/make/protocol/cs/AchievementProtocol.cs",
        io = [
            #io{
                number = 12201,
                comment = "统计列表",
                handler = #handler{module = achievement, function = query_count},
                decode = {},
                encode = [                                 %% 统计列表
                    #count{
                        type = u32(),                      %% 统计类型
                        total_number = u32()               %% 总数
                    }
                ]
            },
            #io{
                number = 12202,
                comment = "成就列表",
                handler = #handler{module = achievement, function = query},
                decode = {},
                encode = [                                 %% 成就列表
                    #achievement{
                        achievement_id = u32(),            %% 成就ID
                        type = u32()                       %% 成就类型
                    }
                ]
            },
            #io{
                number = 12203,
                comment = "提交成就",
                handler = #handler{module = achievement, function = award},
                decode = u32(),                            %% 成就ID
                encode = ast()                             %% 结果       
            }
        ]
    }.
