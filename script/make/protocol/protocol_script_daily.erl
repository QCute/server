%%%-------------------------------------------------------------------
%%! +pc unicode -pa beam
%%% @doc
%%% protocol read write define
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_daily).
-mode(compile).
-compile({parse_transform, protocol_maker_transform}).
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
        number = 123,
        comment = "日常",
        erl = "script/make/protocol/erl/daily_protocol.erl",
        html = "script/make/protocol/html/DailyProtocol.html",
        lua = "script/make/protocol/lua/DailyProtocol.lua",
        js = "script/make/protocol/js/DailyProtocol.js",
        cs = "script/make/protocol/cs/DailyProtocol.cs",
        io = [
            #io{
                number = 12301,
                comment = "日常活跃",
                handler = #handler{module = daily, function = query_active},
                decode = {},
                encode = #daily_active{
                    stage_id = u32(),                      %% 奖励阶段ID
                    score = u32()                          %% 活跃度
                }
            },
            #io{
                number = 12302,
                comment = "日常列表",
                handler = #handler{module = daily, function = query},
                decode = {},
                encode = [
                    #daily{
                        daily_id = u32(),                  %% 日常ID
                        is_award = u8()                    %% 是否领取奖励
                    }
                ]
            },
            #io{
                number = 12303,
                comment = "领取日常奖励",
                handler = #handler{module = daily, function = award},
                decode = u32(),                            %% 日常ID
                encode = ast()                             %% 结果
            },
            #io{
                number = 12304,
                comment = "领取活跃度阶段奖励",
                handler = #handler{module = daily, function = award_active},
                decode = u32(),                            %% 阶段ID
                encode = ast()                             %% 结果
            }
        ]
    }.
