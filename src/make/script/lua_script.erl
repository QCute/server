%%%-------------------------------------------------------------------
%%% @doc
%%% module data script
%%% @end
%%%-------------------------------------------------------------------
-module(lua_script).
-export([main/1]).
%%%===================================================================
%%% API
%%%===================================================================
main([Key]) ->
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    List = [X || X <- data(), string:str(element(1, X), Key) =/= 0],
    console:stacktrace(catch maker:start(fun lua_maker:parse/2, List)),
    ok;
main(_) ->
    io:format("invail argument~n").

%%%===================================================================
%%% base data
%%%===================================================================
data() ->
    [
        {"src/data/data_heir.lua",
            [
                {"SELECT `quality_name` FROM `data_heir_quality` where `quality` = 'Quality'", "quality"},
                {"SELECT `exp` FROM `data_heir_level` where `lv` = 'Lv'", "level"},
                {"SELECT {*} FROM `data_heir_diploma` where `keju` = 'KeJu'", "diploma"}
            ]
        },
        {"src/data/data_accost.lua",
            [
                {"SELECT {`num_id`, `type`, `obj_id`, `hour_start`, `hour_end`} FROM `data_accost` where `day_of_week` = 'DayOfWeek' AND `hour_start` = 'HourStart' AND `hour_end` = 'HourEnd'", ""}
            ]
        },
        {"src/data/data_limit.lua",
            [
                {"select {*} from `data_limit` where `activity_id` = 'ActivityId' and `limit_type` = 'LimitType' and `award_id` = 'AwardId'", ""}
            ]
        },
        {"src/data/data_activity_limit.lua",
            [
                {"select {*} from `data_activity_limit` where `activity_id` = 'ActivityId' and `limit_type` = 'LimitType' and `award_id` = 'AwardId'", ""}
            ]
        }
    ].