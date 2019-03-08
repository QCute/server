%%%-------------------------------------------------------------------
%%% @doc
%%% module data script
%%% @end
%%%-------------------------------------------------------------------
-module(data_script).
-export([main/1]).
%% ------------------------ user guide -------------------------------
%%
%% sql      :: auto group by key(when key reduplicated)
%% type     :: list | maps | tuple |    record     | origin(default)
%% type     :: []   | #{}  |   {}  | ()/#record{}  |
%% default  :: [] | record | maps | tuple | list | (specified value)
%% includes :: ["*.hrl", "*.hrl"]
%%%===================================================================
%%% API
%%%===================================================================
main([Key]) ->
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    List = [X || X <- data(), string:str(element(1, X), Key) =/= 0],
    console:stacktrace(catch maker:start(fun data_maker:parse/2, List)),
    ok;
main(_) ->
    io:format("invail argument~n").

%%%===================================================================
%%% base data
%%%===================================================================
data() ->
    [
        {"src/data/data_parameter.erl", [], 
            [
                {"select `value` from `data_parameter` where `key` = 'Key'", "get", []}
            ]
        },
        {"src/data/data_node.erl", [], 
            [
                {"select `name` from `data_node` where `node` = 'Node'", "get", []}
            ]
        },
        {"src/data/data_item.erl", ["item.hrl"],
            [
                {"select #record{*} from `data_item`", "get", []},
                {"select #record{*} from `data_item` where `data_id` = 'DataId'", "get", []}
            ]
        },
        {"src/data/data_vip.erl", [],
            [
                {"SELECT `vip` FROM `data_vip` where 'Gold' < `gold` order by `gold` asc;", "get", 0},
                {"SELECT `vip` FROM `data_vip` group by `vip` order by `gold` asc;", "list", []}
            ]
        },
        {"src/data/data_player.erl", ["player.hrl"],
            [
                {"SELECT `level` FROM `data_level` where 'Exp' < `exp` order by `exp` asc;", "level", 0}
            ]
        },
        {"src/data/data_accost.erl", [],
            [
                {"SELECT {`num_id`, `type`, `obj_id`, `hour_start`, `hour_end`} FROM `data_accost` where `day_of_week` = 'DayOfWeek' AND `hour_start` = 'HourStart' AND `hour_end` = 'HourEnd'", "get", []}
            ]
        },
        {"src/data/data_key.erl", ["key.hrl"],
            [
                {"SELECT `type` FROM `data_key` where `key` = 'Key'", "get", 0},
                {"SELECT #record{*} FROM `data_key_award` where `type` = 'Type'", "award", []}
            ]
        },
        {"src/data/data_quest.erl", ["quest.hrl"],
            [
                {"SELECT #record{*} FROM `data_quest` where `quest_id` = 'QuestId'", "get", []}
            ]
        }
    ].