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
    List = [X || X <- data(), filename:basename(element(1, X), ".erl") == Key],
    console:stacktrace(catch maker:start(fun data_maker:parse/2, List)),
    ok;
main(_) ->
    io:format("invail argument~n").

%%%===================================================================
%%% base data
%%%===================================================================
data() ->
    [
        {"src/module/parameter/parameter_data.erl", [],
            [
                {"select `value` from `data_parameter` where `key` = 'Key'", "get", []}
            ]
        },
        {"src/module/text/text_data.erl", [],
            [
                {"select `value` from `data_text` where `key` = 'Key'", "get", []}
            ]
        },
        {"src/cluster/node_data.erl", [],
            [
                {"select `center_node` from `data_node` where `server_node` = 'ServerNode'", "get", []},
                {"select `center_ip` from `data_node` where `server_node` = 'ServerNode'", "ip", []},
                {"select `server_node` from `data_node`", "all", []}
            ]
        },
        {"src/module/item/item_data.erl", ["item.hrl"],
            [
                {"select #record{*} from `data_item`", "get", []},
                {"select #record{*} from `data_item` where `data_id` = 'DataId'", "get", []}
            ]
        },
        {"src/module/role/role_data.erl", ["role.hrl"],
            [
                {"SELECT `level` FROM `data_level` where 'Exp' < `exp` order by `exp` asc;", "level", 0},
                {"SELECT `vip` FROM `data_vip` where 'Gold' < `gold` order by `gold` asc;", "vip", 0}
            ]
        },
        {"src/module/accost/accost_data.erl", [],
            [
                {"SELECT {`num_id`, `type`, `obj_id`, `hour_start`, `hour_end`} FROM `data_accost` where `day_of_week` = 'DayOfWeek' AND `hour_start` = 'HourStart' AND `hour_end` = 'HourEnd'", "get", []}
            ]
        },
        {"src/module/key/key_data.erl", ["key.hrl"],
            [
                {"SELECT `type` FROM `data_key` where `key` = 'Key'", "get", 0},
                {"SELECT #record{*} FROM `data_key_award` where `type` = 'Type'", "award", []}
            ]
        },
        {"src/module/quest/quest_data.erl", ["quest.hrl"],
            [
                {"SELECT #record{*} FROM `data_quest` where `quest_id` = 'QuestId'", "get", []}
            ]
        },
        {"src/module/shop/shop_data.erl", ["shop.hrl"],
            [
                {"SELECT #record{*} FROM `data_shop` where `shop_id` = 'ShopId'", "get", []}
            ]
        }
    ].