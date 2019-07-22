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
%% default  :: []   | maps | tuple |    record     | (specified value)
%% includes :: ["*.hrl", "*.hrl"]
%%
%% string type term guide
%% varchar                                   => term
%% varchar with default(<<>>) in comment     => <<>>
%% char                                      => <<>>
%% text                                      => <<>>
%%
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
        {"src/module/text/error_code_data.erl", [],
            [
                {"SELECT `text` FROM `error_code_data` WHERE `protocol` = 'Protocol' AND `code` = 'Code'", "get", []}
            ]
        },
        {"src/module/parameter/parameter_data.erl", [],
            [
                {"select `value` from `parameter_data` where `key` = 'Key'", "get", []}
            ]
        },
        {"src/module/text/text_data.erl", [],
            [
                {"select `value` from `text_data` where `key` = 'Key'", "get", []}
            ]
        },
        {"src/cluster/node_data.erl", [],
            [
                {"select `center_node` from `node_data` where `server_node` = 'ServerNode'", "get", []},
                {"select `center_ip` from `node_data` where `server_node` = 'ServerNode'", "ip", []},
                {"select `server_node` from `node_data`", "all", []}
            ]
        },
        {"src/module/item/item_data.erl", ["item.hrl"],
            [
                {"select #record{*} from `item_data`", "get", []},
                {"select #record{*} from `item_data` where `item_id` = 'ItemId'", "get", []}
            ]
        },
        {"src/module/role/role_data.erl", ["role.hrl"],
            [
                {"SELECT `level` FROM `level_data` where 'Exp' < `exp` order by `exp` asc;", "level", 0}
            ]
        },
        {"src/module/vip/vip_data.erl", ["vip.hrl"],
            [
                {"SELECT `vip` FROM `vip_data` where 'Exp' < `exp` order by `exp` asc;", "vip", 0}
            ]
        },
        {"src/module/accost/accost_data.erl", [],
            [
                {"SELECT {`num_id`, `type`, `obj_id`, `hour_start`, `hour_end`} FROM `accost_data` where `day_of_week` = 'DayOfWeek' AND `hour_start` = 'HourStart' AND `hour_end` = 'HourEnd'", "get", []}
            ]
        },
        {"src/module/key/key_data.erl", ["key.hrl"],
            [
                {"SELECT `type` FROM `key_data` where `key` = 'Key'", "get", 0}
            ]
        },
        {"src/module/key/key_award_data.erl", ["key.hrl"],
            [
                {"SELECT #record{*} FROM `key_award_data` where `type` = 'Type'", "award", []}
            ]
        },
        {"src/module/quest/quest_data.erl", ["quest.hrl"],
            [
                {"SELECT #record{*} FROM `quest_data` where `quest_id` = 'QuestId'", "get", []}
            ]
        },
        {"src/module/quest/quest_progress_data.erl", ["quest.hrl"],
            [
                {"SELECT #record{*} FROM `quest_progress_data` where `progress_id` = 'ProgressId'", "get", []}
            ]
        },
        {"src/module/shop/shop_data.erl", ["shop.hrl"],
            [
                {"SELECT #record{*} FROM `shop_data` where `shop_id` = 'ShopId'", "get", []}
            ]
        },
        {"src/module/skill/skill_data.erl", ["skill.hrl"],
            [
                {"SELECT #record{*} FROM `skill_data` where `skill_id` = 'SkillId'", "get", []}
            ]
        },
        {"src/module/buff/buff_data.erl", ["buff.hrl"],
            [
                {"SELECT #record{*} FROM `buff_data` where `buff_id` = 'BuffId'", "get", []}
            ]
        }
    ].