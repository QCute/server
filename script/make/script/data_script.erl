%%%------------------------------------------------------------------
%%% @doc
%%% module data script
%%% @end
%%%------------------------------------------------------------------
-module(data_script).
-export([main/1]).
%% ------------------------ user guide -------------------------------
%%
%% sql      :: auto group by key(when key reduplicated)
%% type     :: list |  maps  |  tuple  | record    | origin(default)
%% type     :: []   |  #{}   |  {}     | #record{} |
%% default  :: []   |  maps  |  tuple  | #record{} | (specified value) | default | [default] | {default}
%% includes :: ["*.hrl", "*.hrl"]
%%
%% string type term guide
%% varchar                                   => term
%% varchar with default(<<>>) in comment     => <<>>
%% char                                      => <<>>
%% text                                      => <<>>
%%
%%%==================================================================
%%% API functions
%%%==================================================================
main([Key]) ->
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    List = [X || X <- data(), filename:basename(element(1, X), ".erl") == Key orelse filename:basename(element(1, X), ".erl") == Key ++ "_data"],
    io:format("~p~n", [catch data_maker:start(List)]);
main(_) ->
    io:format("invalid argument~n").

%%%==================================================================
%%% base data
%%%==================================================================
data() ->
    [
        {"src/node/node_data.erl", [], %% 节点配置
            [
                {"SELECT `center_node` FROM `node_data` WHERE `server_node` = 'ServerNode'", "center_node"},
                {"SELECT `center_ip` FROM `node_data` WHERE `server_node` = 'ServerNode'", "center_ip"},
                {"SELECT `server_node` FROM `node_data` WHERE `server_type` = 'ServerType' GROUP BY `server_type`", "server_node"},
                {"SELECT `server_ip` FROM `node_data` WHERE `server_node` = 'ServerNode'", "server_ip"}
            ]
        },
        {"src/module/parameter/parameter_data.erl", [], %% 自定义参数配置
            [
                {"SELECT `value` FROM `parameter_data` WHERE `key` = 'Key'", "get"}
            ]
        },
        {"src/module/role/role_data.erl", ["role.hrl"], %% 角色配置
            [
                {"SELECT `level` FROM `level_data` WHERE 'Exp' < `exp` ORDER BY `exp` ASC DEFAULT 0", "level"}
            ]
        },
        {"src/module/asset/asset_data.erl", [], %% 资产配置
            [
                {"SELECT `item_id` FROM `asset_data` WHERE `asset` = 'Asset' ORDER BY `item_id` ASC DEFAULT 0", "get"}
            ]
        },
        {"src/module/vip/vip_data.erl", ["vip.hrl"], %% VIP配置
            [
                {"SELECT `vip` FROM `vip_data` WHERE 'Exp' < `exp` ORDER by `exp` ASC DEFAULT 0", "vip"},
                {"SELECT `vip` FROM `vip_data` WHERE 'Exp' >= `exp` ORDER by `exp` DESC DEFAULT 0", "more"}
            ]
        },
        {"src/module/item/item_data.erl", ["item.hrl"], %% 物品配置
            [
                {"SELECT #record{*} FROM `item_data` WHERE `item_id` = 'ItemId'", "get"}
            ]
        },
        {"src/module/quest/quest_data.erl", ["quest.hrl"], %% 任务配置
            [
                {"SELECT #record{*} FROM `quest_data` WHERE `quest_id` = 'QuestId'", "get"}
            ]
        },
        {"src/module/shop/shop_data.erl", ["shop.hrl"], %% 商店配置
            [
                {"SELECT #record{*} FROM `shop_data` WHERE `shop_id` = 'ShopId'", "get"}
            ]
        },
        {"src/module/key/key_data.erl", ["key.hrl"], %% 激活码配置
            [
                {"SELECT `type` FROM `key_data` WHERE `key` = 'Key' DEFAULT 0", "get"}
            ]
        },
        {"src/module/key/key_award_data.erl", ["key.hrl"], %% 激活码奖励配置
            [
                {"SELECT #record{*} FROM `key_award_data` WHERE `type` = 'Type'", "award"}
            ]
        },
        {"src/module/skill/skill_data.erl", ["skill.hrl"], %% 技能配置
            [
                {"SELECT #record{*} FROM `skill_data` WHERE `skill_id` = 'SkillId'", "get"}
            ]
        },
        {"src/module/buff/buff_data.erl", ["buff.hrl"], %% Buff配置
            [
                {"SELECT #record{*} FROM `buff_data` WHERE `buff_id` = 'BuffId'", "get"}
            ]
        },
        {"src/module/activity/activity_data.erl", ["activity.hrl"], %% 活动配置
            [
                {"SELECT #record{*} FROM `activity_data` WHERE `activity_id` = 'ActivityId'", "get"},
                {"SELECT `activity_id` FROM `activity_data`", "list"}
            ]
        },
        {"src/module/auction/auction_data.erl", ["auction.hrl"], %% 拍卖配置
            [
                {"SELECT #record{*} FROM `auction_data` WHERE `auction_id` = 'AuctionId'", "get"}
            ]
        },
        {"src/module/monster/monster_data.erl", ["monster.hrl"], %% 怪物配置
            [
                {"SELECT #record{*} FROM `monster_data` WHERE `monster_id` = 'MonsterId'", "get"},
                {"SELECT `monster_id` FROM `monster_data` WHERE `type` = 'Type' GROUP BY `type`", "type"},
                {"SELECT `monster_id` FROM `monster_data`", "all"}
            ]
        }
    ].
