%%%-------------------------------------------------------------------
%%! +pc unicode
%%% @doc
%%% data script for data maker
%%% @end
%%%-------------------------------------------------------------------
%%! -s data
-module(data_script).
-export([main/1]).
-include("../../../include/journal.hrl").
%% ------------------------ user guide -------------------------------
%%
%% sql      :: auto group by key(when key reduplicated)
%% type     :: list |  maps  |  tuple  | record    | origin(default)
%% type     :: []   |  #{}   |  {}     | #record{} | window()
%% default  :: []   |  maps  |  tuple  | #record{} | (specified value) | default | [default] | {default}
%% includes :: ["*.hrl", "*.hrl"]
%%
%% string type term guide
%% varchar                                   => term
%% char                                      => <<>>
%%
%%%===================================================================
%%% API functions
%%%===================================================================
main([Key]) ->
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    Data = [X || X <- data(), filename:basename(element(1, X), ".erl") == Key orelse filename:basename(element(1, X), ".erl") == Key ++ "_data"],
    try
        io:format("~p~n", [data_maker:start(Data)])
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?ERROR_STACKTRACE(Class, Reason, Stacktrace)
    end;
main([]) ->
    io:format("[~n~ts~n]~n", [string:join([io_lib:format("{\"file\":\"~s\",\"description\":\"~ts\"}", [filename:basename(element(1, F)), binary_to_list(unicode:characters_to_binary(element(3, F)))]) || F <- data()], ",\n")]);
main(Args) ->
    io:format(standard_error, "invalid argument: ~p~n", [Args]).

%%%===================================================================
%%% data
%%%===================================================================
data() ->
    [
        {"src/module/text/test_data.erl", [], "测试配置",
            [
                {"SELECT `en` FROM `text_data` WHERE `key` = Key", "sc"},
                {"SELECT {*} FROM `text_data` WHERE `key` = Key", "text"},
                {"SELECT ALL `level` FROM `level_data` ORDER BY `level` ASC", "level"},
                {"SELECT `tc` FROM `error_code_data` WHERE `key` = Key AND `type` = Type ", "tc"},
                {"SELECT ALL `monster_id` FROM `monster_data` WHERE `type` = Type ", "type"},
                {"SELECT ALL `type` FROM `monster_data` GROUP BY `type` ", "type_list"},
                {"SELECT MAX(`level`) FROM `level_data` ", "max_level"},
                {"SELECT COUNT(`exp`) FROM `level_data` ", "level_count"},
                {"SELECT COUNT(`en`) FROM `text_data` ", "text_count"},
                {"SELECT `level` FROM `level_data` WHERE Exp >= `exp` ORDER BY `exp` DESC DEFAULT 0 ", "get_level_by_exp"}
            ]
        },
        {"src/module/text/text_data.erl", [], "文本配置",
            [
                {"SELECT `en` FROM `text_data` WHERE `key` = Key DEFAULT KEY", "en"},
                {"SELECT `tc` FROM `text_data` WHERE `key` = Key DEFAULT KEY", "tc"},
                {"SELECT `sc` FROM `text_data` WHERE `key` = Key DEFAULT KEY", "sc"}
            ]
        },
        {"src/module/text/error_code_data.erl", [], "错误码配置",
            [
                {"SELECT `en` FROM `error_code_data` WHERE `type` = Type AND `key` = Key DEFAULT _Key", "en"},
                {"SELECT `sc` FROM `error_code_data` WHERE `type` = Type AND `key` = Key DEFAULT _Key", "sc"},
                {"SELECT `tc` FROM `error_code_data` WHERE `type` = Type AND `key` = Key DEFAULT _Key", "tc"}
            ]
        },
        {"src/module/parameter/parameter_data.erl", [], "自定义参数配置",
            [
                {"SELECT `value` FROM `parameter_data` WHERE `key` = Key", "get"}
            ],
            [
                "get(Key, Default) ->\n    case ?MODULE:get(Key) of\n        [] ->\n            Default;\n        Value ->\n            Value\n    end.\n\n"
            ]
        },
        {"src/module/effect/effect_data.erl", ["effect.hrl"], "效果配置",
            [
                {"SELECT #record{`effect_id`, `scope`, `object`, `operation`, `attribute`, `field`} FROM `effect_data` WHERE `effect_id` = EffectId", "get"}
            ]
        },
        {"src/module/recharge/recharge_data.erl", ["recharge.hrl"], "充值配置",
            [
                {"SELECT #record{*} FROM `recharge_data` WHERE `recharge_id` = RechargeId", "get"}
            ]
        },
        {"src/module/role/role_data.erl", ["role.hrl"], "角色配置",
            [
                {"SELECT MIN(`level`) FROM `level_data`", "min_level"},
                {"SELECT MAX(`level`) FROM `level_data`", "max_level"},
                {"SELECT `level` FROM `level_data` WHERE Exp > `exp` ORDER BY `exp` DESC DEFAULT 0", "level"},
                {"SELECT `exp` FROM `level_data` WHERE Level = `level` ORDER BY `level` ASC DEFAULT 0", "exp"},
                {"SELECT `name` FROM `sex_data` WHERE `sex` = Sex ORDER BY `sex` ASC DEFAULT <<>>", "sex"},
                {"SELECT `name` FROM `classes_data` WHERE `classes` = Classes ORDER BY `classes` ASC DEFAULT <<>>", "classes"}
            ]
        },
        {"src/module/asset/asset_data.erl", [], "资产配置",
            [
                {"SELECT `item_id` FROM `asset_data` WHERE `asset` = Asset ORDER BY `item_id` ASC DEFAULT 0", "get"}
            ]
        },
        {"src/module/vip/vip_data.erl", ["vip.hrl"], "VIP配置",
            [
                {"SELECT `vip_level` FROM `vip_data` WHERE Exp >= `exp` ORDER by `exp` DESC DEFAULT 0", "level"}
            ]
        },
        {"src/module/item/item_data.erl", ["item.hrl"], "物品配置",
            [
                {"SELECT #record{*} FROM `item_data` WHERE `item_id` = ItemId", "get"}
            ]
        },
        {"src/module/quest/quest_data.erl", ["quest.hrl"], "任务配置",
            [
                {"SELECT #record{*} FROM `quest_data` WHERE `quest_id` = QuestId", "get"}
            ]
        },
        {"src/module/shop/shop_data.erl", ["shop.hrl"], "商店配置",
            [
                {"SELECT #record{*} FROM `shop_data` WHERE `shop_id` = ShopId", "get"}
            ]
        },
        {"src/module/skill/skill_data.erl", ["skill.hrl"], "技能配置",
            [
                {"SELECT #record{*} FROM `skill_data` WHERE `skill_id` = SkillId", "get"}
            ]
        },
        {"src/module/buff/buff_data.erl", ["buff.hrl"], "Buff配置",
            [
                {"SELECT #record{*} FROM `buff_data` WHERE `buff_id` = BuffId", "get"}
            ]
        },
        {"src/module/fashion/fashion_data.erl", ["fashion.hrl"], "时装配置",
            [
                {"SELECT #record{*} FROM `fashion_data` WHERE `fashion_id` = FashionId", "get"}
            ]
        },
        {"src/module/title/title_data.erl", ["title.hrl"], "称号配置",
            [
                {"SELECT #record{*} FROM `title_data` WHERE `title_id` = TitleId", "get"}
            ]
        },
        {"src/module/bubble/bubble_data.erl", ["bubble.hrl"], "气泡配置",
            [
                {"SELECT #record{*} FROM `bubble_data` WHERE `bubble_id` = BubbleId", "get"}
            ]
        },
        {"src/module/sign/sign_data.erl", ["sign.hrl"], "签到配置",
            [
                {"SELECT `award` FROM `sign_data` WHERE `day` = Day", "get"}
            ]
        },
        {"src/module/key/key_data.erl", ["key.hrl"], "激活码配置",
            [
                {"SELECT `type` FROM `key_data` WHERE `key` = Key DEFAULT 0", "get"}
            ]
        },
        {"src/module/key/key_award_data.erl", ["key.hrl"], "激活码奖励配置",
            [
                {"SELECT #record{*} FROM `key_award_data` WHERE `type` = Type", "award"}
            ]
        },
        {"src/module/activity/activity_data.erl", ["activity.hrl"], "活动配置",
            [
                {"SELECT #record{*} FROM `activity_data` WHERE `activity_id` = ActivityId", "get"},
                {"SELECT `activity_id` FROM `activity_data`", "list"}
            ]
        },
        {"src/module/auction/auction_data.erl", ["auction.hrl"], "拍卖配置",
            [
                {"SELECT #record{*} FROM `auction_data` WHERE `auction_id` = AuctionId", "get"}
            ]
        },
        {"src/module/dungeon/dungeon_data.erl", ["dungeon.hrl"], "副本配置",
            [
                {"SELECT #record{*} FROM `dungeon_data` WHERE `dungeon_id` = DungeonId", "get"}
            ]
        },
        {"src/module/map/map_data.erl", ["map.hrl"], "地图配置",
            [
                {"SELECT #record{*} FROM `map_data` WHERE `map_id` = MapId", "get"}
            ]
        },
        {"src/module/monster/monster_data.erl", ["monster.hrl"], "怪物配置",
            [
                {"SELECT #record{*} FROM `monster_data` WHERE `monster_id` = MonsterId", "get"},
                {"SELECT ALL `monster_id` FROM `monster_data` WHERE `type` = Type NAME type"},
                {"SELECT `monster_id` FROM `monster_data`", "all"}
            ]
        },
        {"src/module/guild/guild_data.erl", ["guild.hrl"], "公会配置",
            [
                {"SELECT {*} FROM `guild_create_data` WHERE `type` = Type", "create_type"},
                {"SELECT `level` FROM `guild_level_data` WHERE Exp > `exp` ORDER BY `exp` DESC DEFAULT 0", "level"}
            ]
        }
    ].
