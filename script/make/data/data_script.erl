%%%-------------------------------------------------------------------
%%! +pc unicode
%%% @doc
%%% data script for data maker
%%% @end
%%%-------------------------------------------------------------------
-module(data_script).
-export([main/1]).
-include("../../../include/journal.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
main([]) ->
    io:setopts([{encoding, unicode}]),
    io:setopts(standard_error, [{encoding, unicode}]),
    List = [io_lib:format("{\"file\":\"~s\",\"comment\":\"~ts\"}", [File, Comment]) || #{file := File, comment := Comment} <- data()],
    io:format("[~n~ts~n]~n", [string:join(List, ",\n")]);
main(Keys) ->
    io:setopts([{encoding, unicode}]),
    io:setopts(standard_error, [{encoding, unicode}]),
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    Data = [X || X <- data(), lists:member(filename:basename(maps:get(file, X), ".erl"), Keys)],
    try
        io:format("~tp~n", [data_maker:start(Data)])
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?HALT(Class, Reason, Stacktrace)
    end.

%%%===================================================================
%%% data
%%%===================================================================
data() ->
    [
        #{
            file => "src/module/test/test_data.erl",
            comment => "测试配置",
            include => [],
            meta => [
                %% key -> value
                #{name => "zhCN", sql => "SELECT `zhCN` FROM `text_data` WHERE `key` = Key"},
                %% key -> column value
                #{name => "text", sql => "SELECT {*} FROM `text_data` WHERE `key` = Key"},
                %% key -> [value]
                #{name => "type", sql => "SELECT ALL `monster_id` FROM `monster_data` WHERE `type` = Type"},
                %% -> [value] (not unique)
                #{name => "level", sql => "SELECT ALL `level` FROM `level_data` ORDER BY `level` ASC"},
                %% -> [value] (unique)
                #{name => "type_list", sql => "SELECT ALL `type` FROM `monster_data` GROUP BY `type`"},
                %% -> value
                #{name => "min_max_level", sql => "SELECT {MIN(`level`), MAX(`level`)} FROM `level_data`"},
                %% -> value
                #{name => "text_count", sql => "SELECT COUNT(`zhCN`) FROM `text_data`"},
                %% -> value
                #{name => "max_text", sql => "SELECT {MAX(`key`), MAX(`zhCN`)} FROM `text_data`"},
                %% key, key, ... -> value
                #{name => "ref", sql => "SELECT `description` FROM `reference_data` WHERE `key` = Key AND `value` = Value"},
                %% key, key, ... -> value in if else range
                #{name => "ref_range", sql => "SELECT `description` FROM `reference_data` WHERE `key` = Key AND `value` < Value"},
                %% key -> value in if else range ...
                #{name => "get_level_by_exp_asc", sql => "SELECT `level` FROM `level_data` WHERE Exp < `exp` ORDER BY `exp` ASC"},
                % filter data
                #{name => "get", sql => "SELECT `value` FROM `parameter_data` WHERE `key` = Key HAVING `key` LIKE '%size' "}
            ]
        },
        #{
            file => "src/module/text/text_data.erl",
            include => [],
            comment => "文本配置",
            sql => [
                #{name => "zhCN", sql => "SELECT `zhCN` FROM `text_data` WHERE `key` = Key DEFAULT KEY"}
            ],
            extra => [
                %% text with default lang
                "-spec text(Key :: atom()) -> Text :: binary() | Key :: atom().\n"
                "text(Key) ->\n"
                "    text(Key, parameter_data:get(language)).\n\n",

                %% text with spec lang
                "-spec text(Key :: atom(), Lang :: atom()) -> Text :: binary() | Key :: atom().\n"
                "text(Key, zhCN) ->\n"
                "    zhCN(Key).\n\n"
            ]
        },
        #{
            file => "src/module/parameter/parameter_data.erl",
            include => [],
            comment => "自定义参数配置",
            sql => [
                #{name => "get", sql => "SELECT `value` FROM `parameter_data` WHERE `key` = Key"}
            ],
            extra => [
                %% get with default
                "-spec get(Key :: atom(), Default :: term()) -> term().\n"
                "get(Key, Default) ->\n"
                "    case parameter_data:get(Key) of\n"
                "        [] ->\n"
                "            Default;\n"
                "        Value ->\n"
                "            Value\n"
                "    end.\n\n"
            ]
        },
        #{
            file => "src/module/effect/effect_data.erl",
            comment => "效果配置",
            include => ["effect.hrl"],
            meta => [
                #{name => "get", sql => "SELECT #record{`effect_id`, `scope`, `object`, `operation`, `attribute`, `field`} FROM `effect_data` WHERE `effect_id` = EffectId"}
            ]
        },
        #{
            file => "src/module/charge/charge_data.erl",
            comment => "充值配置",
            include => ["charge.hrl"],
            meta => [
                #{name => "get", sql => "SELECT #record{*} FROM `charge_data` WHERE `charge_id` = ChargeId"}
            ]
        },
        #{
            file => "src/module/role/role_data.erl",
            comment => "角色配置",
            include => ["role.hrl"],
            meta => [
                #{name => "min_level", sql => "SELECT MIN(`level`) FROM `level_data`"},
                #{name => "max_level", sql => "SELECT MAX(`level`) FROM `level_data`"},
                #{name => "level", sql => "SELECT `level` FROM `level_data` WHERE Exp > `exp` ORDER BY `exp` DESC DEFAULT 0"},
                #{name => "exp", sql => "SELECT `exp` FROM `level_data` WHERE Level = `level` ORDER BY `level` ASC DEFAULT 0"},
                #{name => "sex", sql => "SELECT `name` FROM `sex_data` WHERE `sex` = Sex ORDER BY `sex` ASC DEFAULT <<>>"},
                #{name => "classes", sql => "SELECT `name` FROM `classes_data` WHERE `classes` = Classes ORDER BY `classes` ASC DEFAULT <<>>"}
            ]
        },
        #{
            file => "src/module/asset/asset_data.erl",
            comment => "资产配置",
            include => [],
            meta => [
                #{name => "get", sql => "SELECT `item_id` FROM `asset_data` WHERE `asset` = Asset ORDER BY `item_id` ASC DEFAULT 0"}
            ]
        },
        #{
            file => "src/module/vip/vip_data.erl",
            comment => "VIP配置",
            include => ["vip.hrl"],
            meta => [
                #{name => "level", sql => "SELECT `vip_level` FROM `vip_data` WHERE Exp >= `exp` ORDER by `exp` DESC DEFAULT 0"}
            ]
        },
        #{
            file => "src/module/item/item_data.erl",
            comment => "物品配置",
            include => ["item.hrl"],
            meta => [
                #{name => "get", sql => "SELECT #record{*} FROM `item_data` WHERE `item_id` = ItemId"}
            ]
        },
        #{
            file => "src/module/task/task_data.erl",
            comment => "任务配置",
            include => ["task.hrl"],
            meta => [
                #{name => "get", sql => "SELECT #record{*} FROM `task_data` WHERE `task_id` = TaskId"}
            ]
        },
        #{
            file => "src/module/achievement/achievement_data.erl",
            comment => "成就配置",
            include => ["achievement.hrl"],
            meta => [
                #{name => "get", sql => "SELECT #record{*} FROM `achievement_data` WHERE `achievement_id` = AchievementId"},
                #{name => "first", sql => "SELECT MIN(`achievement_id`) FROM `achievement_data` WHERE `type` = Type GROUP BY `type`"},
                #{name => "last", sql => "SELECT MAX(`achievement_id`) FROM `achievement_data` WHERE `type` = Type GROUP BY `type`"},
                #{name => "type", sql => "SELECT ALL `achievement_id` FROM `achievement_data` WHERE `type` = Type"}
            ]
        },
        #{
            file => "src/module/shop/shop_data.erl",
            comment => "商店配置",
            include => ["shop.hrl"],
            meta => [
                #{name => "get", sql => "SELECT #record{*} FROM `shop_data` WHERE `shop_id` = ShopId"}
            ]
        },
        #{
            file => "src/module/skill/skill_data.erl",
            comment => "技能配置",
            include => ["skill.hrl"],
            meta => [
                #{name => "get", sql => "SELECT #record{*} FROM `skill_data` WHERE `skill_id` = SkillId"}
            ]
        },
        #{
            file => "src/module/buff/buff_data.erl",
            comment => "Buff配置",
            include => ["buff.hrl"],
            meta => [
                #{name => "get", sql => "SELECT #record{*} FROM `buff_data` WHERE `buff_id` = BuffId"}
            ]
        },
        #{
            file => "src/module/fashion/fashion_data.erl",
            comment => "时装配置",
            include => ["fashion.hrl"],
            meta => [
                #{name => "get", sql => "SELECT #record{*} FROM `fashion_data` WHERE `fashion_id` = FashionId"}
            ]
        },
        #{
            file => "src/module/title/title_data.erl",
            comment => "称号配置",
            include => ["title.hrl"],
            meta => [
                #{name => "get", sql => "SELECT #record{*} FROM `title_data` WHERE `title_id` = TitleId"}
            ]
        },
        #{
            file => "src/module/bubble/bubble_data.erl",
            comment => "气泡配置",
            include => ["bubble.hrl"],
            meta => [
                #{name => "get", sql => "SELECT #record{*} FROM `bubble_data` WHERE `bubble_id` = BubbleId"}
            ]
        },
        #{
            comment => "签到配置",
            file => "src/module/sign/sign_data.erl",
            include => ["sign.hrl"],
            meta => [
                #{name => "get", sql => "SELECT `award` FROM `sign_data` WHERE `day` = Day"}
            ]
        },
        #{
            file => "src/module/daily/daily_data.erl",
            comment => "日常配置",
            include => ["daily.hrl"],
            meta => [
                #{name => "get_daily", sql => "SELECT #record{*} FROM `daily_data` WHERE `daily_id` = DailyId"},
                #{name => "get_daily_active", sql => "SELECT #record{*} FROM `daily_active_data` WHERE `stage_id` = StageId"}
            ]
        },
        #{
            file => "src/module/key/key_data.erl",
            comment => "激活码配置",
            include => ["key.hrl"],
            meta => [
                #{name => "get", sql => "SELECT `type` FROM `key_data` WHERE `key` = Key DEFAULT 0"}
            ]
        },
        #{
            file => "src/module/key/key_award_data.erl",
            comment => "激活码奖励配置",
            include => ["key.hrl"],
            meta => [
                #{name => "award", sql => "SELECT #record{*} FROM `key_award_data` WHERE `type` = Type"}
            ]
        },
        #{
            file => "src/module/activity/activity_data.erl",
            comment => "活动配置",
            include => ["activity.hrl"],
            meta => [
                #{name => "get", sql => "SELECT #record{*} FROM `activity_data` WHERE `activity_id` = ActivityId"},
                #{name => "list", sql => "SELECT ALL `activity_id` FROM `activity_data`"}
            ]
        },
        #{
            file => "src/module/auction/auction_data.erl",
            comment => "拍卖配置",
            include => ["auction.hrl"],
            meta => [
                #{name => "get", sql => "SELECT #record{*} FROM `auction_data` WHERE `auction_id` = AuctionId"}
            ]
        },
        #{
            file => "src/module/dungeon/dungeon_data.erl",
            comment => "副本配置",
            include => ["dungeon.hrl"],
            meta => [
                #{name => "get", sql => "SELECT #record{*} FROM `dungeon_data` WHERE `dungeon_id` = DungeonId"}
            ]
        },
        #{
            file => "src/module/map/map_data.erl",
            comment => "地图配置",
            include => ["map.hrl"],
            meta => [
                #{name => "get", sql => "SELECT #record{*} FROM `map_data` WHERE `map_id` = MapId"}
            ]
        },
        #{
            file => "src/module/monster/monster_data.erl",
            comment => "怪物配置",
            include => ["monster.hrl"],
            meta => [
                #{name => "get", sql => "SELECT #record{*} FROM `monster_data` WHERE `monster_id` = MonsterId"},
                #{name => "type", sql => "SELECT ALL `monster_id` FROM `monster_data` WHERE `type` = Type"},
                #{name => "all", sql => "SELECT ALL `monster_id` FROM `monster_data`"}
            ]
        },
        #{
            file => "src/module/guild/guild_data.erl",
            comment => "公会配置",
            include => ["guild.hrl"],
            meta => [
                #{name => "create_type", sql => "SELECT {*} FROM `guild_create_data` WHERE `type` = Type"},
                #{name => "level", sql => "SELECT `level` FROM `guild_level_data` WHERE Exp > `exp` ORDER BY `exp` DESC DEFAULT 0"}
            ]
        }
    ].
