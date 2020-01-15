%%%------------------------------------------------------------------
%%% @doc
%%% module excel script
%%% @end
%%%------------------------------------------------------------------
-module(excel_script).
-export([main/1]).
%%%==================================================================
%%% API functions
%%%==================================================================
main(Args) ->
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    io:format("~p~n", [catch parse(Args)]).

%% make xml sheet file
parse(["excel", "xml", Table]) ->
    excel_maker:to_xml(Table, validity_data());
parse(["xml", Table]) ->
    excel_maker:to_xml(Table, validity_data());
parse(["excel", "xml", Table, Path | _]) ->
    excel_maker:to_xml(Table, Path);
parse(["xml", Table, Path | _]) ->
    excel_maker:to_xml(Table, Path);
parse(["excel", "table", _File, "-encode" | Encode]) ->
    %% windows nt file name param list convert to integer list
    File = [list_to_integer(I) || I <- Encode],
    excel_maker:to_table(File);
parse(["table", _File, "-encode" | Encode]) ->
    %% windows nt gbk character set
    %% windows nt file name param list convert to integer list
    File = [list_to_integer(I) || I <- Encode],
    excel_maker:to_table(File);
%% import xml sheet data to database
parse(["excel", "table", Table | _]) ->
    excel_maker:to_table(Table);
parse(["table", File | _]) ->
    excel_maker:to_table(File);
%% argument error
parse(_) ->
    io:format("invalid arguments~n").

%% excel row validity data
validity_data() ->
    [
        {bool, [
            {1, "是"},
            {1, "否"}
        ]},
        {boolean, [
            {true, "是"},
            {false, "否"}
        ]},
        {compare, [
            {eq, "等于"},
            {ge, "大于等于"},
            {gt, "大于"},
            {le, "小于等于"},
            {lt, "小于"},
            {nc, "不比较"},
            {ne, "不等于"}
        ]},
        {asset, [
            {gold, "金币"},
            {sliver, "银币"},
            {copper, "铜币"},
            {coin, "硬币"},
            {exp, "经验"},
            {"", "无"}
        ]},
        {item_type, [
            {1, "道具"},
            {2, "装备"},
            {3, "身上"},
            {4, "仓库"},
            {5, "符文"},
            {6, "寻宝"},
            {7, "神兽"},
            {8, "聚魂"},
            {9, "饕餮"},
            {10, "资产"}
        ]},
        {act_script, [
            {enemy, "敌人"},
            {location, "位置"},
            {monster, "怪物"},
            {role, "玩家"}
        ]},
        {act_type, [
            {active, "主动"},
            {fix, "固定"},
            {movable, "移动"},
            {passive, "被动"}
        ]},
        {classes, [
            {0, "无限制"},
            {1, "七杀"},
            {2, "天师"},
            {3, "飞羽"},
            {4, "御灵"},
            {5, "妙音"},
            {6, "星术"}
        ]},
        {activity_service, [
            {"", "无"}
        ]},
        {compare, [
            {eq, "等于"},
            {ge, "大于等于"},
            {gt, "大于"},
            {le, "小于等于"},
            {lt, "小于"},
            {nc, "不比较"},
            {ne, "不等于"}
        ]},
        {effect_attribute, [
            {asset, "资产"},
            {attribute, "属性"},
            {buff, "Buff"},
            {hurt, "伤害"},
            {skill, "技能"}
        ]},
        {effect_field, [
            {"", "无"},
            {fc, "战力"},
            {hp, "血量"},
            {attack, "攻击"},
            {defense, "防御"},
            {health, "生命"},
            {hit, "命中"},
            {duck, "闪避"},
            {freeze, "冰冻"},
            {destroy, "毁灭"},
            {vertigo, "眩晕"},
            {copper, "铜币"},
            {exp, "经验"}
        ]},
        {effect_object, [
            {mate, "队友"},
            {rival, "对方"},
            {self, "自己"}
        ]},
        {effect_operation, [
            {add, "增加"},
            {clear, "清除"},
            {reduce, "减少"},
            {set, "设置"}
        ]},
        {effect_scope, [
            {battle, "战斗"},
            {user, "玩家"}
        ]},
        {effect_type, [
            {active, "主动"},
            {buff, "Buff"},
            {passive, "被动"}
        ]},
        {event, [
            {event_add_friend, "添加好友"},
            {event_guild_join, "加入公会"},
            {event_kill_monster, "杀怪"},
            {event_level_upgrade, "升级"},
            {event_dungeon_passed, "通关副本"},
            {event_shop_buy, "商店购买"},
            {event_friend_add, "添加好友"},
            {"", "无"}
        ]},
        {module, [
            {role, "角色"},
            {friend, "好友"},
            {shop, "商店"},
            {dungeon_copper_map, "铜币副本"},
            {dungeon_exp_map, "经验副本"},
            {"", "无"}
        ]},
        {function, [
            {check_quest, "检查任务"},
            {start, "开始"},
            {"", "无"}
        ]},
        {node_type_atom, [
            {center, "跨服"},
            {center_world, "跨服和大世界"},
            {local, "本地"},
            {local_center, "本地和跨服"},
            {local_center_world, "全部"},
            {local_world, "本地和大世界"},
            {world, "大世界"}
        ]},
        {node_type_integer, [
            {1, "本地"},
            {2, "跨服"},
            {3, "本地和跨服"},
            {4, "大世界"},
            {5, "本地和大世界"},
            {6, "跨服和大世界"},
            {7, "全部"}
        ]},
        {sex, [
            {0, "无限制"},
            {1, "男性"},
            {2, "女性"}
        ]},
        {skill_type, [
            {active, "主动"},
            {passive, "被动"}
        ]},
        {use_effect, [
            {"", "无"},
            {gold, "金币"},
            {sliver, "银币"},
            {copper, "铜币"},
            {coin, "硬币"},
            {exp, "经验"}
        ]},
        {dungeon_type, [
            {0, "无"},
            {1, "经验副本"},
            {2, "铜币副本"}
        ]},
        {map_type, [
            {full, "全图"},
            {slice, "九宫格"}
        ]},
        {map_rank_mode, [
            {global, "全局"},
            {local, "不共享"},
            {share, "共享"},
            {"", "不用排行"}
        ]},
        {map_rank_key, [
            {camp, "阵营"},
            {guild, "公会"},
            {self, "个人"},
            {team, "队伍"},
            {"", "无"}
        ]},
        {map_rank_value, [
            {hurt, "伤害"},
            {"", "无"}
        ]}
    ].
