-ifndef(DUNGEON_HRL).
-define(DUNGEON_HRL, 'DUNGEON_HRL').

%% 副本类型
-define(DUNGEON_TYPE_EXP,                             1). %% 经验副本
-define(DUNGEON_TYPE_COPPER,                          2). %% 铜币副本
-define(DUNGEON_TYPE_RUNE,                            3). %% 符文副本
-define(DUNGEON_TYPE_TREASURE,                        4). %% 寻宝副本
-define(DUNGEON_TYPE_BEAST,                           5). %% 神兽副本
-define(DUNGEON_TYPE_SOUL,                            6). %% 聚魂副本
-define(DUNGEON_TYPE_GLUTTON,                         7). %% 饕餮副本

%% 角色副本表
-record(dungeon, {
    role_id = 0,                                      %% 玩家ID
    dungeon_id = 0,                                   %% 副本ID
    type = 0,                                         %% 类型
    today_number = 0,                                 %% 今天次数
    total_number = 0,                                 %% 历史总次数
    is_pass = 0,                                      %% 是否通关
    flag = 0                                          %% 标识
}).

%% 副本配置表
-record(dungeon_data, {
    dungeon_id = 0,                                   %% 副本ID
    type = 0,                                         %% 类型
    condition = [],                                   %% 条件
    cost = [],                                        %% 消耗
    day_number = [],                                  %% 每日次数
    buy_number = [],                                  %% 购买次数
    map_id = 0,                                       %% 地图ID
    monsters = [],                                    %% 怪物
    boss = [],                                        %% Boss
    time = 0,                                         %% 时间
    award = [],                                       %% 奖励
    name = <<>>,                                      %% 名字
    description = <<>>                                %% 描述
}).

-endif.
