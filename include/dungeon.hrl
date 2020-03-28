%% 角色副本表
%% dungeon =====> dungeon
-record(dungeon, {
    role_id = 0,                                      %% 玩家ID(select) 
    dungeon_id = 0,                                   %% 副本ID 
    type = 0,                                         %% 类型 
    today_number = 0,                                 %% 今天次数 
    total_number = 0,                                 %% 历史总次数 
    flag = 0                                          %% 标识(flag) 
}).

%% 副本配置表
%% dungeon_data =====> dungeon_data
-record(dungeon_data, {
    dungeon_id = 0,                                   %% 副本ID 
    type = 0,                                         %% 类型(validate(dungeon_type)) 
    event = [],                                       %% 事件(validate(event)) 
    condition = [],                                   %% 条件 
    cost = [],                                        %% 消耗 
    day_number = [],                                  %% 每日次数 
    buy_number = [],                                  %% 购买次数 
    module = [],                                      %% 代码模块(validate(module)) 
    function = [],                                    %% 入口函数(validate(function)) 
    map_id = 0,                                       %% 地图ID 
    monsters = [],                                    %% 怪物 
    boss = [],                                        %% Boss 
    time = 0,                                         %% 时间 
    award = [],                                       %% 奖励 
    name = <<>>,                                      %% 名字 
    description = <<>>                                %% 描述 
}).

