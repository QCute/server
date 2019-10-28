%% 怪物配置表
%% monster_data =====> monster_data
-record(monster_data, {
    monster_id = 0,                                   %% 怪物ID 
    monster_name = <<>>,                              %% 怪物名称 
    type = 0,                                         %% 怪物类型 
    level = 0,                                        %% 等级 
    hp = 0,                                           %% 血量 
    map_id = 0,                                       %% 地图ID 
    camp = 0,                                         %% 阵营 
    range = 0,                                        %% 攻击距离 
    relive_time = 0,                                  %% 复活时间 
    act_type = [],                                    %% 动作类型 
    act_script = [],                                  %% 动作脚本 
    skill = [],                                       %% 技能 
    born_points = [],                                 %% 出生点 
    award = []                                        %% 奖励 
}).

