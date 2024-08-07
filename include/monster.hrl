%% 怪物配置表
-record(monster_data, {
    monster_id = 0,                                   %% 怪物ID
    type = 0,                                         %% 类型ID
    name = <<>>,                                      %% 怪物名称
    description = <<>>,                               %% 怪物描述
    level = 0,                                        %% 等级
    hp = 0,                                           %% 血量
    map_id = 0,                                       %% 地图ID
    camp = 0,                                         %% 阵营
    range = 0,                                        %% 攻击距离
    distance = 0,                                     %% 搜索距离
    relive_time = 0,                                  %% 复活时间
    act_type = [],                                    %% 动作类型
    act_script = [],                                  %% 动作脚本(enemy:敌人/role:玩家/monster:怪物/{monster,组ID}:特定怪物)
    skills = [],                                      %% 技能
    born_points = [],                                 %% 出生点
    award = []                                        %% 奖励
}).

