%% 怪物配置表
%% monster_data =====> monster_data
-record(monster_data, {
    monster_id = 0,                                   %% 怪物ID 
    name = <<>>,                                      %% 怪物名称 
    level = 1,                                        %% 等级
    hp = 0,                                           %% 血量 
    hp_recover = 0,                                   %% hp恢复速度 
    camp = 0,                                         %% 阵营 
    act = 0,                                          %% 动作AI
    act_type = 0,                                     %% 动作类型怪物种类
    act_script = [],                                  %% 动作脚本
    attack_distance = 0,                              %% 攻击距离 
    reborn_time = 0,                                  %% 重生时间 
    props = [],                                       %% 属性(convert) 
    skill = [],                                       %% 技能(convert) 
    award = [],                                       %% 奖励(convert) 
    born_points = [],                                 %% 出生点(convert) 
    timer_script = [],                                %% 定时脚本(convert) 
    born_script = [],                                 %% 出生时脚本(convert) 
    dead_script = []                                  %% 死亡时脚本(convert) 
}).

