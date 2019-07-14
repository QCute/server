%% 技能配置表
%% skill_data =====> skill_data
-record(skill_data, {
    skill_id = 0,                                     %% 技能ID 
    group_id = 0,                                     %% 组ID 
    type = 0,                                         %% 类型(主动/被动) 
    name = <<>>,                                      %% 名字 
    condition = <<>>,                                 %% 学习条件 
    effect = <<>>,                                    %% 作用效果 
    cd = 0,                                           %% 冷却时间 
    radius = 0,                                       %% 作用半径
    distance = 0,                                     %% 攻击距离
    number = 0,                                       %% 作用对象数 
    buffs = <<>>,                                     %% 作用Buff 
    before_effects = <<>>,                            %% 效果前 
    hit_effects = <<>>,                               %% 击中效果 
    after_effects = <<>>,                             %% 效果后 
    description = <<>>                                %% 描述 
}).

-record(skill, {
    role_id,
    skill_id,
    level,
    extra
}).