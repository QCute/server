-ifndef(SKILL_HRL).
-define(SKILL_HRL, 'SKILL_HRL').

%% 技能配置表
-record(skill_data, {
    skill_id = 0,                                     %% 技能ID
    type = [],                                        %% 类型
    classes = 0,                                      %% 职业
    name = <<>>,                                      %% 名字
    condition = [],                                   %% 学习条件
    cost = [],                                        %% 升级消耗
    attribute = [],                                   %% 属性
    effect = [],                                      %% 作用效果
    cd = 0,                                           %% 冷却时间
    radius = 0,                                       %% 作用半径
    distance = 0,                                     %% 作用距离
    number = 0,                                       %% 作用对象数
    buffs = [],                                       %% 作用Buff
    before_effects = [],                              %% 效果前
    hit_effects = [],                                 %% 击中效果
    after_effects = [],                               %% 效果后
    description = <<>>                                %% 描述
}).

%% 角色技能表
-record(skill, {
    role_id = 0,                                      %% 角色ID
    skill_id = 0,                                     %% 技能ID
    level = 0,                                        %% 等级
    flag = 0                                          %% 标识
}).

-endif.
