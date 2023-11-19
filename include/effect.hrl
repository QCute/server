-ifndef(EFFECT_HRL).
-define(EFFECT_HRL, 'EFFECT_HRL').

%% 作用效果配置表
-record(effect_data, {
    effect_id = 0,                                    %% 效果ID 
    type = [],                                        %% 类型
    scope = [],                                       %% 作用范围
    condition = [],                                   %% 条件 
    ratio = [],                                       %% 概率 
    restrict = [],                                    %% 约束 
    operation = [],                                   %% 操作
    object = [],                                      %% 作用对象
    attribute = [],                                   %% 操作属性
    field = [],                                       %% 操作属性字段
    value = [],                                       %% 属性值 
    time = 0,                                         %% 效果时间 
    extra = [],                                       %% 额外 
    description = []                                  %% 描述 
}).

-endif.
