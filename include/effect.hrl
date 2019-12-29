%% 作用效果配置表
%% effect_data =====> effect_data
-record(effect_data, {
    effect_id = 0,                                    %% 效果ID 
    type = [],                                        %% 类型(validate(effect_type)) 
    scope = [],                                       %% 作用范围(validate(effect_scope)) 
    condition = [],                                   %% 条件 
    ratio = [],                                       %% 概率 
    operation = [],                                   %% 操作(validate(effect_operation)) 
    object = [],                                      %% 作用对象(validate(effect_object)) 
    attribute = [],                                   %% 操作属性(validate(effect_attribute)) 
    field = [],                                       %% 操作属性字段(validate(effect_field)) 
    value = [],                                       %% 属性值 
    time = 0,                                         %% 效果时间 
    extra = [],                                       %% 额外 
    description = []                                  %% 描述 
}).

