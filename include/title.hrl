%% 角色称号表
-record(title, {
    role_id = 0,                                      %% 角色ID(select_by_role_id)(update_role_id)
    title_id = 0,                                     %% 称号ID(select_by_title_id)
    type = 0,                                         %% 类型
    expire_time = 0,                                  %% 过期时间
    flag = 0                                          %% 标识(flag)
}).

%% 称号配置表
-record(title_data, {
    title_id = 0,                                     %% 称号ID
    type = 0,                                         %% 类型
    multi = [],                                       %% 同类型可否拥有多个(validate(boolean))
    is_unique = [],                                   %% 是否全服唯一(validate(boolean))
    expire_time = 0,                                  %% 过期时间
    attribute = [],                                   %% 属性
    name = <<>>,                                      %% 称号名字
    description = <<>>                                %% 称号描述
}).
