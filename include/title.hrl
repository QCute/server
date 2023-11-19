-ifndef(TITLE_HRL).
-define(TITLE_HRL, 'TITLE_HRL').

%% 角色称号表
-record(title, {
    role_id = 0,                                      %% 角色ID
    title_id = 0,                                     %% 称号ID
    type = 0,                                         %% 类型
    expire_time = 0,                                  %% 过期时间
    flag = 0                                          %% 标识
}).

%% 称号配置表
-record(title_data, {
    title_id = 0,                                     %% 称号ID
    type = 0,                                         %% 类型
    multi = [],                                       %% 同类型可否拥有多个
    is_unique = [],                                   %% 是否全服唯一
    expire_time = 0,                                  %% 过期时间
    attribute = [],                                   %% 属性
    name = <<>>,                                      %% 称号名字
    description = <<>>                                %% 称号描述
}).

-endif.
