%% 玩家时装表
%% fashion =====> fashion
-record(fashion, {
    role_id = 0,                                      %% 玩家ID(select_by_role_id)(update_role_id)
    fashion_id = 0,                                   %% 时装ID(select_by_fashion_id)
    type = 0,                                         %% 类型
    expire_time = 0,                                  %% 过期时间
    flag = 0                                          %% 标识(flag)
}).

%% 时装配置表
%% fashion_data =====> fashion_data
-record(fashion_data, {
    fashion_id = 0,                                   %% 时装ID
    type = 0,                                         %% 时装类型
    is_unique = [],                                   %% 是否全局唯一(validate(boolean))
    expire_time = 0,                                  %% 过期时间
    attribute = [],                                   %% 属性
    name = <<>>,                                      %% 时装名字
    description = <<>>                                %% 时装描述
}).

