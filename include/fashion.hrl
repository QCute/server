-ifndef(FASHION_HRL).
-define(FASHION_HRL, 'FASHION_HRL').

%% 玩家时装表
-record(fashion, {
    role_id = 0,                                      %% 玩家ID
    fashion_id = 0,                                   %% 时装ID
    type = 0,                                         %% 类型
    expire_time = 0,                                  %% 过期时间
    flag = 0                                          %% 标识
}).

%% 时装配置表
-record(fashion_data, {
    fashion_id = 0,                                   %% 时装ID
    type = 0,                                         %% 时装类型
    is_unique = [],                                   %% 是否全局唯一
    expire_time = 0,                                  %% 过期时间
    attribute = [],                                   %% 属性
    name = <<>>,                                      %% 时装名字
    description = <<>>                                %% 时装描述
}).

-endif.
