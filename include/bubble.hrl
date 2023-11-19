-ifndef(BUBBLE_HRL).
-define(BUBBLE_HRL, 'BUBBLE_HRL').

%% 聊天气泡数据
-record(bubble, {
    role_id = 0,                                      %% 角色ID
    bubble_id = 0,                                    %% 气泡ID
    type = 0,                                         %% 类型
    expire_time = 0,                                  %% 过期时间
    flag = 0                                          %% 标识
}).

%% 聊天气泡配置
-record(bubble_data, {
    bubble_id = 0,                                    %% 气泡ID
    type = 0,                                         %% 类型
    tag = 0,                                          %% 标签
    expire_time = 0,                                  %% 过期时间
    name = <<>>,                                      %% 气泡名称
    description = <<>>                                %% 气泡描述
}).

-endif.
