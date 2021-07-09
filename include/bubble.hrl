%% 聊天气泡数据
%% bubble =====> bubble
-record(bubble, {
    role_id = 0,                                      %% 角色ID(select_by_role_id)
    bubble_id = 0,                                    %% 气泡ID
    type = 0,                                         %% 类型
    expire_time = 0,                                  %% 过期时间
    flag = 0                                          %% 标识(flag)
}).

%% 聊天气泡配置
%% bubble_data =====> bubble_data
-record(bubble_data, {
    bubble_id = 0,                                    %% 气泡ID
    type = 0,                                         %% 类型
    tag = 0,                                          %% 标签
    expire_time = 0,                                  %% 过期时间
    name = <<>>,                                      %% 气泡名称
    description = <<>>                                %% 气泡描述
}).

