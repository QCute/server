-ifndef(FRIEND_HRL).
-define(FRIEND_HRL, 'FRIEND_HRL').

%% 好友状态定义
-define(FRIEND_RELATION_APPLY,                        1). %% 申请
-define(FRIEND_RELATION_FRIEND,                       2). %% 好友
-define(FRIEND_RELATION_BLOCK,                        3). %% 拉黑
-define(FRIEND_RELATION_BE_BLOCK,                     4). %% 被拉黑

%% 角色好友表
-record(friend, {
    role_id = 0,                                      %% 用户ID
    friend_role_id = 0,                               %% 好友角色ID
    friend_name = <<>>,                               %% 好友名字
    sex = 0,                                          %% 好友性别
    avatar = 0,                                       %% 头像
    classes = 0,                                      %% 好友职业
    level = 0,                                        %% 等级
    vip_level = 0,                                    %% VIP等级
    is_online = 0,                                    %% 好友在线状态
    relation = 0,                                     %% 友好状态
    time = 0,                                         %% 时间
    flag = 0                                          %% 标识
}).

-endif.
