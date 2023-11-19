-ifndef(ACHIEVEMENT_HRL).
-define(ACHIEVEMENT_HRL, 'ACHIEVEMENT_HRL').

%% 角色成就表
-record(achievement, {
    role_id = 0,                                      %% 角色ID
    achievement_id = 0,                               %% 成就ID
    type = 0,                                         %% 类型
    flag = 0                                          %% 标识
}).

%% 成就配置表
-record(achievement_data, {
    achievement_id = 0,                               %% 成就ID
    type = 0,                                         %% 类型
    count_type = 0,                                   %% 统计类型
    pre_id = 0,                                       %% 前置成就
    next_id = 0,                                      %% 后置成就
    event = [],                                       %% 事件
    target = 0,                                       %% 目标
    number = 0,                                       %% 数量
    award = [],                                       %% 奖励
    title = <<>>,                                     %% 标题
    content = <<>>,                                   %% 内容
    description = <<>>                                %% 描述
}).

-endif.
