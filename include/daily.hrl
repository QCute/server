-ifndef(DAILY_HRL).
-define(DAILY_HRL, 'DAILY_HRL').

%% 角色日常表
-record(daily, {
    role_id = 0,                                      %% 角色ID
    daily_id = 0,                                     %% 日常ID
    count_type = 0,                                   %% 统计类型
    number = 0,                                       %% 数量
    is_award = 0,                                     %% 是否领取奖励
    flag = 0                                          %% 标识
}).

%% 日常配置表
-record(daily_data, {
    daily_id = 0,                                     %% 日常ID
    type = 0,                                         %% 类型
    count_type = 0,                                   %% 统计类型
    number = 0,                                       %% 目标数量
    score = 0,                                        %% 活跃度
    award,                                            %% 奖励
    description = <<>>                                %% 描述
}).

%% 角色日常活跃表
-record(daily_active, {
    role_id = 0,                                      %% 角色ID
    stage_id = 0,                                     %% 奖励阶段ID
    score = 0                                         %% 活跃度
}).

%% 日常活跃奖励配置
-record(daily_active_data, {
    stage_id = 0,                                     %% 阶段ID
    pre_id = 0,                                       %% 前一个阶段ID
    next_id = 0,                                      %% 下一个阶段ID
    score = 0,                                        %% 所需活跃度
    award                                             %% 奖励
}).

-endif.