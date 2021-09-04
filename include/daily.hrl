%% 角色日常表
%% daily =====> daily
-record(daily, {
    role_id = 0,                                      %% 角色ID(select_by_role_id)
    daily_id = 0,                                     %% 日常ID
    is_award = 0,                                     %% 是否领取奖励
    flag = 0                                          %% 标识(flag)
}).

%% 日常配置表
%% daily_data =====> daily_data
-record(daily_data, {
    daily_id = 0,                                     %% 日常ID
    type = 0,                                         %% 类型
    count_type = 0,                                   %% 统计类型
    number = 0,                                       %% 目标数量
    score = 0,                                        %% 活跃度
    award = []                                        %% 奖励
}).

%% 角色日常活跃表
%% daily_active =====> daily_active
-record(daily_active, {
    role_id = 0,                                      %% 角色ID
    stage_id = 0,                                     %% 奖励阶段ID
    score = 0                                         %% 活跃度
}).

%% 日常活跃奖励配置
%% daily_active_data =====> daily_active_data
-record(daily_active_data, {
    stage_id = 0,                                     %% 阶段ID
    pre_id = 0,                                       %% 前一个阶段ID
    next_id = 0,                                      %% 下一个阶段ID
    score = 0,                                        %% 所需活跃度
    award = []                                        %% 奖励
}).

