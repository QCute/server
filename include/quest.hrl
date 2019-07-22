%% 角色任务表
%% quest =====> quest
-record(quest, {
    role_id = 0,                                      %% 角色ID(select) 
    quest_id = 0,                                     %% 任务ID 
    group_id = 0,                                     %% 组ID 
    progress = [],                                    %% 进度 
    award = 0,                                        %% 是否领取奖励 
    extra = 0                                         %% 额外(ignore)(flag),default(0) 
}).

%% 任务配置表
%% quest_data =====> quest_data
-record(quest_data, {
    quest_id = 0,                                     %% 任务ID 
    group_id = 0,                                     %% 组ID 
    pre_id = 0,                                       %% 前置任务 
    next_id = 0,                                      %% 后置任务 
    condition = [],                                   %% 条件 
    progress = [],                                    %% 目标 
    award = []                                        %% 奖励 
}).

%% 任务条件
-record(quest_condition, {
    type,                                             %% 类型
    value = 0                                         %% 数值
}).

%% 任务进度
-record(quest_progress, {
    type,                                             %% 类型
    progress_id,                                      %% ID
    value = 0,                                        %% 数值
    mode                                              %% 比较模式
}).

%% 任务进度配置表
%% quest_progress_data =====> quest_progress_data
-record(quest_progress_data, {
    progress_id = 0,                                  %% 进度ID 
    event = [],                                       %% 事件 
    type = 0,                                         %% 类型 
    expect = 0,                                       %% 排除类型 
    value = 0,                                        %% 数值 
    compare = []                                      %% 比较模式 
}).

