%% 角色任务表表
%% quest =====> quest
-record(quest, {
    role_id = 0,                                      %% 角色ID(select)
    quest_id = 0,                                     %% 任务ID 
    group_id = 0,                                     %% 组ID 
    progress = [],                                    %% 进度(convert) 
    award = 0,                                        %% 是否领取奖励 
    extra = <<>>                                      %% 额外(ignore)(flag) 
}).

%% 任务配置表
%% quest_data =====> quest_data
-record(quest_data, {
    quest_id = 0,                                     %% 任务ID 
    group_id = 0,                                     %% 组ID 
    pre_id = 0,                                       %% 前置任务 
    next_id = 0,                                      %% 后置任务 
    condition = <<>>,                                 %% 条件 
    progress = <<>>,                                  %% 目标 
    award = <<>>                                      %% 奖励 
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