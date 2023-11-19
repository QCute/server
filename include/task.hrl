-ifndef(TASK_HRL).
-define(TASK_HRL, 'TASK_HRL').

%% 角色任务表
-record(task, {
    role_id = 0,                                      %% 角色ID
    task_id = 0,                                      %% 任务ID
    type = 0,                                         %% 类型
    number = 0,                                       %% 数量
    is_award = 0,                                     %% 是否领取奖励
    flag = 0                                          %% 标识
}).

%% 任务配置表
-record(task_data, {
    task_id = 0,                                      %% 任务ID
    type = 0,                                         %% 类型
    pre_id = 0,                                       %% 前置任务
    next_id = 0,                                      %% 后置任务
    event = [],                                       %% 事件
    compare = [],                                     %% 比较模式
    target = 0,                                       %% 目标
    number = 0,                                       %% 数量
    condition = [],                                   %% 条件
    cost = [],                                        %% 消耗
    award = [],                                       %% 奖励
    title = <<>>,                                     %% 标题
    content = <<>>,                                   %% 内容
    description = <<>>                                %% 描述
}).

%% 任务条件
-record(task_condition, {
    type,                                             %% 类型
    value = 0                                         %% 数值
}).

%% 任务进度
-record(task_progress, {
    type,                                             %% 类型
    progress_id,                                      %% ID
    value = 0,                                        %% 数值
    mode                                              %% 比较模式
}).

-endif.
