%% 角色任务表
%% quest =====> quest
-record(quest, {
    role_id = 0,                                      %% 角色ID(select) 
    quest_id = 0,                                     %% 任务ID 
    type = 0,                                         %% 类型 
    event = [],                                       %% 事件 
    target = 0,                                       %% 目标 
    number = 0,                                       %% 数量 
    compare = [],                                     %% 比较 
    award = 0,                                        %% 是否领取奖励 
    flag = 0                                          %% 标识(flag) 
}).

%% 任务配置表
%% quest_data =====> quest_data
-record(quest_data, {
    quest_id = 0,                                     %% 任务ID 
    type = 0,                                         %% 类型 
    pre_id = 0,                                       %% 前置任务 
    next_id = 0,                                      %% 后置任务 
    module = [],                                      %% 模块(validate(module)) 
    function = [],                                    %% 函数(validate(function)) 
    event = [],                                       %% 事件(validate(event)) 
    compare = [],                                     %% 比较模式(validate(compare)) 
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

