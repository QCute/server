%% 活动配置表
-record(activity_data, {
    activity_id = 0,                                  %% 活动ID
    mode = 0,                                         %% 活动模式
    service = [],                                     %% 服务进程模块
    type = 0,                                         %% 类型
    subtype = 0,                                      %% 子类型
    award_type = 0,                                   %% 领奖类型(自动:0/手动:1)
    show_time = 0,                                    %% 图标展示时间(时间戳)
    start_time = 0,                                   %% 开始时间(时间戳)
    over_time = 0,                                    %% 结束时间(时间戳)
    award_time = 0,                                   %% 领奖时间(时间戳)
    stop_time = 0,                                    %% 图标消失时间(时间戳)
    show_hour = 0,                                    %% 每天展示小时
    start_hour = 0,                                   %% 每天开始小时
    over_hour = 0,                                    %% 每天结束小时
    start_award_hour = 0,                             %% 每天领奖开始小时
    over_award_hour = 0,                              %% 每天领奖结束小时
    min_open_days = 0,                                %% 最小开服天数
    max_open_days = 0,                                %% 最大开服天数
    name = <<>>,                                      %% 活动名
    icon = <<>>,                                      %% 活动图标
    entrance = <<>>,                                  %% 活动入口
    description = <<>>                                %% 活动描述
}).

%% 活动信息表
-record(activity, {
    activity_id = 0,                                  %% 活动ID
    show_time = 0,                                    %% 图标展示时间(时间戳)
    start_time = 0,                                   %% 开始时间(时间戳)
    over_time = 0,                                    %% 结束时间(时间戳)
    award_time = 0,                                   %% 领奖时间(时间戳)
    stop_time = 0                                     %% 图标消失时间(时间戳)
}).

