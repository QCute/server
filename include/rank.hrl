%% 角色排行表
%% rank =====> rank
-record(rank, {
    type = 0,                                         %% 类型(select)
    rank = 0,                                         %% 排名
    key = 0,                                          %% 键 
    value = 0,                                        %% 值 
    time = 0,                                         %% 时间
    name = <<>>,                                      %% 名字 
    other = [],                                       %% 附加数据 
    flag = 1                                          %% 标识(flag),default(1) 
}).

