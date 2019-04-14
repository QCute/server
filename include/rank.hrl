%% 排行
%% rank =====> rank
-record(rank, {
    type = 0,                                         %% 类型(select) 
    key = 0,                                          %% 键 
    value = 0,                                        %% 值 
    time = 0,                                         %% 时间 
    rank = 0,                                         %% 排名 
    name = <<>>,                                      %% 名字 
    other = 0,                                        %% 附加数据(0) 
    flag = 0                                          %% 标志(ignore)(flag)(0) 
}).

