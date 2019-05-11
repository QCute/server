%% 玩家排行表
%% rank =====> rank
-record(rank, {
    type = 0,                                         %% 类型 
    key = 0,                                          %% 键 
    value = 0,                                        %% 值 
    time = 0,                                         %% 时间 
    rank = 0,                                         %% 排名 
    name = <<>>,                                      %% 名字(string) 
    other = 0,                                        %% 附加数据(0) 
    flag = 1                                          %% 标识(ignore)(flag)(1) 
}).

