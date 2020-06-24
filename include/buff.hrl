
%% Buff overlap type
-define(BUFF_OVERLAP_TYPE_NONE,  0).
-define(BUFF_OVERLAP_TYPE_TIME,  1).
-define(BUFF_OVERLAP_TYPE_VALUE, 2).
-define(BUFF_OVERLAP_TYPE_ALL,   3).

%% buff配置表
%% buff_data =====> buff_data
-record(buff_data, {
    buff_id = 0,                                      %% 增益状态(Buff)ID 
    type = 0,                                         %% 类型 
    time = 0,                                         %% 有效时间 
    effect = [],                                      %% 效果 
    temporary = 0,                                    %% 是否临时的(切地图失效) 
    overlap_type = 0,                                 %% 叠加类型(0:不叠加/1:时间/2:数值/3:都叠加) 
    name = <<>>,                                      %% 名字 
    description = []                                  %% 描述 
}).

%% 角色buff表
%% buff =====> buff
-record(buff, {
    role_id = 0,                                      %% 角色ID(select) 
    buff_id = 0,                                      %% 状态增益ID 
    expire_time = 0,                                  %% 结束时间 
    overlap = 1,                                      %% 叠加数 
    flag = 0                                          %% 标识(flag) 
}).

