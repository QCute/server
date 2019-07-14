%% buff配置表
%% buff_data =====> buff_data
-record(buff_data, {
    buff_id = 0,                                      %% 增益状态(Buff)ID 
    group_id = 0,                                     %% 组ID 
    type = 0,                                         %% 类型 
    time = 0,                                         %% 有效时间 
    name = <<>>,                                      %% 名字 
    effect = <<>>,                                    %% 效果 
    temporary = 0,                                    %% 是否临时的(切地图失效) 
    overlap_type = 0,                                 %% 叠加类型(0:不叠加/1:时间/2:数值/3:都叠加) 
    replace_buffs = <<>>,                             %% 替换Buffs 
    description = <<>>                                %% 描述 
}).

