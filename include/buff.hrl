
%% buff type
-define(BUFF_TYPE_NORMAL,        1).
-define(BUFF_TYPE_ATTRIBUTE,     2).
-define(BUFF_TYPE_TIME,          3).

%% buff overlap type
-define(BUFF_OVERLAP_TYPE_NONE,  0).
-define(BUFF_OVERLAP_TYPE_TIME,  1).
-define(BUFF_OVERLAP_TYPE_VALUE, 2).
-define(BUFF_OVERLAP_TYPE_ALL,   3).

%% 角色buff表
%% buff =====> buff
-record(buff, {
    role_id = 0,                                      %% 角色ID(select_by_role_id)
    buff_id = 0,                                      %% 状态增益ID
    expire_time = 0,                                  %% 结束时间
    overlap = 1,                                      %% 叠加数
    flag = 0                                          %% 标识(flag)
}).

%% buff配置表
%% buff_data =====> buff_data
-record(buff_data, {
    buff_id = 0,                                      %% 增益状态(Buff)ID
    type = 0,                                         %% 类型
    expire_time = 0,                                  %% 过期时间
    attribute = [],                                   %% 属性
    effect = [],                                      %% 效果
    is_temporary = [],                                %% 是否临时的(切地图失效)(validate(boolean))
    overlap_type = 0,                                 %% 叠加类型(validate(overlap_type))
    name = <<>>,                                      %% 名字
    description = []                                  %% 描述
}).
