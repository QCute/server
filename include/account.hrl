%% 角色信息表
%% account =====> account
-record(account, {
    role_id = 0,                                      %% 角色ID 
    agent_id = 0,                                     %% 代理ID 
    device = [],                                      %% 设备 
    device_type = [],                                 %% 设备类型 
    mac = [],                                         %% Mac地址 
    extra = []                                        %% 额外(ignore) 
}).

