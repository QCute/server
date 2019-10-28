%% 角色信息表
%% role =====> role
-record(role, {
    role_id = undefined,                              %% 角色ID 
    role_name = <<>>,                                 %% 角色名((once)/(update_name)) 
    account = <<>>,                                   %% 账户(once) 
    level = 0,                                        %% 等级 
    sex = 0,                                          %% 性别 
    classes = 0,                                      %% 职业 
    item_size = 0,                                    %% 普通背包大小 
    bag_size = 0,                                     %% 装备背包大小 
    store_size = 0,                                   %% 仓库背包大小 
    online = 0,                                       %% 是否在线 
    server_id = 0,                                    %% 服ID 
    channel_id = 0,                                   %% 渠道ID 
    map = [],                                         %% 地图 
    device_id = [],                                   %% 设备ID 
    device_type = [],                                 %% 设备类型 
    mac = []                                          %% Mac地址 
}).

