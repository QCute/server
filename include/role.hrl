%% 角色信息表
%% role =====> role
-record(role, {
    role_id = 0,                                      %% 角色ID 
    role_name = <<>>,                                 %% 角色名((once)/(update_name)) 
    account = <<>>,                                   %% 账户(once) 
    type = 0,                                         %% 账户类型 
    level = 0,                                        %% 等级 
    sex = 0,                                          %% 性别 
    classes = 0,                                      %% 职业 
    item_size = 0,                                    %% 普通背包大小 
    bag_size = 0,                                     %% 装备背包大小 
    store_size = 0,                                   %% 仓库背包大小 
    online = 0,                                       %% 是否在线 
    online_time = 0,                                  %% 在线时间 
    register_time = 0,                                %% 注册时间 
    server_id = 0,                                    %% 服ID 
    channel_id = 0,                                   %% 渠道ID 
    map = [],                                         %% 地图 
    device_id = <<>>,                                 %% 设备ID 
    device_type = <<>>,                               %% 设备类型 
    mac = <<>>                                        %% Mac地址 
}).

