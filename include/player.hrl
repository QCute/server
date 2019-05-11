%% 玩家信息表
%% player =====> player
-record(player, {
    id = undefined,                                   %% ID 
    account = <<>>,                                   %% 用户名(once) 
    name = <<>>,                                      %% 昵称(once)(update_name) 
    sex = 0,                                          %% 性别 
    level = 0,                                        %% 等级 
    classes = 0,                                      %% 职业 
    item_size = 0,                                    %% 普通背包大小 
    bag_size = 0,                                     %% 装备背包大小 
    store_size = 0,                                   %% 仓库背包大小 
    focus = [],                                       %% 关注(convert) 
    server_id = 0,                                    %% 服务器ID 
    agent_id = 0,                                     %% 代理ID 
    device = <<>>,                                    %% 设备 
    device_type = <<>>,                               %% 设备类型 
    mac = <<>>,                                       %% Mac地址 
    extra = <<>>                                      %% 额外(ignore) 
}).

