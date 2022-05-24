%% 角色信息表
%% role =====> role
-record(role, {
    role_id = 0,                                      %% 角色ID
    role_name = <<>>,                                 %% 角色名(update_name)
    server_id = 0,                                    %% 服务器ID
    account_name = <<>>,                              %% 账户
    origin_server_id = 0,                             %% 原服务器ID
    type = 0,                                         %% 账户类型
    status = 0,                                       %% 账户状态
    sex = 0,                                          %% 性别
    avatar = 0,                                       %% 头像
    classes = 0,                                      %% 职业
    level = 0,                                        %% 等级
    is_online = 0,                                    %% 是否在线
    register_time = 0,                                %% 注册时间
    login_time = 0,                                   %% 登录时间
    online_time = 0,                                  %% 在线时间
    logout_time = 0,                                  %% 登出时间
    world_chat_time = 0,                              %% 世界聊天时间
    guild_chat_time = 0,                              %% 公会聊天时间
    first_recharge_time = 0,                          %% 首充时间
    last_recharge_time = 0,                           %% 最后充值时间
    recharge_total = 0.00,                            %% 总充值
    item_size = 0,                                    %% 普通背包大小
    bag_size = 0,                                     %% 装备背包大小
    store_size = 0,                                   %% 仓库背包大小
    map = [],                                         %% 地图
    channel = <<>>,                                   %% 渠道
    device_id = <<>>,                                 %% 设备ID
    device_type = <<>>,                               %% 设备类型
    mac = <<>>,                                       %% Mac地址
    ip = <<>>                                         %% IP地址
}).

