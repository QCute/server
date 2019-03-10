%% 玩家数据
%% user =====> user
-record(user, {
    player = [],                                      %% 玩家表
    assets = [],                                      %% 资产表 
    item = [],                                        %% 物品表 
    bag = [],                                         %% 装备背包 
    store = [],                                       %% 仓库背包 
    quest = [],                                       %% 任务表 
    mail = [],                                        %% 邮件表 
    friend = [],                                      %% 好友表 
    shop = [],                                        %% 商店表 
    vip = [],                                         %% vip表
    id = 0,                                           %% id
    account = <<>>,                                   %% 帐户名
    name = <<>>,                                      %% 昵称 
    pid,                                              %% 玩家进程pid
    pid_sender,                                       %% 玩家发送进程pid
    pid_receiver,                                     %% 玩家接受器进程pid
    socket_type,                                      %% 套接字类型
    socket,                                           %% 套接字
    online_time = 0,                                  %% 在线时间
    tick = 0,                                         %% 保存时间 
    timeout = 0,                                      %% 超时时间
    loop_timer,                                       %% 循环定时器
    logout_timer,                                     %% 退出定时器
    location = local                                  %% 所处节点
}).

%% 玩家在线信息
%% online =====> online
-record(online, {
    id = 0,                                           %% id 
    account = <<>>,                                   %% 用户名 
    name = <<>>,                                      %% 昵称
    status = undefined,                               %% online/hosting
    pid = undefined,                                  %% 玩家进程pid 
    pid_sender = undefined,                           %% 玩家发送进程pid 
    socket = undefined                                %% 套接字
}).

%% 玩家信息表
%% player =====> player
-record(player, {
    id = undefined,                                   %% ID 
    account = <<>>,                                   %% 用户名(once) 
    name = <<>>,                                      %% 昵称(once)(update_name) 
    sex = 0,                                          %% 性别 
    level = 0,                                        %% 等级 
    classes = 0,                                      %% 职业 
    focus = [],                                       %% 关注(convert) 
    extra = undefined                                 %% 额外(ignore)
}).

