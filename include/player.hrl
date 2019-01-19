%% 玩家数据
%% user =====> user
-record(user, {
    player = [],                  %% 玩家表 
    assets = [],                  %% 资产表(convert) 
    item = [],                    %% 物品表(convert) 
    bag = [],                     %% 装备背包(convert) 
    store = [],                   %% 装备背包(convert) 
    quest = [],                   %% 任务表(convert) 
    mail = [],                    %% 邮件表(convert) 
    friend = [],                  %% 好友表(convert) 
    shop = [],                    %% 商店表(convert) 
    vip = [],                     %% vip表(convert) 
    id = 0,                       %% id(ignore) 
    name = <<>>,                  %% 用户名 
    nick = <<>>,                  %% 昵称 
    pid = 0,                      %% 玩家进程pid(ignore) 
    pid_sender = 0,               %% 玩家发送进程pid(ignore) 
    socket = 0,                   %% 套接字(ignore) 
    online_time = 0,              %% 在线时间(ignore) 
    tick = 0,                     %% 保存时间(ignore) 
    timeout = 0                   %% 超时时间(ignore) 
}).

%% 玩家信息表
%% player =====> player
-record(player, {
    id = undefined,               %% ID 
    name = <<>>,                  %% 用户名(once)(update_name) 
    nick = <<>>,                  %% 昵称(once)(update_nick) 
    sex = 0,                      %% 性别 
    level = 0,                    %% 等级 
    classes = 0,                  %% 职业 
    focus = [],                   %% 关注(convert) 
    extra = undefined             %% 额外(ignore) 
}).

%% 玩家在线信息
%% online =====> online
-record(online, {
    id = 0,                       %% id 
    name = <<>>,                  %% 用户名 
    nick = <<>>,                  %% 昵称 
    pid = undefined,              %% 玩家进程pid 
    pid_sender = undefined,       %% 玩家发送进程pid 
    socket = undefined            %% 套接字 
}).

