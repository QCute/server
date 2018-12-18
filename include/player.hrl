%% 玩家数据
%% user =====> user
-record(user, {
    player = undefined,           %% 玩家表(null) 
    assets = undefined,           %% 资产表(null) 
    item = undefined,             %% 物品表(null) 
    bag = undefined,              %% 装备背包(null) 
    quest = undefined,            %% 任务表(null) 
    mail = undefined,             %% 邮件表(null) 
    friend = undefined,           %% 好友表(null) 
    shop = undefined,             %% 商店表(null) 
    vip = undefined,              %% vip表(null) 
    id = 0,                       %% id(0) 
    name = <<>>,                  %% 用户名 
    nick = <<>>,                  %% 昵称 
    pid = undefined,              %% 玩家进程pid(null) 
    pid_sender = undefined,       %% 玩家发送进程pid(null) 
    socket = undefined,           %% 套接字(null) 
    online_time = 0,              %% 在线时间(0) 
    tick = 0,                     %% 保存时间(0) 
    timeout = 0                   %% 超时时间(0) 
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
    extra = undefined             %% 额外(ignore)(null) 
}).

%% 资产表
%% assets =====> assets
-record(assets, {
    gold = 0,                     %% 元宝 
    silver = 0,                   %% 银币 
    copper = 0,                   %% 铜币 
    exp = 0                       %% 经验 
}).

%% 玩家vip表
%% vip =====> vip
-record(vip, {
    player_id = 0,                %% 玩家id 
    vip = 0,                      %% vip等级 
    expire_time = 0               %% 过期时间 
}).

%% 玩家在线信息
%% online =====> online
-record(online, {
    id = 0,                       %% id 
    name = <<>>,                  %% 用户名 
    nick = <<>>,                  %% 昵称 
    pid = undefined,              %% 玩家进程pid 
    pid_sender = undefined        %% 玩家发送进程pid 
}).

