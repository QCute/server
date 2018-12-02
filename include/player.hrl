%% 玩家数据
%% user =====> user
-record(user, {
    player = 0,                   %% 玩家表 
    assets = 0,                   %% 资产表
    item = [],                    %% 物品表 
    bag = [],                     %% 装备背包 
    quest = [],                   %% 任务表 
    mail = [],                    %% 邮件表 
    friend = [],                  %% 好友表 
    shop = [],                    %% 商店表
    vip = [],
    id = undefined,               %% id(ignore)
    name = <<>>,
    nick = <<>>,
    guild_id = 0,
    guild_job = 0,
    pid = undefined,              %% 玩家进程pid(ignore) 
    pid_sender = undefined,       %% 玩家发送进程pid(ignore) 
    socket = undefined,           %% 套接字(ignore) 
    online_time = undefined,      %% 在线时间(ignore) 
    save_tick = undefined         %% 保存时间(ignore) 
}).

%% 玩家信息表
%% player =====> player
-record(player, {
    id = undefined,               %% id 
    name = <<>>,                  %% 用户名 
    nick = <<>>,                  %% 昵称 
    sex = 0,                      %% 性别
    level = 0,
    classes = 0,
    focus = []                    %% 关注
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

-record(vip, {vip, expire_time}).
-record(assets, {gold, silver, copper, score, point}).