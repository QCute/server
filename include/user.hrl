%%%-------------------------------------------------------------------
%%% @doc
%%% user user
%%% @end
%%%-------------------------------------------------------------------

%% 玩家数据
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
    connect_type = 0,                                 %% 协议类型, tcp(默认) websocket(HyBi) websocket(HiXie)
    socket,                                           %% 套接字
    online_time = 0,                                  %% 在线时间
    tick = 0,                                         %% 保存时间 
    timeout = 0,                                      %% 超时时间
    loop_timer,                                       %% 循环定时器
    logout_timer,                                     %% 退出定时器
    node = local,                                     %% 所处节点
    map,                                              %% 所处地图位置
    attributes = [],                                  %% 属性列表
    attributes_percent = []                           %% 百分比属性列表
}).


%% 通用成功类型
-type ok() :: ok | {ok, User :: #user{}} | {reply, non_neg_integer(), User :: #user{}}.