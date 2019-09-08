%%%-------------------------------------------------------------------
%%% @doc
%%% user user
%%% @end
%%%-------------------------------------------------------------------

%% 角色数据 (load/save/clean) 使用lsc_maker生成load/save/clean代码
-record(user, {
    role = [],                                        %% 角色表 (load/save)
    asset = [],                                       %% 资产表 (load/save)
    vip = [],                                         %% vip表 (load/save)
    item = [],                                        %% 物品表 (load/save)
    bag = [],                                         %% 装备背包 
    store = [],                                       %% 仓库背包
    
    quest = [],                                       %% 任务表 (load/save)
    mail = [],                                        %% 邮件表 (load)
    friend = [],                                      %% 好友表 (load/save)
    shop = [],                                        %% 商店表 (load/save/clean)
    buff = [],                                        %% Buff (load/save/clean)
    skill = [],                                       %% 技能 (load/save)
    role_id = 0,                                      %% 角色ID
    role_name = <<>>,                                 %% 角色名
    account_id = <<>>,                                %% 账户ID
    account_name = <<>>,                              %% 帐户名
    server_id = 0,                                    %% 服务器ID
    pid,                                              %% 角色进程pid
    sender_pid,                                       %% 角色发送进程pid
    receiver_pid,                                     %% 角色接受器进程pid
    socket_type,                                      %% 套接字类型
    connect_type = 0,                                 %% 协议类型, tcp(默认) websocket(HyBi) websocket(HiXie)
    socket,                                           %% 套接字
    online_time = 0,                                  %% 在线时间
    tick = 1,                                         %% 保存时间
    timeout = 0,                                      %% 超时时间
    loop_timer,                                       %% 循环定时器
    logout_timer,                                     %% 退出定时器
    node = local,                                     %% 所处节点
    map,                                              %% 所处地图位置
    attributes = [],                                  %% 属性列表
    trigger = []                                      %% 触发器
}).


%% 通用成功类型
-type ok() :: ok | {ok, User :: #user{}} | {ok, [Code :: non_neg_integer()], User :: #user{}} | {ok, [term()], User :: #user{}}.
