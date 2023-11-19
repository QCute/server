-ifndef(USER_HRL).
-define(USER_HRL, 'USER_HRL').

%% 用户数据
-record(user, {
    role,                                             %% 角色 
    device,                                           %% 设备 
    asset,                                            %% 资产 
    vip,                                              %% vip 
    count = [],                                       %% 计数 
    package,                                          %% 包裹 
    item = [],                                        %% 物品 
    bag = [],                                         %% 装备背包
    body = [],                                        %% 身上装备
    store = [],                                       %% 仓库背包
    task = [],                                        %% 任务 
    achievement = [],                                 %% 成就 
    daily = [],                                       %% 日常 
    daily_active,                                     %% 日常活跃
    sign = [],                                        %% 签到 
    shop = [],                                        %% 商店 
    mail = [],                                        %% 邮件 
    notice = [],                                      %% 公告 
    friend = [],                                      %% 好友 
    chat = [],                                        %% 聊天 
    buff = [],                                        %% Buff 
    skill = [],                                       %% 技能 
    fashion = [],                                     %% 时装 
    title = [],                                       %% 称号 
    bubble = [],                                      %% 气泡 
    dungeon = [],                                     %% 副本 
    location,                                         %% 地点 
    charge,                                           %% 充值 
    guild,                                            %% 公会
    role_id = 0,                                      %% 角色ID
    role_name = <<>>,                                 %% 角色名
    sender_pid,                                       %% 角色发送进程pid
    loop_timer,                                       %% 循环定时器
    node = local,                                     %% 所处节点
    total_attribute,                                  %% 总属性
    attributes = [],                                  %% 属性列表
    effect = [],                                      %% 效果
    trigger = [],                                     %% 触发器
    buffer = []                                       %% 发送缓存
}).

%% 通用成功类型
-type ok() :: ok | {ok, User :: #user{}} | {ok, term()} | {ok, term(), User :: #user{}}.

-endif.
