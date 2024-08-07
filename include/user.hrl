
%% 角色数据 (load/save/reset/clean/expire) 使用loop_maker生成load/save/reset/clean/expire代码
-record(user, {
    role,                                             %% 角色 (create/load/save/login/logout/reconnect/disconnect)
    device,                                           %% 设备 (create/load/save)
    asset,                                            %% 资产 (load/save)
    vip,                                              %% vip (load/save)
    count = [],                                       %% 计数 (load/save/reset)
    package,                                          %% 包裹 (load/save)
    item = [],                                        %% 物品 (load/save/expire)
    bag = [],                                         %% 装备背包
    body = [],                                        %% 身上装备
    store = [],                                       %% 仓库背包
    task = [],                                        %% 任务 (load/save)
    achievement = [],                                 %% 成就 (load/save)
    daily = [],                                       %% 日常 (load/save/reset)
    daily_active,                                     %% 日常活跃
    sign = [],                                        %% 签到 (load/reset)
    shop = [],                                        %% 商店 (load/save/reset)
    mail = [],                                        %% 邮件 (load/save/expire)
    notice = [],                                      %% 公告 (load/save)
    friend = [],                                      %% 好友 (load/save)
    chat = [],                                        %% 聊天 (load/save)
    buff = [],                                        %% Buff (load/save/expire)
    skill = [],                                       %% 技能 (load/save)
    fashion = [],                                     %% 时装 (load/save/expire)
    title = [],                                       %% 称号 (load/save/expire)
    bubble = [],                                      %% 气泡 (load/save/expire)
    dungeon = [],                                     %% 副本 (load/save/reset)
    location,                                         %% 地点 (load/save)
    charge,                                           %% 充值 (load/save/reset)
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

