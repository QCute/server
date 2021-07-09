%%%-------------------------------------------------------------------
%%% @doc
%%% user define
%%% @end
%%%-------------------------------------------------------------------

%% 角色数据 (load/save/reset/clean/expire) 使用loop_maker生成load/save/reset/clean/expire代码
-record(user, {
    role = [],                                        %% 角色 (load/save/login/logout/reconnect/disconnect)
    asset = [],                                       %% 资产 (load/save)
    vip = [],                                         %% vip (load/save)
    count = [],                                       %% 计数 (load/save/reset)
    item = [],                                        %% 物品 (load/save/expire)
    bag = [],                                         %% 装备背包
    body = [],                                        %% 身上装备
    store = [],                                       %% 仓库背包
    quest = [],                                       %% 任务 (load/save)
    shop = [],                                        %% 商店 (load/save/reset)
    mail = [],                                        %% 邮件 (load/save/expire)
    friend = [],                                      %% 好友 (load/save)
    buff = [],                                        %% Buff (load/save/expire)
    skill = [],                                       %% 技能 (load/save)
    fashion = [],                                     %% 时装 (load/save/expire)
    title = [],                                       %% 称号 (load/save/expire)
    bubble = [],                                      %% 气泡 (load/save/expire)
    dungeon = [],                                     %% 副本 (load/save/reset)
    sign = [],                                        %% 签到 (load/reset)
    role_id = 0,                                      %% 角色ID
    role_name = <<>>,                                 %% 角色名
    server_id = 0,                                    %% 服务器ID
    account_name = <<>>,                              %% 帐户名
    sender_pid,                                       %% 角色发送进程pid
    loop_timer,                                       %% 循环定时器
    node = local,                                     %% 所处节点
    total_attribute,                                  %% 总属性
    attributes = [],                                  %% 属性列表
    world_chat_time = 0,                              %% 世界聊天时间
    guild_chat_time = 0,                              %% 公会聊天时间
    guild_id = 0,                                     %% 公会ID
    guild_name = <<>>,                                %% 公会名
    guild_job = 0,                                    %% 公会职位
    guild_wealth = 0,                                 %% 公会个人财富
    effect = [],                                      %% 效果
    trigger = []                                      %% 触发器
}).


%% 通用成功类型
-type ok() :: ok | {ok, User :: #user{}} | {ok, term()} | {ok, term(), User :: #user{}}.
