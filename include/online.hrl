-ifndef(ONLINE_HRL).
-define(ONLINE_HRL, 'ONLINE_HRL').

%% server allow state
-define(SERVER_STATE_BAN,                             0).
-define(SERVER_STATE_NORMAL,                          1).
-define(SERVER_STATE_INSIDER,                         2).
-define(SERVER_STATE_MASTER,                          3).

%% server chat state
-define(CHAT_STATE_NORMAL,                            0).
-define(CHAT_STATE_BAN_WORLD,                         1).
-define(CHAT_STATE_BAN_GUILD,                         2).
-define(CHAT_STATE_BAN_SCENE,                         4).
-define(CHAT_STATE_BAN_PRIVATE,                       8).
-define(CHAT_STATE_BAN,                               ?CHAT_STATE_BAN_WORLD bor ?CHAT_STATE_BAN_GUILD bor ?CHAT_STATE_BAN_SCENE bor ?CHAT_STATE_BAN_PRIVATE).

%% 角色在线信息
-record(online, {
    role_id = 0,                                      %% 角色ID
    role_name = <<>>,                                 %% 角色名
    server_id = <<>>,                                 %% 服务器ID
    account_name = <<>>,                              %% 帐户名
    sex = 0,                                          %% 性别
    avatar = 0,                                       %% 头像
    classes = 0,                                      %% 职业
    level = 0,                                        %% 等级
    vip_level = 0,                                    %% Vip等级
    guild_id = 0,                                     %% 公会ID
    guild_name = <<>>,                                %% 公会名
    guild_job = 0,                                    %% 公会职位
    state,                                            %% 在线状态 online/hosting
    pid,                                              %% 角色进程pid
    sender_pid,                                       %% 角色发送进程pid
    socket,                                           %% 套接字
    node                                              %% 节点
}).

-endif.
