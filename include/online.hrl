%%%-------------------------------------------------------------------
%%% @doc
%%% user online info
%%% @end
%%%-------------------------------------------------------------------
%% server allow state
-define(SERVER_STATE_REFUSE,                          0).
-define(SERVER_STATE_NORMAL,                          1).
-define(SERVER_STATE_INSIDER,                         2).
-define(SERVER_STATE_MASTER,                          3).

%% server chat state
-define(CHAT_STATE_UNLIMITED,                         0).
-define(CHAT_STATE_SILENT_WORLD,                      1).
-define(CHAT_STATE_SILENT_GUILD,                      2).
-define(CHAT_STATE_SILENT_PRIVATE,                    4).
-define(CHAT_STATE_SILENT,                            ?CHAT_STATE_SILENT_WORLD bor ?CHAT_STATE_SILENT_GUILD bor ?CHAT_STATE_SILENT_PRIVATE).

%% 角色在线信息
-record(online, {
    role_id = 0,                                      %% 角色ID
    role_name = <<>>,                                 %% 角色名
    account_id = <<>>,                                %% 账户ID
    account_name = <<>>,                              %% 帐户名
    level = 0,                                        %% 等级
    sex = 0,                                          %% 性别
    classes = 0,                                      %% 职业
    state,                                            %% 在线状态 online/hosting
    pid,                                              %% 角色进程pid
    sender_pid,                                       %% 角色发送进程pid
    socket                                            %% 套接字
}).

