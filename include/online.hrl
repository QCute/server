%%%-------------------------------------------------------------------
%%% @doc
%%% user online info
%%% @end
%%%-------------------------------------------------------------------
%% server allow state
-define(SERVER_STATE_REFUSE,                          1).
-define(SERVER_STATE_MASTER,                          2).
-define(SERVER_STATE_INSIDER,                         4).
-define(SERVER_STATE_NORMAL,                          8).
-define(SERVER_STATE_SILENT,                          16).
-define(SERVER_STATE_SILENT_WORLD,                    32).
-define(SERVER_STATE_SILENT_GUILD,                    64).
-define(SERVER_STATE_SILENT_PRIVATE,                  128).

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
    receiver_pid,                                     %% 角色接收进程pid
    socket                                            %% 套接字
}).

