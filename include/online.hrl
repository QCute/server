%%%-------------------------------------------------------------------
%%% @doc
%%% user online info
%%% @end
%%%-------------------------------------------------------------------
%% server allow state
-define(SERVER_STATE_MASTER,   3).
-define(SERVER_STATE_INSIDER,  2).
-define(SERVER_STATE_NORMAL,   1).
-define(SERVER_STATE_REFUSE,   0).

%% 角色在线信息
-record(online, {
    role_id = 0,                                      %% 角色ID
    role_name = <<>>,                                 %% 角色名
    account_id = <<>>,                                %% 账户ID
    account_name = <<>>,                              %% 帐户名
    level = 0,                                        %% 等级
    status,                                           %% online/hosting
    pid,                                              %% 角色进程pid
    sender_pid,                                       %% 角色发送进程pid
    receiver_pid,                                     %% 角色接收进程pid
    socket                                            %% 套接字
}).

