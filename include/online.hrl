%%%-------------------------------------------------------------------
%%% @doc
%%% user user online info
%%% @end
%%%-------------------------------------------------------------------

%% 角色在线信息
%% online =====> online
-record(online, {
    role_id = 0,                                      %% 角色ID
    role_name = <<>>,                                 %% 角色名
    account_id = <<>>,                                %% 账户ID
    account_name = <<>>,                              %% 帐户名
    level = 0,                                        %% 等级
    status = undefined,                               %% online/hosting
    pid = undefined,                                  %% 角色进程pid
    sender_pid = undefined,                           %% 角色发送进程pid
    receiver_pid = undefined,                         %% 角色接收进程pid
    socket = undefined                                %% 套接字
}).

