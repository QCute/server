%%%------------------------------------------------------------------
%%% @doc
%%% user user online info
%%% @end
%%%------------------------------------------------------------------

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

