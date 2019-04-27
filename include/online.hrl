%%%-------------------------------------------------------------------
%%% @doc
%%% user user online info
%%% @end
%%%-------------------------------------------------------------------

%% 玩家在线信息
%% online =====> online
-record(online, {
    id = 0,                                           %% id 
    account = <<>>,                                   %% 用户名 
    name = <<>>,                                      %% 昵称
    status = undefined,                               %% online/hosting
    pid = undefined,                                  %% 玩家进程pid 
    pid_sender = undefined,                           %% 玩家发送进程pid 
    socket = undefined                                %% 套接字
}).

