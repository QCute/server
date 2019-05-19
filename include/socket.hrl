%%%-------------------------------------------------------------------
%%% @doc
%%% socket define
%%% @end
%%%-------------------------------------------------------------------
-define(HEART_TIMEOUT,                                60 * 1000). %% 心跳包超时时间
-define(PACKET_HEAD_LENGTH,                           4).         %% 协议头长度
-define(TCP_TIMEOUT,                                  60 * 1000). %% 解析协议超时时间

%% receiver state
%% 记录客户端进程
-record(client, {
    socket_type,                                      %% gen_tcp or ssl
    socket,                                           %% socket/port
    reference,                                        %% socket message reference
    state,                                            %% 读取器状态
    packet_length = 0,                                %% 内容长度
    packet = <<>>,                                    %% 内容
    http_header = <<>>,                               %% http 头
    h5_length = 0,                                    %% http 内容长度
    masking_h5 = <<>>,                                %% html5 掩码
    connect_type = 0,                                 %% 协议类型, tcp(默认) websocket(HyBi) websocket(HiXie)
    protocol = 0,                                     %% 协议号
    login_state = 0,                                  %% 登录状态
    user_id = 0,                                      %% 玩家id
    user_pid = undefined,                             %% 玩家进程Pid
    user_sender_pid = undefined,                      %% 玩家发送进程Pid
    user_account = <<>>,                              %% 玩家账户名字
    user_name = <<>>,                                 %% 玩家名字 
    total_packet_count = 0,                           %% 总包数
    total_last_packet_time = 0,                       %% 最后包时间
    heart_last_time = 0,                              %% 心跳包最后时间
    heart_error_count = 0,                            %% 错误心跳包数
    agent_id = 0,                                     %% 代理ID
    annal = 0,                                        %% 是否是内部账号
    timeout = 0                                       %% 超时次数
}).
