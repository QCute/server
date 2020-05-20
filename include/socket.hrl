%%%-------------------------------------------------------------------
%%% @doc
%%% socket define
%%% @end
%%%-------------------------------------------------------------------
-define(PACKET_HEAD_LENGTH,                           4).         %% 协议头长度
-define(TCP_TIMEOUT,                                  60 * 1000). %% 解析协议超时时间

%% receiver state
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
    protocol_type = tcp,                              %% 协议类型, TCP(默认) WebSocket(Draft-HyBi-00) WebSocket(Draft-HiXie-76)
    protocol = 0,                                     %% 协议号
    login_state,                                      %% 登录状态
    role_id = 0,                                      %% 角色id
    role_pid,                                         %% 角色进程Pid
    heart_time = 0,                                   %% 心跳时间
    last_time = 0,                                    %% 最后包时间
    total_packet = 0                                  %% 总包数
}).

%% http content
-record(http, {
    method = <<>>,                                    %% 请求方法
    uri = <<>>,                                       %% 资源路径
    version = <<>>,                                   %% 版本
    fields = [],                                      %% 头部数据
    body = <<>>                                       %% 数据主体
}).
