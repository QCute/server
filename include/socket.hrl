%%%-------------------------------------------------------------------
%%% @doc
%%% socket define
%%% @end
%%%-------------------------------------------------------------------
-define(HEART_TIMEOUT,                                      60 * 1000). %% 心跳包超时时间
-define(PACK_HEAD_LENGTH,                                   4).         %% 协议头长度
-define(TCP_TIMEOUT,                                        60 * 1000). %% 解析协议超时时间
-define(HEART_TIMEOUT_TIME,                                 1).         %% 心跳包超时次数
-define(UNK_STATE_EVENT,                                    0).         %% 未知的状态事件
-define(CATCH_EXCEPT_ERROR_INFO,                            1).         %% 方法抛出异常导致停止
-define(HEART_TIMEOUT_ERROR,                                2).         %% 超时
-define(PEER_CLOSE_SOCKET,                                  3).         %% 对端关闭连接
-define(UNKNOWN_STATE_RETURN,                               4).         %% 状态方法未知返回类型
-define(SOCKET_REFERENCE_NOT_MATCH,                         5).         %% socket, refrence 不匹配
-define(LOGIN_HANDLE_RETURN_ERROR,                          6).         %% pp_acount 返回错误
-define(LOGIN_HANDLE_RETURN_UNK,                            7).         %% pp_acount 返回未知
-define(CREATE_ACCOUNT_RETURN_ERROR,                        8).         %% CREATE ACCOUNT 返回未知
-define(SELECT_ROLE_NOT_LOGIN,                              9).         %% 选择角色，但是未登陆
-define(LOGIN_ROUTING_UNKNOWN,                              10).        %% ROUTING LOGIN, 未知
-define(MOD_LOGIN_LOGIN_ERROR,                              11).        %% mod_login:login 错误
-define(CHECK_FORBID_ERROR,                                 12).        %% check_forbid 错误
-define(GAME_ROUTING_PROTOCOL_ERROR,                        13).        %% 游戏解析错误
-define(HEARD_PACK_FAST,                                    14).        %% 心跳包过快
-define(OTHER_PACK_FAST,                                    15).        %% 其他包过快
-define(WAIT_H5_HEAD_LENGTH_ERROR,                          16).        %% H5包长度错误
-define(WAIT_H5_HEAD_ERROR,                                 17).        %% H5协议头错误
-define(HTTP_REQUEST_NORMAL,                                18).        %% 后台请求正常结束
-define(FLASH_REQUEST_NORMAL,                               19).        %% falsh 安全沙箱请求正常结束
-define(WAIT_H5_HANDSHAKE_ERROR,                            20).        %% H5握手失败



-define(PROTOCOL_TYPE_TCP,                                  0).         %% socket类型：普通tcp
-define(PROTOCOL_TYPE_WS_HYBI,                              11).        %% socket类型：Websocket(hybi)
-define(PROTOCOL_TYPE_WS_HIXIE,                             12).        %% socket类型：Websocket(hixie)


%% state
%% 记录客户端进程
-record(client, {
    state,                        %% execute state & function name
    receiver,                     %% receive function callback
    socket_type,                  %% gen_tcp or ssl
    socket = undefiend,           %% socket
    reference = undefiend,        %% socket message reference
    last_h5_length = 0,           %% http协议长度
    masking_h5 = <<>>,            %% html5 掩码
    protocol = 0,                 %% 协议
    connect_type  = 0,            %% 1.flash安全沙箱 2.http, 3.tcp
    protocol_type = 0,            %% 协议类型, (默认)0-tcp 11-websocket(hybi) 12-websocket(hixie)
    login_state = 0,              %% 0登陆中, 1游戏中, 2游戏监控
    pid_sender = undefined,       %% 玩家发送进程
    user_pid = undefined,         %% 玩家进程
    user_id = 0,                  %% 玩家id
    login  = 0,                   %%
    site  = undefined,            %%
    user_name = undefined,        %%
    total_packet_count = 0,
    total_last_packet_time = 0,
    heart_last_time = 0,
    heart_error_count = 0,
    infant = 0,
    agent_id = 0,                 %% 代理ID
    annal = 0,                    %% 是否是内部账号
    timeout = 0                   %% 超时次数
}).
