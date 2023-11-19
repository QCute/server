-ifndef(ROLE_HRL).
-define(ROLE_HRL, 'ROLE_HRL').

%% 角色信息表
-record(role, {
    role_id = 0,                                      %% 角色ID
    role_name = <<>>,                                 %% 角色名
    server_id = 0,                                    %% 服务器ID
    account_name = <<>>,                              %% 账户
    origin_server_id = 0,                             %% 原服务器ID
    channel = <<>>,                                   %% 渠道
    type = 0,                                         %% 账户类型
    status = 0,                                       %% 账户状态
    sex = 0,                                          %% 性别
    avatar = 0,                                       %% 头像
    classes = 0,                                      %% 职业
    level = 0,                                        %% 等级
    is_online = 0,                                    %% 是否在线
    register_time = 0,                                %% 注册时间
    login_time = 0,                                   %% 登录时间
    logout_time = 0                                   %% 登出时间
}).

-endif.
