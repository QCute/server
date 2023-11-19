-ifndef(NOTICE_HRL).
-define(NOTICE_HRL, 'NOTICE_HRL').

%% 消息提示公告范围类型
-define(NOTICE_SCOPE_WORLD,                           1). %% 世界公告
-define(NOTICE_SCOPE_GUILD,                           2). %% 公会
-define(NOTICE_SCOPE_SCENE,                           3). %% 场景
-define(NOTICE_SCOPE_TEAM,                            4). %% 队伍

%% 消息提示公告形式类型
-define(NOTICE_TYPE_BOARD,                            1). %% 公告栏
-define(NOTICE_TYPE_MAIL,                             2). %% 邮件公告
-define(NOTICE_TYPE_CHAT,                             3). %% 聊天框提示
-define(NOTICE_TYPE_FLOAT,                            4). %% 漂浮提示
-define(NOTICE_TYPE_SCROLL,                           5). %% 滚动提示
-define(NOTICE_TYPE_POP,                              6). %% 弹出
-define(NOTICE_TYPE_DIALOG,                           7). %% 弹出框

%% notice table
-define(NOTICE,                                       notice).

%% 公告表
-record(notice, {
    notice_id = 0,                                    %% 公告ID
    type = 0,                                         %% 类型
    receive_time = 0,                                 %% 接收时间
    expire_time = 0,                                  %% 过期时间
    title = <<>>,                                     %% 标题
    content = <<>>,                                   %% 内容
    attachment = [],                                  %% 附件
    from = []                                         %% 来源
}).

%% 角色公告表
-record(notice_role, {
    role_id = 0,                                      %% 角色ID
    notice_id = 0,                                    %% 公告ID
    receive_time = 0,                                 %% 接收时间
    expire_time = 0,                                  %% 过期时间
    read_time = 0,                                    %% 读取时间
    title = <<>>,                                     %% 标题
    content = <<>>,                                   %% 内容
    flag = 0                                          %% 标识
}).

-endif.
