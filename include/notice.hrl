%%%------------------------------------------------------------------
%%% @doc
%%% notice define
%%% @end
%%%------------------------------------------------------------------

%% 消息提示公告范围类型
-define(NOTICE_SCOPE_WORLD,                           1). %% 世界公告
-define(NOTICE_SCOPE_GUILD,                           2). %% 公会
-define(NOTICE_SCOPE_TEAM,                            3). %% 队伍
-define(NOTICE_SCOPE_SCENE,                           4). %% 场景

%% 消息提示公告形式类型
-define(NOTICE_TYPE_CHAT,                             1). %% 聊天框提示
-define(NOTICE_TYPE_FLOAT,                            2). %% 漂浮提示
-define(NOTICE_TYPE_SCROLL,                           3). %% 滚动提示
-define(NOTICE_TYPE_POP,                              4). %% 弹出
-define(NOTICE_TYPE_DIALOG,                           5). %% 弹出框
