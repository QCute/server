%%%-------------------------------------------------------------------
%%% @doc
%%% chat define
%%% @end
%%%-------------------------------------------------------------------

%% chat type
-define(CHAT_TYPE_NORMAL,                             0). %% 文字
-define(CHAT_TYPE_SHARE,                              1). %% 分享
-define(CHAT_TYPE_VOICE,                              2). %% 语音
-define(CHAT_TYPE_LUCKY_MONEY,                        3). %% 红包

%% chat scope type
-define(SYSTEM_CHAT,                                  system_chat).  %% 系统聊天
-define(WORLD_CHAT,                                   world_chat).   %% 世界聊天
-define(GUILD_CHAT,                                   guild_chat).   %% 公会聊天
-define(PRIVATE_CHAT,                                 private_chat). %% 私人聊天

%% world chat
-record(world_chat, {
    id = 0,                                           %% ID
    role_id = 0,                                      %% 角色ID
    role_name = <<>>,                                 %% 角色名
    sex = 0,                                          %% 性别
    avatar = 0,                                       %% 头像
    classes = 0,                                      %% 职业
    level = 0,                                        %% 等级
    vip_level = 0,                                    %% Vip等级
    guild_id = 0,                                     %% 公会ID
    guild_name = <<>>,                                %% 公会名
    skin = 0,                                         %% 皮肤
    lucky_money_id = 0,                               %% 红包ID
    from = 0,                                         %% 来源
    type = 0,                                         %% 类型
    message = <<>>,                                   %% 内容
    time = 0                                          %% 时间
}).

%% system chat
-record(system_chat, {
    id = 0,                                           %% ID
    role_id = 0,                                      %% 角色ID
    role_name = <<>>,                                 %% 角色名
    sex = 0,                                          %% 性别
    avatar = 0,                                       %% 头像
    classes = 0,                                      %% 职业
    level = 0,                                        %% 等级
    vip_level = 0,                                    %% Vip等级
    guild_id = 0,                                     %% 公会ID
    guild_name = <<>>,                                %% 公会名
    guild_job = 0,                                    %% 公会职位
    skin = 0,                                         %% 皮肤
    lucky_money_id = 0,                               %% 红包ID
    from = 0,                                         %% 来源
    type = 0,                                         %% 类型
    message = <<>>,                                   %% 内容
    time = 0                                          %% 时间
}).

%% guild chat
-record(guild_chat, {
    id = 0,                                           %% ID
    role_id = 0,                                      %% 角色ID
    role_name = <<>>,                                 %% 角色名
    sex = 0,                                          %% 性别
    avatar = 0,                                       %% 头像
    classes = 0,                                      %% 职业
    level = 0,                                        %% 等级
    vip_level = 0,                                    %% Vip等级
    guild_id = 0,                                     %% 公会ID
    guild_name = <<>>,                                %% 公会名
    guild_job = 0,                                    %% 公会职位
    skin = 0,                                         %% 皮肤
    lucky_money_id = 0,                               %% 红包ID
    from = 0,                                         %% 来源
    type = 0,                                         %% 类型
    message = <<>>,                                   %% 内容
    time = 0                                          %% 时间
}).

%% private chat
-record(private_chat, {
    sender_id = 0,                                    %% 发送者ID
    receiver_id = 0,                                  %% 接收者ID
    skin = 0,                                         %% 皮肤
    lucky_money_id = 0,                               %% 红包ID
    type = 0,                                         %% 类型
    message = <<>>,                                   %% 内容
    time = 0                                          %% 时间
}).
