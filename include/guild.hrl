-ifndef(GUILD_HRL).
-define(GUILD_HRL, 'GUILD_HRL').

%% 公会职位定义
-define(GUILD_JOB_LEADER,                             1). %% 主会长
-define(GUILD_JOB_VICE,                               2). %% 副会长
-define(GUILD_JOB_ELITE,                              3). %% 精英
-define(GUILD_JOB_ASSISTANT,                          4). %% 助理
-define(GUILD_JOB_MEMBER,                             5). %% 成员


%% 公会状态
-record(guild_state, {
    guild = [],                                       %% 公会
    role = [],                                        %% 角色
    apply = [],                                       %% 申请
    trigger = []                                      %% 触发器
}).

%% 公会表
-record(guild, {
    guild_id = 0,                                     %% 公会ID
    guild_name = <<>>,                                %% 名字
    exp = 0,                                          %% 经验
    wealth = 0,                                       %% 财富
    level = 0,                                        %% 等级
    create_time = 0,                                  %% 时间
    notice = <<>>,                                    %% 公告
    leader_role_id = 0,                               %% 会长角色ID
    leader_name = <<>>,                               %% 会长名字
    leader_sex = 0,                                   %% 性别
    leader_avatar = 0,                                %% 头像
    leader_class = 0,                                 %% 会长名字
    leader_level = 0,                                 %% 职业
    leader_vip_level = 0,                             %% 会长名字
    flag = 0                                          %% 标识
}).

%% 公会角色表
-record(guild_role, {
    guild_id = 0,                                     %% 公会ID
    role_id = 0,                                      %% 角色ID
    job = 0,                                          %% 职位
    wealth = 0,                                       %% 财富
    join_time = 0,                                    %% 加入时间
    leave_time = 0,                                   %% 离开时间
    guild_name = <<>>,                                %% 帮派名
    role_name = <<>>,                                 %% 角色名
    sex = 0,                                          %% 性别
    avatar = 0,                                       %% 头像
    classes = 0,                                      %% 职业
    level = 0,                                        %% 职业
    vip_level = 0,                                    %% VIP等级
    flag = 0                                          %% 标识
}).

%% 公会申请表
-record(guild_apply, {
    guild_id = 0,                                     %% 公会ID
    role_id = 0,                                      %% 角色ID
    apply_time = 0,                                   %% 时间
    guild_name = <<>>,                                %% 帮派名
    role_name = <<>>,                                 %% 角色名
    sex = 0,                                          %% 性别
    avatar = 0,                                       %% 头像
    classes = 0,                                      %% 职业
    level = 0,                                        %% 职业
    vip_level = 0,                                    %% VIP等级
    flag = 0                                          %% 标识
}).

-endif.
