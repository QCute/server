
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
    guild_name = <<>>,                                %% 名字(update_name)
    exp = 0,                                          %% 经验
    wealth = 0,                                       %% 财富
    level = 0,                                        %% 等级
    create_time = 0,                                  %% 时间
    notice = <<>>,                                    %% 公告(update_notice)
    leader_role_id = 0,                               %% 会长角色ID(join_on(`role`.`role_id`)/join_on(`vip`.`role_id`))
    leader_name = <<>>,                               %% 会长名字(join(`role`.`role_name`))
    leader_sex = 0,                                   %% 性别(join(`role`.`sex`))
    leader_avatar = 0,                                %% 头像(join(`role`.`avatar`))
    leader_class = 0,                                 %% 会长名字(join(`role`.`classes`))
    leader_level = 0,                                 %% 职业(join(`role`.`level`))
    leader_vip_level = 0,                             %% 会长名字(join(`vip`.`vip_level`))
    flag = 0                                          %% 标识(flag)
}).

%% 公会角色表
-record(guild_role, {
    guild_id = 0,                                     %% 公会ID(join_on(`guild`.`guild_id`))
    role_id = 0,                                      %% 角色ID(join_on(`role`.`role_id`)/join_on(`vip`.`role_id`))
    job = 0,                                          %% 职位
    wealth = 0,                                       %% 财富
    join_time = 0,                                    %% 加入时间
    leave_time = 0,                                   %% 离开时间
    guild_name = <<>>,                                %% 帮派名(join(`guild`.`guild_name`))
    role_name = <<>>,                                 %% 角色名(join(`role`.`role_name`))
    sex = 0,                                          %% 性别(join(`role`.`sex`))
    avatar = 0,                                       %% 头像(join(`role`.`avatar`))
    classes = 0,                                      %% 职业(join(`role`.`classes`))
    level = 0,                                        %% 职业(join(`role`.`level`))
    vip_level = 0,                                    %% VIP等级(join(`vip`.`vip_level`))
    flag = 0                                          %% 标识(flag)
}).

%% 公会申请表
-record(guild_apply, {
    guild_id = 0,                                     %% 公会ID(join_on(`guild`.`guild_id`)/(delete_by_guild_id))
    role_id = 0,                                      %% 角色ID(join_on(`role`.`role_id`)/join_on(`vip`.`role_id`)/(delete_by_role_id))
    apply_time = 0,                                   %% 时间
    guild_name = <<>>,                                %% 帮派名(join(`guild`.`guild_name`))
    role_name = <<>>,                                 %% 角色名(join(`role`.`role_name`))
    sex = 0,                                          %% 性别(join(`role`.`sex`))
    avatar = 0,                                       %% 头像(join(`role`.`avatar`))
    classes = 0,                                      %% 职业(join(`role`.`classes`))
    level = 0,                                        %% 职业(join(`role`.`level`))
    vip_level = 0,                                    %% VIP等级(join(`vip`.`vip_level`))
    flag = 0                                          %% 标识(flag)
}).

