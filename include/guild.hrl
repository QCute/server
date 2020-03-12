
%% 公会职位定义
-define(GUILD_JOB_LEADER,                             1). %% 主会长
-define(GUILD_JOB_VICE,                               2). %% 副会长
-define(GUILD_JOB_MEMBER,                             3). %% 成员


%% 公会状态
%% guild_state =====> guild_state
-record(guild_state, {
    tick,                                             %% 保存时间
    guild = [],                                       %% 公会
    role = [],                                        %% 角色
    apply = []                                        %% 申请
}).

%% 公会表
%% guild =====> guild
-record(guild, {
    guild_id = 0,                                     %% 公会id 
    exp = 0,                                          %% 经验 
    wealth = 0,                                       %% 财富 
    level = 0,                                        %% 等级 
    create_time = 0,                                  %% 时间(once) 
    guild_name = <<>>,                                %% 名字((once)/(update_name)) 
    notice = <<>>,                                    %% 公告((once)/(update_notice)) 
    leader_id = 0,                                    %% 会长id(join(`role`.`role_id`)/join(`vip`.`role_id`)) 
    leader_name = <<>>,                               %% 会长名字(join(`role`.`role_name`)) 
    leader_sex = 0,                                   %% 性别(join(`role`.`sex`)/default(0)) 
    leader_class = 0,                                 %% 会长名字(join(`role`.`classes`)) 
    leader_level = 0,                                 %% 职业(join(`role`.`level`)/default(0)) 
    leader_vip_level = 0,                             %% 会长名字(join(`vip`.`vip_level`)) 
    flag = 0                                          %% 标识(flag) 
}).

%% 公会角色表
%% guild_role =====> guild_role
-record(guild_role, {
    guild_id = 0,                                     %% 公会ID(join(`guild`.`guild_id`)) 
    role_id = 0,                                      %% 角色ID(join(`role`.`role_id`)/join(`vip`.`role_id`)) 
    job = 0,                                          %% 职位 
    wealth = 0,                                       %% 财富 
    join_time = 0,                                    %% 加入时间 
    leave_time = 0,                                   %% 离开时间 
    guild_name = <<>>,                                %% 帮派名(join(`guild`.`guild_name`)) 
    role_name = <<>>,                                 %% 角色名(join(`role`.`role_name`)) 
    sex = 0,                                          %% 性别(join(`role`.`sex`)/default(0)) 
    classes = 0,                                      %% 职业(join(`role`.`classes`)/default(0)) 
    level = 0,                                        %% 职业(join(`role`.`level`)/default(0)) 
    vip_level = 0,                                    %% VIP等级(join(`vip`.`vip_level`)/default(0)) 
    flag = 0                                          %% 标识(flag) 
}).

%% 公会申请表
%% guild_apply =====> guild_apply
-record(guild_apply, {
    guild_id = 0,                                     %% 公会ID(join(`guild`.`guild_id`)/(delete_guild_id)) 
    role_id = 0,                                      %% 角色ID(join(`role`.`role_id`)/join(`vip`.`role_id`)/(delete_role_id)) 
    apply_time = 0,                                   %% 时间 
    guild_name = <<>>,                                %% 帮派名(join(`guild`.`guild_name`)) 
    role_name = <<>>,                                 %% 角色名(join(`role`.`role_name`)) 
    sex = 0,                                          %% 性别(join(`role`.`sex`)/default(0)) 
    classes = 0,                                      %% 职业(join(`role`.`classes`)/default(0)) 
    level = 0,                                        %% 职业(join(`role`.`level`)/default(0)) 
    vip_level = 0,                                    %% VIP等级(join(`vip`.`vip_level`)/default(0)) 
    flag = 0                                          %% 标识(flag) 
}).

