%% 公会状态
%% guild_state =====> guild_state
-record(guild_state, {
    tick,
    timeout,
    guild = 0,                                        %% 公会 
    role = 0,                                         %% 角色
    apply = 0                                         %% 申请 
}).

%% 公会表
%% guild =====> guild
-record(guild, {
    guild_id = undefined,                             %% 公会id 
    guild_name = <<>>,                                %% 名字(update_name) 
    create_time = 0,                                  %% 时间(once) 
    exp = 0,                                          %% 经验 
    wealth = 0,                                       %% 财富 
    level = 0,                                        %% 等级(update_level) 
    notice = [],                                      %% 公告(update_notice) 
    leader_id = <<>>,                                 %% 会长id 
    leader_name = <<>>,                               %% 会长名字 
    extra = 0                                         %% 额外(flag),default(0) 
}).

%% 公会角色表
%% guild_role =====> guild_role
-record(guild_role, {
    guild_id = 0,                                     %% 公会ID,on(`guild`.`guild_id`)(update_guild_id) 
    role_id = 0,                                      %% 角色ID,on(`role`.`role_id`) 
    job = 0,                                          %% 职位 
    join_time = 0,                                    %% 加入时间 
    leave_time = 0,                                   %% 离开时间 
    guild_name = <<>>,                                %% 帮派名(ignore),on(`guild`.`guild_name`) 
    role_name = <<>>,                                 %% 角色名(ignore),on(`role`.`role_name`) 
    role_pid = undefined,                             %% 角色Pid(ignore) 
    role_sender_pid = undefined,                      %% 角色发送进程Pid(ignore) 
    extra = 0                                         %% 额外(ignore)(flag),default(0) 
}).

%% 公会申请表
%% guild_apply =====> guild_apply
-record(guild_apply, {
    role_id = 0,                                      %% 角色ID,on(`role`.`role_id`)(delete_role) 
    guild_id = 0,                                     %% 公会ID(delete_guild) 
    time = 0,                                         %% 时间 
    role_name = <<>>,                                 %% 角色名(ignore),on(`role`.`role_name`) 
    role_pid = undefined,                             %% 角色Pid(ignore) 
    sender_pid = undefined,                           %% 角色发送进程Pid(ignore) 
    extra = 0,                                        %% 额外(ignore),default(0) 
    flag = 0                                          %% 标识(ignore)(flag),default(0) 
}).

