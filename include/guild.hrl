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
    exp = 0,                                          %% 经验 
    wealth = 0,                                       %% 财富 
    level = 0,                                        %% 等级 
    create_time = 0,                                  %% 时间(once) 
    guild_name = <<>>,                                %% 名字((once)/(update_name)) 
    notice = [],                                      %% 公告((once)/(update_notice)) 
    leader_id = <<>>,                                 %% 会长id 
    leader_name = <<>>,                               %% 会长名字 
    flag = undefined                                  %% 标识(flag) 
}).

%% 公会角色表
%% guild_role =====> guild_role
-record(guild_role, {
    guild_id = 0,                                     %% 公会ID(join(`guild`.`guild_id`)/(delete_guild_id)) 
    role_id = 0,                                      %% 角色ID(join(`role`.`role_id`)/(delete_role_id)) 
    job = 0,                                          %% 职位 
    join_time = 0,                                    %% 加入时间 
    leave_time = 0,                                   %% 离开时间 
    guild_name = <<>>,                                %% 帮派名(join(`guild`.`guild_name`)) 
    role_name = <<>>,                                 %% 角色名(join(`role`.`role_name`)) 
    role_pid = undefined,                             %% 角色Pid 
    role_sender_pid = undefined,                      %% 角色发送进程Pid 
    flag = undefined                                  %% 标识(flag) 
}).

%% 公会申请表
%% guild_apply =====> guild_apply
-record(guild_apply, {
    guild_id = 0,                                     %% 公会ID(join(`guild`.`guild_id`)/(delete_guild_id)) 
    role_id = 0,                                      %% 角色ID(join(`role`.`role_id`)/(delete_role_id)) 
    apply_time = 0,                                   %% 时间 
    guild_name = <<>>,                                %% 帮派名(join(`guild`.`guild_name`)) 
    role_name = <<>>,                                 %% 角色名(join(`role`.`role_name`)) 
    role_pid = undefined,                             %% 角色Pid 
    sender_pid = undefined,                           %% 角色发送进程Pid 
    flag = undefined                                  %% 标识(flag) 
}).

