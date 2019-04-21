%% 公会状态
%% guild_status =====> guild_status
-record(guild_status, {
    guild = 0,                                        %% 公会 
    player = 0,                                       %% 玩家 
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
    notice = <<>>,                                    %% 公告(update_notice) 
    leader_id = <<>>,                                 %% 会长id(ignore) 
    leader_name = <<>>,                               %% 会长名字(ignore) 
    extra = <<>>                                      %% 额外(ignore)(flag) 
}).

%% 公会玩家表
%% guild_player =====> guild_player
-record(guild_player, {
    guild_id = 0,                                     %% 公会id(`guild`.`guild_id`)(update_guild_id) 
    player_id = 0,                                    %% 玩家id(`player`.`id`) 
    job = 0,                                          %% 职位 
    join_time = 0,                                    %% 加入时间 
    leave_time = 0,                                   %% 离开时间 
    guild_name = <<>>,                                %% 帮派名(ignore)(`guild`.`guild_name`) 
    player_name = <<>>,                               %% 玩家名(ignore)(`player`.`name`) 
    player_pid = <<>>,                                %% 玩家Pid(ignore) 
    player_sender_pid = <<>>,                         %% 玩家发送进程Pid(ignore) 
    extra = <<>>                                      %% 额外(ignore)(flag) 
}).

