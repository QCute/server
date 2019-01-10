%% 公会状态
%% guild_status =====> guild_status
-record(guild_status, {
    guild = 0,                    %% 公会 
    player = 0,                   %% 玩家 
    apply = 0                     %% 申请 
}).

%% 公会表
%% guild =====> guild
-record(guild, {
    guild_id = undefined,         %% 公会id 
    guild_name = <<>>,            %% 名字 
    create_time = 0,              %% 时间 
    exp = 0,                      %% 经验 
    wealth = 0,                   %% 财富 
    notice = <<>>,                %% 公告(update_notice) 
    leader_id = undefined,        %% 会长id(ignore) 
    leader_name = <<>>,           %% 会长名字(ignore) 
    extra = undefined             %% 额外(ignore)(flag) 
}).

%% 公会玩家表
%% guild_player =====> guild_player
-record(guild_player, {
    guild_id = 0,                 %% 公会id(`guild`.`id`) 
    player_id = 0,                %% 玩家id(`player`.`id`) 
    job = 0,                      %% 职位 
    join_time = 0,                %% 加入时间 
    leave_time = 0,               %% 离开时间 
    guild_name = <<>>,            %% 帮派名(ignore)(`guild`.`name`) 
    player_name = <<>>,           %% 玩家名(ignore)(`player`.`name`) 
    player_nick = <<>>,           %% 玩家昵称(ignore)(`player`.`nick`) 
    extra = undefined             %% 额外(ignore)(flag) 
}).

