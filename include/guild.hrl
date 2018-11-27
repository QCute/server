%% 公会表
%% guild =====> guild
-record(guild, {
    id = undefined,               %% id 
    name = <<>>,                  %% 名字 
    create_time = 0,              %% 时间 
    exp = 0,                      %% 经验 
    wealth = 0,                   %% 财富 
    extra = 0                     %% 额外(ignore)(save_flag) 
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
    extra = 0                     %% 额外(ignore)(save_flag) 
}).

