%% 红包信息表
%% lucky_money =====> lucky_money
-record(lucky_money, {
    lucky_money_id = 0,                               %% 红包ID 
    server_id = 0,                                    %% 服务器ID 
    role_id = 0,                                      %% 角色ID 
    role_name = <<>>,                                 %% 角色名 
    guild_id = 0,                                     %% 公会ID 
    guild_name = <<>>,                                %% 公会名 
    total_gold = 0,                                   %% 总金币 
    remain_gold = 0,                                  %% 剩余金币 
    total_number = 0,                                 %% 总人数 
    receive_number = 0,                               %% 已领取人数 
    receive_list = [],                                %% 领取列表 
    time = 0,                                         %% 发送时间 
    flag = 0                                          %% 标识(flag) 
}).

%% 红包角色表
%% lucky_money_role =====> lucky_money_role
-record(lucky_money_role, {
    lucky_money_id = 0,                               %% 红包ID 
    server_id = 0,                                    %% 服务器ID 
    role_id = 0,                                      %% 角色ID 
    role_name = <<>>,                                 %% 角色名 
    guild_id = 0,                                     %% 公会ID 
    guild_name = <<>>,                                %% 公会名 
    gold = 0,                                         %% 领取金币数 
    time = 0,                                         %% 领取时间 
    flag = 0                                          %% 标识(flag) 
}).

