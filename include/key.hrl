%% 激活码
%% key =====> key
-record(key, {
    player_id = 0,                                    %% ID
    key = <<>>                                        %% 码
}).

%% 激活码配置
%% data_key =====> data_key
-record(data_key, {
    key = <<>>,                                       %% 码(string) 
    type = 0                                          %% 类型
}).

%% 激活码奖励配置
%% data_key_award =====> data_key_award
-record(data_key_award, {
    type = 0,                                         %% 类型 
    only = 0,                                         %% 唯一 
    award = <<>>                                      %% 奖励
}).

