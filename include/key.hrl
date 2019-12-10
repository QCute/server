%% 角色兑换码表
%% key =====> key
-record(key, {
    role_id = 0,                                      %% ID
    key = <<>>                                        %% 码
}).

%% 兑换码奖励配置表
%% key_award_data =====> key_award_data
-record(key_award_data, {
    type = 0,                                         %% 类型 
    only = 0,                                         %% 唯一 
    award = []                                        %% 奖励 
}).

