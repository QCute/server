%% 角色兑换码表
-record(key, {
    role_id = 0,                                      %% 角色ID
    key = <<>>                                        %% 码
}).

-record(key_data, {
    key = <<>>,                                       %% 码
    type = 0                                          %% 类型
}).

%% 兑换码奖励配置表
-record(key_award_data, {
    type = 0,                                         %% 类型
    is_unique = [],                                   %% 是否唯一
    award = []                                        %% 奖励
}).

