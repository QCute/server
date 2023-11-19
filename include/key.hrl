-ifndef(KEY_HRL).
-define(KEY_HRL, 'KEY_HRL').

%% 角色兑换码表
-record(key, {
    role_id = 0,                                      %% 角色ID
    key = <<>>                                        %% 码
}).

-record(key_data, {
    key = <<>>,                                       %% 码
    key_award_id = 0                                  %% 奖励ID
}).

%% 兑换码奖励配置表
-record(key_award_data, {
    key_award_id = 0,                                 %% 奖励ID
    is_unique = [],                                   %% 是否唯一
    award = []                                        %% 奖励
}).

-endif.
