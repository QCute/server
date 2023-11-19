-ifndef(SIGN_HRL).
-define(SIGN_HRL, 'SIGN_HRL').

%% 角色签到表
-record(sign, {
    role_id = 0,                                      %% 角色ID
    login_day = 0,                                    %% 登录天数
    sign_total = 0,                                   %% 签到总数
    is_sign_today = 0                                 %% 今天是否签到
}).

%% 签到配置表
-record(sign_data, {
    day = 0,                                          %% 签到天数
    award = []                                        %% 奖励
}).

-endif.
