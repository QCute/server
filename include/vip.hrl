%% 角色vip表
%% vip =====> vip
-record(vip, {
    role_id = 0,                                      %% 角色id
    vip_level = 0,                                    %% vip等级
    exp = 0,                                          %% vip经验
    expire_time = 0                                   %% 过期时间
}).

