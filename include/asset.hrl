%% 角色资产表
%% asset =====> asset
-record(asset, {
    role_id = 0,                                      %% 角色ID
    gold = 0,                                         %% 元宝 
    silver = 0,                                       %% 银币 
    copper = 0,                                       %% 铜币 
    exp = 0                                           %% 经验 
}).
