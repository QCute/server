%% 角色资产表
%% asset =====> asset
-record(asset, {
    role_id = 0,                                      %% 角色ID 
    gold = 0,                                         %% 金币 
    silver = 0,                                       %% 银币 
    copper = 0,                                       %% 铜币 
    coin = 0,                                         %% 硬币 
    exp = 0,                                          %% 经验 
    sliver_rate = 0,                                  %% 银币倍率(default(0)) 
    copper_rate = 0,                                  %% 铜币倍率(default(0)) 
    coin_rate = 0,                                    %% 硬币倍率(default(0)) 
    exp_rate = 0                                      %% 经验倍率(default(0)) 
}).

