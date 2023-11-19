-ifndef(ASSET_HRL).
-define(ASSET_HRL, 'ASSET_HRL').

%% 角色资产表
-record(asset, {
    role_id = 0,                                      %% 角色ID
    diamond = 0,                                      %% 钻石
    gold = 0,                                         %% 金币
    silver = 0,                                       %% 银币
    copper = 0,                                       %% 铜币
    coin = 0,                                         %% 硬币
    exp = 0                                           %% 经验
}).

-endif.
