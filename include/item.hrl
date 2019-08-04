
%% 背包类型
-define(ITEM_TYPE_COMMON,                             1). %% 普通背包(道具背包)
-define(ITEM_TYPE_EQUIPMENT,                          2). %% 装备背包
-define(ITEM_TYPE_STORE,                              3). %% 仓库背包

%% 角色物品表
%% item =====> item
-record(item, {
    id = undefined,                                   %% ID 
    role_id = 0,                                      %% 角色id(select)(once) 
    item_id = 0,                                      %% 物品id(once) 
    type = 0,                                         %% 类型 
    amount = 0,                                       %% 数量 
    bind = 0,                                         %% 绑定 
    flag = 0                                          %% 标识(flag),default(0) 
}).

%% 物品配置表
%% item_data =====> item_data
-record(item_data, {
    item_id = 0,                                      %% 物品id 
    name = <<>>,                                      %% 名字 
    type = 0,                                         %% 类型 
    overlap = 1                                       %% 叠加数 
}).

