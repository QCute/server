
%% 背包类型
-define(ITEM_TYPE_COMMON,                             1). %% 普通背包(道具背包)
-define(ITEM_TYPE_EQUIPMENT,                          2). %% 装备背包
-define(ITEM_TYPE_STORE,                              3). %% 仓库背包

%% 物品表
%% item =====> item
-record(item, {
    id = undefined,                                   %% id 
    user_id = 0,                                      %% 玩家id(select)(once) 
    data_id = 0,                                      %% 基础id(once) 
    type = 0,                                         %% 类型 
    amount = 0,                                       %% 数量 
    bind = 0,                                         %% 绑定 
    flag = <<>>                                       %% 标识(ignore)(flag) 
}).

%% 物品配置表
%% data_item =====> data_item
-record(data_item, {
    data_id = 0,                                      %% 基础id 
    name = <<>>,                                      %% 名字(string) 
    type = 0,                                         %% 类型 
    bind = 0,                                         %% 绑定 
    overlap = 1                                       %% 叠加数 
}).

