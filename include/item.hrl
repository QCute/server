%% 物品表
%% item =====> item
-record(item, {
    id = undefined,               %% id 
    user_id = 0,                  %% 玩家id(select)(once) 
    data_id = 0,                  %% 基础id(once) 
    amount = 0,                   %% 数量 
    extra = undefined             %% 额外(ignore)(flag)(null) 
}).

%% 物品配置表
%% data_item =====> data_item
-record(data_item, {
    data_id = 0,                  %% 基础id 
    name = <<>>,                  %% 名字 
    type = 0,                     %% 类型 
    bind = 0,                     %% 绑定 
    overlap = 1                   %% 叠加数 
}).

