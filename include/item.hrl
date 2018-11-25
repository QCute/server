%% 物品
%% item ==> item
-record(item, {
    id = undefined,               %% id 
    user_id = 0,                  %% 玩家id(select)(once) 
    base_id = 0,                  %% 基础id(once) 
    amount = 0,                   %% 数量 
    extra = 0                     %% 额外(ignore) 
}).

%% 物品配置表
%% base_item ==> base_item
-record(base_item, {
    base_id = 0,                  %% 基础id 
    name = [],                    %% 名字 
    type = 0,                     %% 类型 
    bind = 0,                     %% 绑定 
    overlap = 1                   %% 叠加数 
}).
