%% 玩家商店表
%% shop =====> shop
-record(shop, {
    player_id = 0,                                    %% 玩家ID(select) 
    shop_id = 0,                                      %% 商店ID 
    amount = 0,                                       %% 数量 
    flag = <<>>                                       %% 标识(ignore)(flag) 
}).

%% 商店
%% data_shop =====> data_shop
-record(data_shop, {
    shop_id = 0,                                      %% 商店ID 
    item_id = 0,                                      %% 物品配置ID 
    type = 0,                                         %% 商店类型 
    pay_assets = <<>>,                                %% 货币类型 
    price = 0,                                        %% 价格 
    amount = 1,                                       %% 数量 
    bind = 0,                                         %% 是否绑定 
    level = 0,                                        %% 等级限制 
    limit = 0,                                        %% 购买上限 
    vip_level = 0,                                    %% vip等级限购 
    vip_limit = <<>>,                                 %% vip等级购买上限 
    description = <<>>                                %% 描述 
}).

