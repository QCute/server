%% 角色商店表
-record(shop, {
    role_id = 0,                                      %% 角色ID
    shop_id = 0,                                      %% 商店ID
    number = 0,                                       %% 数量
    flag = 0                                          %% 标识
}).

%% 商店配置表
-record(shop_data, {
    shop_id = 0,                                      %% 商店ID
    item_id = 0,                                      %% 物品配置ID
    type = 0,                                         %% 商店类型
    pay_asset = [],                                   %% 货币类型
    price = 0,                                        %% 价格
    number = 1,                                       %% 数量
    level = 0,                                        %% 等级限制
    limit = 0,                                        %% 购买上限
    vip_level = 0,                                    %% vip等级限购
    vip_limit = [],                                   %% vip等级购买上限
    description = <<>>                                %% 描述
}).

