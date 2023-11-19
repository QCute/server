-ifndef(PACKAGE_HRL).
-define(PACKAGE_HRL, 'PACKAGE_HRL').

%% 角色物品包裹表
-record(package, {
    role_id = 0,                                      %% 角色ID
    item_size = 0,                                    %% 普通背包大小
    bag_size = 0,                                     %% 装备背包大小
    body_size = 0,                                    %% 身上背包大小
    store_size = 0                                    %% 仓库背包大小
}).

-endif.
