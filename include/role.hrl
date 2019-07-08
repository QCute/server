%% 角色信息表
%% role =====> role
-record(role, {
    id = undefined,                                   %% ID 
    account_name = <<>>,                              %% 用户名(once) 
    name = <<>>,                                      %% 昵称(once)(update_name) 
    sex = 0,                                          %% 性别 
    level = 0,                                        %% 等级 
    classes = 0,                                      %% 职业 
    item_size = 0,                                    %% 普通背包大小 
    bag_size = 0,                                     %% 装备背包大小 
    store_size = 0,                                   %% 仓库背包大小 
    server_id = 0,                                    %% 服ID 
    online = 0,                                       %% 是否在线 
    extra = <<>>                                      %% 额外(ignore) 
}).

