%%%-------------------------------------------------------------------
%%% @doc
%%% item define
%%% @end
%%%-------------------------------------------------------------------

%% 背包/物品类型
-define(ITEM_TYPE_COMMON,                             1).   %% 道具
-define(ITEM_TYPE_EQUIPMENT,                          2).   %% 背包
-define(ITEM_TYPE_STORE,                              3).   %% 仓库

%% 特殊物品类型
-define(ITEM_TYPE_GOLD,                               101). %% 金币
-define(ITEM_TYPE_SLIVER,                             102). %% 银币
-define(ITEM_TYPE_COPPER,                             103). %% 铜币
-define(ITEM_TYPE_COIN,                               104). %% 硬币
-define(ITEM_TYPE_EXP,                                105). %% 经验

%% 角色物品表
%% item =====> item
-record(item, {
    unique_id = undefined,                            %% 唯一ID 
    role_id = 0,                                      %% 角色ID((select)/(once)) 
    item_id = 0,                                      %% 物品ID(once) 
    type = 0,                                         %% 类型 
    number = 1,                                       %% 数量
    bind = 0,                                         %% 绑定 
    flag = undefined                                  %% 标识(flag) 
}).

%% 物品配置表
%% item_data =====> item_data
-record(item_data, {
    item_id = 0,                                      %% 物品id 
    type = 0,                                         %% 类型 
    overlap = 1,                                      %% 叠加数 
    name = <<>>,                                      %% 名字 
    icon = <<>>,                                      %% 图标 
    description = <<>>                                %% 描述 
}).

