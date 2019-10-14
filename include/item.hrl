%%%-------------------------------------------------------------------
%%% @doc
%%% item define
%%% @end
%%%-------------------------------------------------------------------

%% 背包/物品类型
-define(ITEM_TYPE_COMMON,                             1).   %% 道具
-define(ITEM_TYPE_BAG,                                2).   %% 装备
-define(ITEM_TYPE_BODY,                               3).   %% 身上
-define(ITEM_TYPE_STORE,                              4).   %% 仓库
-define(ITEM_TYPE_RUNE,                               5).   %% 符文
-define(ITEM_TYPE_TREASURE,                           6).   %% 寻宝
-define(ITEM_TYPE_BEAST,                              7).   %% 神兽
-define(ITEM_TYPE_SOUL,                               8).   %% 聚魂
-define(ITEM_TYPE_ASSET,                              10).  %% 资产

-define(ITEM_TYPE_LIST,                               [?ITEM_TYPE_COMMON, ?ITEM_TYPE_BAG, ?ITEM_TYPE_BODY, ?ITEM_TYPE_STORE]).  %% 物品类型列表

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
    asset = 0,                                        %% 资产类型
    overlap = 1,                                      %% 叠加数 
    name = <<>>,                                      %% 名字 
    icon = <<>>,                                      %% 图标 
    description = <<>>                                %% 描述 
}).

