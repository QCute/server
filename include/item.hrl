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
-define(ITEM_TYPE_TOTEM,                              9).   %% 图腾
-define(ITEM_TYPE_ASSET,                              10).  %% 资产

%% 物品类型列表
-define(ITEM_TYPE_LIST,                               [?ITEM_TYPE_COMMON, ?ITEM_TYPE_BAG, ?ITEM_TYPE_BODY, ?ITEM_TYPE_STORE, ?ITEM_TYPE_RUNE, ?ITEM_TYPE_TREASURE, ?ITEM_TYPE_BEAST, ?ITEM_TYPE_SOUL, ?ITEM_TYPE_TOTEM]).

%% 角色物品表
%% item =====> item
-record(item, {
    item_no = 0,                                      %% 物品编号
    role_id = 0,                                      %% 角色ID(select_by_role_id)
    item_id = 0,                                      %% 物品ID
    type = 0,                                         %% 类型
    number = 1,                                       %% 数量
    expire_time = 0,                                  %% 过期时间
    flag = 0                                          %% 标识(flag)
}).

%% 物品配置表
%% item_data =====> item_data
-record(item_data, {
    item_id = 0,                                      %% 物品id
    type = 0,                                         %% 类型(validate(item_type))
    overlap = 1,                                      %% 叠加数
    category = 0,                                     %% 分类ID
    time = 0,                                         %% 有效时间
    use_number = 0,                                   %% 使用数量(0:不能直接使用/1:一个/N:N个)
    use_effect = [],                                  %% 使用效果(validate(use_effect))
    use_value = 0,                                    %% 使用效果数值
    name = <<>>,                                      %% 名字
    icon = <<>>,                                      %% 图标
    description = <<>>                                %% 描述
}).

