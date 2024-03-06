-module(item_sql).
-export([save/1]).
-export([select/1]).
-export([delete_in_item_no/1]).
-include("item.hrl").

%% @doc insert into item
-spec save(ItemList :: [#item{}] | ets:tab()) -> NewItemList :: [#item{}].
save(ItemList) ->
    db:save_into(<<"INSERT INTO `item` (`item_no`, `role_id`, `item_id`, `type`, `number`, `expire_time`) VALUES">>, <<"(?, ?, ?, ?, ?, ?)">>, <<"ON DUPLICATE KEY UPDATE `item_no` = VALUES(`item_no`), `role_id` = VALUES(`role_id`), `item_id` = VALUES(`item_id`), `type` = VALUES(`type`), `number` = VALUES(`number`), `expire_time` = VALUES(`expire_time`)">>, ItemList, fun(#item{item_no = ItemNo, role_id = RoleId, item_id = ItemId, type = Type, number = Number, expire_time = ExpireTime}) -> [ItemNo, RoleId, ItemId, Type, Number, ExpireTime] end, #item.flag).

%% @doc select from item
-spec select(RoleId :: non_neg_integer()) -> Rows :: [#item{}].
select(RoleId) ->
    Data = db:select(<<"SELECT `item_no`, `role_id`, `item_id`, `type`, `number`, `expire_time`, `flag` FROM `item` WHERE `role_id` = ?">>, [RoleId]),
    parser:convert(Data, item).

%% @doc delete row from item
-spec delete_in_item_no(RoleId :: non_neg_integer()) -> AffectedRows :: non_neg_integer().
delete_in_item_no(RoleId) ->
    db:delete(<<"DELETE FROM `item` WHERE `role_id` IN (?)">>, [db:in(RoleId)]).
