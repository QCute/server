-module(package_sql).
-export([insert/1]).
-export([select/1]).
-export([update/1]).
-include("package.hrl").

%% @doc insert into package
-spec insert(Package :: #package{}) -> InsertIdOrAffectedRows :: non_neg_integer().
insert(#package{role_id = RoleId, item_size = ItemSize, bag_size = BagSize, body_size = BodySize, store_size = StoreSize}) ->
    db:insert(<<"INSERT INTO `package` (`role_id`, `item_size`, `bag_size`, `body_size`, `store_size`) VALUES (?, ?, ?, ?, ?)">>, [RoleId, ItemSize, BagSize, BodySize, StoreSize]).

%% @doc select from package
-spec select(RoleId :: non_neg_integer()) -> Rows :: [#package{}].
select(RoleId) ->
    Data = db:select(<<"SELECT `role_id`, `item_size`, `bag_size`, `body_size`, `store_size` FROM `package` WHERE `role_id` = ?">>, [RoleId]),
    parser:convert(Data, package).

%% @doc update into package
-spec update(#package{}) -> AffectedRows :: non_neg_integer().
update(#package{item_size = ItemSize, bag_size = BagSize, body_size = BodySize, store_size = StoreSize, role_id = RoleId}) ->
    db:update(<<"UPDATE `package` SET `item_size` = ?, `bag_size` = ?, `body_size` = ?, `store_size` = ? WHERE `role_id` = ?">>, [ItemSize, BagSize, BodySize, StoreSize, RoleId]).
