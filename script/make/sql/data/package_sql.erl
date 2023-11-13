-module(package_sql).
-export([insert/1]).
-export([select/1]).
-export([update/1]).
-include("package.hrl").

%% @doc insert into package
-spec insert(Package :: #package{}) -> InsertIdOrAffectedRows :: non_neg_integer().
insert(Package) ->
    db:insert(<<"INSERT INTO `package` (`role_id`, `item_size`, `bag_size`, `body_size`, `store_size`) VALUES (:2:, :3:, :4:, :5:, :6:)">>, Package).

%% @doc select from package
-spec select(RoleId :: non_neg_integer()) -> Rows :: [#package{}].
select(RoleId) ->
    Data = db:select(<<"SELECT `role_id`, `item_size`, `bag_size`, `body_size`, `store_size` FROM `package` WHERE `role_id` = ?">>, [RoleId]),
    parser:convert(Data, package).

%% @doc update into package
-spec update(Package :: #package{}) -> AffectedRows :: non_neg_integer().
update(Package) ->
    db:update(<<"UPDATE `package` SET `item_size` = :3:, `bag_size` = :4:, `body_size` = :5:, `store_size` = :6: WHERE `role_id` = :1:">>, Package).
