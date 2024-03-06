-module(asset_sql).
-export([insert/1]).
-export([select/1]).
-export([update/1]).
-include("asset.hrl").

%% @doc insert into asset
-spec insert(Asset :: #asset{}) -> InsertIdOrAffectedRows :: non_neg_integer().
insert(Asset) ->
    db:insert(<<"INSERT INTO `asset` (`role_id`, `gold`, `silver`, `copper`, `coin`, `exp`) VALUES (:1:, :2:, :3:, :4:, :5:, :6:)">>, Asset).

%% @doc select from asset
-spec select(RoleId :: non_neg_integer()) -> Rows :: [#asset{}].
select(RoleId) ->
    Data = db:select(<<"SELECT `role_id`, `gold`, `silver`, `copper`, `coin`, `exp` FROM `asset` WHERE `role_id` = ?">>, [RoleId]),
    parser:convert(Data, asset).

%% @doc update into asset
-spec update(Asset :: #asset{}) -> AffectedRows :: non_neg_integer().
update(Asset) ->
    db:update(<<"UPDATE `asset` SET `gold` = :2:, `silver` = :3:, `copper` = :4:, `coin` = :5:, `exp` = :6: WHERE `role_id` = :1:">>, Asset).
