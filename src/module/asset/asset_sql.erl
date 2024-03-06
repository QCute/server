-module(asset_sql).
-export([insert/1]).
-export([select/1]).
-export([update/1]).
-include("asset.hrl").

%% @doc insert into asset
-spec insert(Asset :: #asset{}) -> InsertIdOrAffectedRows :: non_neg_integer().
insert(#asset{role_id = RoleId, gold = Gold, silver = Silver, copper = Copper, coin = Coin, exp = Exp}) ->
    db:insert(<<"INSERT INTO `asset` (`role_id`, `gold`, `silver`, `copper`, `coin`, `exp`) VALUES (?, ?, ?, ?, ?, ?)">>, [RoleId, Gold, Silver, Copper, Coin, Exp]).

%% @doc select from asset
-spec select(RoleId :: non_neg_integer()) -> Rows :: [#asset{}].
select(RoleId) ->
    Data = db:select(<<"SELECT `role_id`, `gold`, `silver`, `copper`, `coin`, `exp` FROM `asset` WHERE `role_id` = ?">>, [RoleId]),
    parser:convert(Data, asset).

%% @doc update into asset
-spec update(#asset{}) -> AffectedRows :: non_neg_integer().
update(#asset{gold = Gold, silver = Silver, copper = Copper, coin = Coin, exp = Exp, role_id = RoleId}) ->
    db:update(<<"UPDATE `asset` SET `gold` = ?, `silver` = ?, `copper` = ?, `coin` = ?, `exp` = ? WHERE `role_id` = ?">>, [Gold, Silver, Copper, Coin, Exp, RoleId]).
