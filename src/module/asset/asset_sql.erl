-module(asset_sql).
-export([insert/1]).
-export([select/1]).
-export([update/1]).
-export([delete/1]).
-include("asset.hrl").

-define(INSERT_ASSET, <<"INSERT INTO `asset` (`role_id`, `gold`, `silver`, `copper`, `coin`, `exp`) VALUES (~i~w, ~w, ~w, ~w, ~w, ~w)">>).
-define(SELECT_ASSET, <<"SELECT `role_id`, `gold`, `silver`, `copper`, `coin`, `exp` FROM `asset` WHERE `role_id` = ~w">>).
-define(UPDATE_ASSET, {<<"UPDATE `asset` SET ~i~i`gold` = ~w, `silver` = ~w, `copper` = ~w, `coin` = ~w, `exp` = ~w ">>, <<"WHERE `role_id` = ~w">>}).
-define(DELETE_ASSET, <<"DELETE FROM `asset` WHERE `role_id` = ~w">>).

%% @doc insert
-spec insert(Asset :: #asset{}) -> InsertIdOrAffectedRows :: non_neg_integer().
insert(Asset) ->
    Sql = parser:format(?INSERT_ASSET, Asset),
    db:insert(Sql).

%% @doc select
-spec select(RoleId :: integer()) -> AssetList :: [#asset{}].
select(RoleId) ->
    Sql = parser:format(?SELECT_ASSET, [RoleId]),
    Data = db:select(Sql),
    parser:convert(Data, asset).

%% @doc update
-spec update(Asset :: #asset{}) -> AffectedRows :: non_neg_integer().
update(Asset) ->
    Sql = <<(parser:format(element(1, ?UPDATE_ASSET), Asset))/binary, (parser:format(element(2, ?UPDATE_ASSET), [Asset#asset.role_id]))/binary>>,
    db:update(Sql).

%% @doc delete
-spec delete(RoleId :: integer()) -> AffectedRows :: non_neg_integer().
delete(RoleId) ->
    Sql = parser:format(?DELETE_ASSET, [RoleId]),
    db:delete(Sql).

