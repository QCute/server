-module(asset_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("asset.hrl").
-define(INSERT_ASSET, <<"INSERT INTO `asset` (`role_id`, `gold`, `silver`, `copper`, `coin`, `exp`) VALUES (~i~w, ~w, ~w, ~w, ~w, ~w)">>).
-define(SELECT_ASSET, <<"SELECT `role_id`, `gold`, `silver`, `copper`, `coin`, `exp` FROM `asset` WHERE `role_id` = ~w">>).
-define(UPDATE_ASSET, {<<"UPDATE `asset` SET ~i~i`gold` = ~w, `silver` = ~w, `copper` = ~w, `coin` = ~w, `exp` = ~w ">>, <<"WHERE `role_id` = ~w">>}).
-define(DELETE_ASSET, <<"DELETE  FROM `asset` WHERE `role_id` = ~w">>).

%% @doc insert
insert(Asset) ->
    Sql = parser:format(?INSERT_ASSET, Asset),
    db:insert(Sql).

%% @doc select
select(RoleId) ->
    Sql = parser:format(?SELECT_ASSET, [RoleId]),
    Data = db:select(Sql),
    parser:convert(Data, asset).

%% @doc update
update(Asset) ->
    Sql = <<(parser:format(element(1, ?UPDATE_ASSET), Asset))/binary, (parser:format(element(2, ?UPDATE_ASSET), [Asset#asset.role_id]))/binary>>,
    db:update(Sql).

%% @doc delete
delete(RoleId) ->
    Sql = parser:format(?DELETE_ASSET, [RoleId]),
    db:delete(Sql).

