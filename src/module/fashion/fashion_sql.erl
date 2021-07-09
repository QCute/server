-module(fashion_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("fashion.hrl").
-define(INSERT_FASHION, <<"INSERT INTO `fashion` (`role_id`, `fashion_id`, `type`, `expire_time`) VALUES (~i~w, ~w, ~w, ~w~i)">>).
-define(SELECT_FASHION, <<"SELECT `role_id`, `fashion_id`, `type`, `expire_time`, 0 AS `flag` FROM `fashion` WHERE `role_id` = ~w AND `fashion_id` = ~w">>).
-define(UPDATE_FASHION, {<<"UPDATE `fashion` SET ~i~i~i`type` = ~w, `expire_time` = ~w~i ">>, <<"WHERE `role_id` = ~w AND `fashion_id` = ~w">>}).
-define(DELETE_FASHION, <<"DELETE  FROM `fashion` WHERE `role_id` = ~w AND `fashion_id` = ~w">>).
-define(INSERT_UPDATE_FASHION, {<<"INSERT INTO `fashion` (`role_id`, `fashion_id`, `type`, `expire_time`) VALUES ">>, <<"(~i~w, ~w, ~w, ~w~i)">>, <<" ON DUPLICATE KEY UPDATE `type` = VALUES(`type`), `expire_time` = VALUES(`expire_time`)">>}).
-define(SELECT_BY_ROLE_ID, <<"SELECT `role_id`, `fashion_id`, `type`, `expire_time`, 0 AS `flag` FROM `fashion` WHERE `role_id` = ~w">>).
-define(SELECT_BY_FASHION_ID, <<"SELECT `role_id`, `fashion_id`, `type`, `expire_time`, 0 AS `flag` FROM `fashion` WHERE `fashion_id` = ~w">>).
-define(SELECT_JOIN_BY_ROLE_ID, <<"SELECT `fashion`.`role_id`, `fashion`.`fashion_id`, `fashion`.`type`, `fashion`.`expire_time`, IFNULL(`fashion`.`flag`, 0) AS `flag` FROM `fashion` WHERE `fashion`.`role_id` = ~w">>).
-define(SELECT_JOIN_BY_FASHION_ID, <<"SELECT `fashion`.`role_id`, `fashion`.`fashion_id`, `fashion`.`type`, `fashion`.`expire_time`, IFNULL(`fashion`.`flag`, 0) AS `flag` FROM `fashion` WHERE `fashion`.`fashion_id` = ~w">>).
-define(UPDATE_ROLE_ID, <<"UPDATE `fashion` SET `role_id` = ~w WHERE `role_id` = ~w AND `fashion_id` = ~w">>).

%% @doc insert
insert(Fashion) ->
    Sql = parser:format(?INSERT_FASHION, Fashion),
    db:insert(Sql).

%% @doc select
select(RoleId, FashionId) ->
    Sql = parser:format(?SELECT_FASHION, [RoleId, FashionId]),
    Data = db:select(Sql),
    parser:convert(Data, fashion).

%% @doc update
update(Fashion) ->
    Sql = <<(parser:format(element(1, ?UPDATE_FASHION), Fashion))/binary, (parser:format(element(2, ?UPDATE_FASHION), [Fashion#fashion.role_id, Fashion#fashion.fashion_id]))/binary>>,
    db:update(Sql).

%% @doc delete
delete(RoleId, FashionId) ->
    Sql = parser:format(?DELETE_FASHION, [RoleId, FashionId]),
    db:delete(Sql).


%% @doc insert_update
insert_update(Data) ->
    {Sql, NewData} = parser:collect_into(Data, ?INSERT_UPDATE_FASHION, #fashion.flag),
    db:insert(Sql),
    NewData.

%% @doc select
select_by_role_id(RoleId) ->
    Sql = parser:format(?SELECT_BY_ROLE_ID, [RoleId]),
    Data = db:select(Sql),
    parser:convert(Data, fashion).

%% @doc select
select_by_fashion_id(FashionId) ->
    Sql = parser:format(?SELECT_BY_FASHION_ID, [FashionId]),
    Data = db:select(Sql),
    parser:convert(Data, fashion).

%% @doc update
update_role_id(ThisRoleId, RoleId, FashionId) ->
    Sql = parser:format(?UPDATE_ROLE_ID, [ThisRoleId, RoleId, FashionId]),
    db:update(Sql).

