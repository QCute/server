-module(title_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("title.hrl").
-define(INSERT_TITLE, <<"INSERT INTO `title` (`role_id`, `title_id`, `type`, `expire_time`) VALUES (~w, ~w, ~w, ~w)">>).
-define(SELECT_TITLE, <<"SELECT `role_id`, `title_id`, `type`, `expire_time`, 0 AS `flag` FROM `title` WHERE `role_id` = ~w AND `title_id` = ~w">>).
-define(UPDATE_TITLE, <<"UPDATE `title` SET `type` = ~w, `expire_time` = ~w WHERE `role_id` = ~w AND `title_id` = ~w">>).
-define(DELETE_TITLE, <<"DELETE  FROM `title` WHERE `role_id` = ~w AND `title_id` = ~w">>).
-define(INSERT_UPDATE_TITLE, {<<"INSERT INTO `title` (`role_id`, `title_id`, `type`, `expire_time`) VALUES ">>, <<"(~w, ~w, ~w, ~w)">>, <<" ON DUPLICATE KEY UPDATE `type` = VALUES(`type`), `expire_time` = VALUES(`expire_time`)">>}).
-define(SELECT_BY_ROLE_ID, <<"SELECT `role_id`, `title_id`, `type`, `expire_time`, 0 AS `flag` FROM `title` WHERE `role_id` = ~w">>).
-define(SELECT_BY_TITLE_ID, <<"SELECT `role_id`, `title_id`, `type`, `expire_time`, 0 AS `flag` FROM `title` WHERE `title_id` = ~w">>).
-define(SELECT_JOIN_BY_ROLE_ID, <<"SELECT `title`.`role_id`, `title`.`title_id`, `title`.`type`, `title`.`expire_time`, IFNULL(`title`.`flag`, 0) AS `flag` FROM `title` WHERE `title`.`role_id` = ~w">>).
-define(SELECT_JOIN_BY_TITLE_ID, <<"SELECT `title`.`role_id`, `title`.`title_id`, `title`.`type`, `title`.`expire_time`, IFNULL(`title`.`flag`, 0) AS `flag` FROM `title` WHERE `title`.`title_id` = ~w">>).
-define(UPDATE_ROLE_ID, <<"UPDATE `title` SET `role_id` = ~w WHERE `role_id` = ~w AND `title_id` = ~w">>).

%% @doc insert
insert(Title) ->
    Sql = parser:format(?INSERT_TITLE, [
        Title#title.role_id,
        Title#title.title_id,
        Title#title.type,
        Title#title.expire_time
    ]),
    db:insert(Sql).

%% @doc select
select(RoleId, TitleId) ->
    Sql = parser:format(?SELECT_TITLE, [RoleId, TitleId]),
    Data = db:select(Sql),
    parser:convert(Data, title).

%% @doc update
update(Title) ->
    Sql = parser:format(?UPDATE_TITLE, [
        Title#title.type,
        Title#title.expire_time,
        Title#title.role_id,
        Title#title.title_id
    ]),
    db:update(Sql).

%% @doc delete
delete(RoleId, TitleId) ->
    Sql = parser:format(?DELETE_TITLE, [RoleId, TitleId]),
    db:delete(Sql).


%% @doc insert_update
insert_update(Data) ->
    F = fun(Title) -> [
        Title#title.role_id,
        Title#title.title_id,
        Title#title.type,
        Title#title.expire_time
    ] end,
    {Sql, NewData} = parser:collect_into(Data, F, ?INSERT_UPDATE_TITLE, #title.flag),
    db:insert(Sql),
    NewData.

%% @doc select
select_by_role_id(RoleId) ->
    Sql = parser:format(?SELECT_BY_ROLE_ID, [RoleId]),
    Data = db:select(Sql),
    parser:convert(Data, title).

%% @doc select
select_by_title_id(TitleId) ->
    Sql = parser:format(?SELECT_BY_TITLE_ID, [TitleId]),
    Data = db:select(Sql),
    parser:convert(Data, title).

%% @doc update
update_role_id(ThisRoleId, RoleId, TitleId) ->
    Sql = parser:format(?UPDATE_ROLE_ID, [ThisRoleId, RoleId, TitleId]),
    db:update(Sql).

