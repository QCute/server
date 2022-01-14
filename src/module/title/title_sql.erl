-module(title_sql).
-export([insert/1]).
-export([select/2]).
-export([update/1]).
-export([delete/2]).
-export([insert_update/1]).
-export([select_by_role_id/1]).
-export([select_by_title_id/1]).
-export([update_role_id/3]).
-include("title.hrl").

-define(INSERT_TITLE, <<"INSERT INTO `title` (`role_id`, `title_id`, `type`, `expire_time`) VALUES (~i~w, ~w, ~w, ~w~i)">>).
-define(SELECT_TITLE, <<"SELECT `role_id`, `title_id`, `type`, `expire_time`, 0 AS `flag` FROM `title` WHERE `role_id` = ~w AND `title_id` = ~w">>).
-define(UPDATE_TITLE, {<<"UPDATE `title` SET ~i~i~i`type` = ~w, `expire_time` = ~w~i ">>, <<"WHERE `role_id` = ~w AND `title_id` = ~w">>}).
-define(DELETE_TITLE, <<"DELETE FROM `title` WHERE `role_id` = ~w AND `title_id` = ~w">>).
-define(INSERT_UPDATE_TITLE, {<<"INSERT INTO `title` (`role_id`, `title_id`, `type`, `expire_time`) VALUES ">>, <<"(~i~w, ~w, ~w, ~w~i)">>, <<" ON DUPLICATE KEY UPDATE `type` = VALUES(`type`), `expire_time` = VALUES(`expire_time`)">>}).
-define(SELECT_BY_ROLE_ID, <<"SELECT `role_id`, `title_id`, `type`, `expire_time`, 0 AS `flag` FROM `title` WHERE `role_id` = ~w">>).
-define(SELECT_BY_TITLE_ID, <<"SELECT `role_id`, `title_id`, `type`, `expire_time`, 0 AS `flag` FROM `title` WHERE `title_id` = ~w">>).
-define(SELECT_JOIN_BY_ROLE_ID, <<"SELECT `title`.`role_id`, `title`.`title_id`, `title`.`type`, `title`.`expire_time`, IFNULL(`title`.`flag`, 0) AS `flag` FROM `title` WHERE `title`.`role_id` = ~w">>).
-define(SELECT_JOIN_BY_TITLE_ID, <<"SELECT `title`.`role_id`, `title`.`title_id`, `title`.`type`, `title`.`expire_time`, IFNULL(`title`.`flag`, 0) AS `flag` FROM `title` WHERE `title`.`title_id` = ~w">>).
-define(UPDATE_ROLE_ID, <<"UPDATE `title` SET `role_id` = ~w WHERE `role_id` = ~w AND `title_id` = ~w">>).

%% @doc insert
-spec insert(Title :: #title{}) -> InsertIdOrAffectedRows :: non_neg_integer().
insert(Title) ->
    Sql = parser:format(?INSERT_TITLE, Title),
    db:insert(Sql).

%% @doc select
-spec select(RoleId :: integer(), TitleId :: integer()) -> TitleList :: [#title{}].
select(RoleId, TitleId) ->
    Sql = parser:format(?SELECT_TITLE, [RoleId, TitleId]),
    Data = db:select(Sql),
    parser:convert(Data, title).

%% @doc update
-spec update(Title :: #title{}) -> AffectedRows :: non_neg_integer().
update(Title) ->
    Sql = <<(parser:format(element(1, ?UPDATE_TITLE), Title))/binary, (parser:format(element(2, ?UPDATE_TITLE), [Title#title.role_id, Title#title.title_id]))/binary>>,
    db:update(Sql).

%% @doc delete
-spec delete(RoleId :: integer(), TitleId :: integer()) -> AffectedRows :: non_neg_integer().
delete(RoleId, TitleId) ->
    Sql = parser:format(?DELETE_TITLE, [RoleId, TitleId]),
    db:delete(Sql).


%% @doc insert_update
-spec insert_update(TitleList :: [#title{}] | ets:tab()) -> NewTitleList :: [#title{}].
insert_update(TitleList) ->
    {Sql, NewTitleList} = parser:collect_into(TitleList, ?INSERT_UPDATE_TITLE, #title.flag),
    db:insert(Sql),
    NewTitleList.

%% @doc select
-spec select_by_role_id(RoleId :: integer()) -> TitleList :: [#title{}].
select_by_role_id(RoleId) ->
    Sql = parser:format(?SELECT_BY_ROLE_ID, [RoleId]),
    Data = db:select(Sql),
    parser:convert(Data, title).

%% @doc select
-spec select_by_title_id(TitleId :: integer()) -> TitleList :: [#title{}].
select_by_title_id(TitleId) ->
    Sql = parser:format(?SELECT_BY_TITLE_ID, [TitleId]),
    Data = db:select(Sql),
    parser:convert(Data, title).

%% @doc update
-spec update_role_id(UpdateRoleId :: integer(), RoleId :: integer(), TitleId :: integer()) -> non_neg_integer().
update_role_id(UpdateRoleId, RoleId, TitleId) ->
    Sql = parser:format(?UPDATE_ROLE_ID, [UpdateRoleId, RoleId, TitleId]),
    db:update(Sql).

