-module(role_notice_sql).
-export([insert/1]).
-export([select/2]).
-export([update/1]).
-export([delete/2]).
-export([insert_update/1]).
-export([select_join/2]).
-export([select_by_role_id/1]).
-export([select_join_by_role_id/1]).
-export([update_read/3]).
-include("notice.hrl").

-define(INSERT_ROLE_NOTICE, <<"INSERT INTO `role_notice` (`role_id`, `notice_id`, `read_time`) VALUES (~i~w, ~w, ~i~i~w~i~i~i)">>).
-define(SELECT_ROLE_NOTICE, <<"SELECT `role_id`, `notice_id`, 0 AS `receive_time`, 0 AS `expire_time`, `read_time`, '' AS `title`, '' AS `content`, 0 AS `flag` FROM `role_notice` WHERE `role_id` = ~w AND `notice_id` = ~w">>).
-define(UPDATE_ROLE_NOTICE, {<<"UPDATE `role_notice` SET ~i~i~i~i~i`read_time` = ~w~i~i~i ">>, <<"WHERE `role_id` = ~w AND `notice_id` = ~w">>}).
-define(DELETE_ROLE_NOTICE, <<"DELETE FROM `role_notice` WHERE `role_id` = ~w AND `notice_id` = ~w">>).
-define(INSERT_UPDATE_ROLE_NOTICE, {<<"INSERT INTO `role_notice` (`role_id`, `notice_id`, `read_time`) VALUES ">>, <<"(~i~w, ~w, ~i~i~w~i~i~i)">>, <<" ON DUPLICATE KEY UPDATE `read_time` = VALUES(`read_time`)">>}).
-define(SELECT_JOIN_ROLE_NOTICE, <<"SELECT `role_notice`.`role_id`, `role_notice`.`notice_id`, IFNULL(`notice`.`receive_time`, 0) AS `receive_time`, IFNULL(`notice`.`expire_time`, 0) AS `expire_time`, `role_notice`.`read_time`, IFNULL(`role_notice`.`title`, '') AS `title`, IFNULL(`role_notice`.`content`, '') AS `content`, IFNULL(`role_notice`.`flag`, 0) AS `flag` FROM `role_notice` LEFT JOIN `notice` ON `role_notice`.`notice_id` = `notice`.`notice_id` WHERE `role_notice`.`role_id` = ~w AND `role_notice`.`notice_id` = ~w">>).
-define(SELECT_BY_ROLE_ID, <<"SELECT `role_id`, `notice_id`, 0 AS `receive_time`, 0 AS `expire_time`, `read_time`, '' AS `title`, '' AS `content`, 0 AS `flag` FROM `role_notice` WHERE `role_id` = ~w">>).
-define(SELECT_JOIN_BY_ROLE_ID, <<"SELECT `role_notice`.`role_id`, `role_notice`.`notice_id`, IFNULL(`notice`.`receive_time`, 0) AS `receive_time`, IFNULL(`notice`.`expire_time`, 0) AS `expire_time`, `role_notice`.`read_time`, IFNULL(`role_notice`.`title`, '') AS `title`, IFNULL(`role_notice`.`content`, '') AS `content`, IFNULL(`role_notice`.`flag`, 0) AS `flag` FROM `role_notice` LEFT JOIN `notice` ON `role_notice`.`notice_id` = `notice`.`notice_id` WHERE `role_notice`.`role_id` = ~w">>).
-define(UPDATE_READ, <<"UPDATE `role_notice` SET `read_time` = ~w WHERE `role_id` = ~w AND `notice_id` = ~w">>).

%% @doc insert
-spec insert(RoleNotice :: #role_notice{}) -> InsertIdOrAffectedRows :: non_neg_integer().
insert(RoleNotice) ->
    Sql = parser:format(?INSERT_ROLE_NOTICE, RoleNotice),
    db:insert(Sql).

%% @doc select
-spec select(RoleId :: integer(), NoticeId :: integer()) -> RoleNoticeList :: [#role_notice{}].
select(RoleId, NoticeId) ->
    Sql = parser:format(?SELECT_ROLE_NOTICE, [RoleId, NoticeId]),
    Data = db:select(Sql),
    parser:convert(Data, role_notice).

%% @doc update
-spec update(RoleNotice :: #role_notice{}) -> AffectedRows :: non_neg_integer().
update(RoleNotice) ->
    Sql = <<(parser:format(element(1, ?UPDATE_ROLE_NOTICE), RoleNotice))/binary, (parser:format(element(2, ?UPDATE_ROLE_NOTICE), [RoleNotice#role_notice.role_id, RoleNotice#role_notice.notice_id]))/binary>>,
    db:update(Sql).

%% @doc delete
-spec delete(RoleId :: integer(), NoticeId :: integer()) -> AffectedRows :: non_neg_integer().
delete(RoleId, NoticeId) ->
    Sql = parser:format(?DELETE_ROLE_NOTICE, [RoleId, NoticeId]),
    db:delete(Sql).


%% @doc insert_update
-spec insert_update(RoleNoticeList :: [#role_notice{}] | ets:tab()) -> NewRoleNoticeList :: [#role_notice{}].
insert_update(RoleNoticeList) ->
    {Sql, NewRoleNoticeList} = parser:collect_into(RoleNoticeList, ?INSERT_UPDATE_ROLE_NOTICE, #role_notice.flag),
    db:insert(Sql),
    NewRoleNoticeList.

%% @doc select join
-spec select_join(RoleId :: integer(), NoticeId :: integer()) -> RoleNoticeList :: [#role_notice{}].
select_join(RoleId, NoticeId) ->
    Sql = parser:format(?SELECT_JOIN_ROLE_NOTICE, [RoleId, NoticeId]),
    Data = db:select(Sql),
    parser:convert(Data, role_notice).

%% @doc select
-spec select_by_role_id(RoleId :: integer()) -> RoleNoticeList :: [#role_notice{}].
select_by_role_id(RoleId) ->
    Sql = parser:format(?SELECT_BY_ROLE_ID, [RoleId]),
    Data = db:select(Sql),
    parser:convert(Data, role_notice).

%% @doc select join
-spec select_join_by_role_id(RoleId :: integer()) -> RoleNoticeList :: [#role_notice{}].
select_join_by_role_id(RoleId) ->
    Sql = parser:format(?SELECT_JOIN_BY_ROLE_ID, [RoleId]),
    Data = db:select(Sql),
    parser:convert(Data, role_notice).

%% @doc update
-spec update_read(UpdateReadTime :: integer(), RoleId :: integer(), NoticeId :: integer()) -> non_neg_integer().
update_read(UpdateReadTime, RoleId, NoticeId) ->
    Sql = parser:format(?UPDATE_READ, [UpdateReadTime, RoleId, NoticeId]),
    db:update(Sql).

