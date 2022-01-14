-module(fashion_sql).
-export([insert/1]).
-export([select/2]).
-export([update/1]).
-export([delete/2]).
-export([insert_update/1]).
-export([select_by_role_id/1]).
-export([select_by_fashion_id/1]).
-export([update_role_id/3]).
-include("fashion.hrl").

-define(INSERT_FASHION, <<"INSERT INTO `fashion` (`role_id`, `fashion_id`, `type`, `expire_time`) VALUES (~i~w, ~w, ~w, ~w~i)">>).
-define(SELECT_FASHION, <<"SELECT `role_id`, `fashion_id`, `type`, `expire_time`, 0 AS `flag` FROM `fashion` WHERE `role_id` = ~w AND `fashion_id` = ~w">>).
-define(UPDATE_FASHION, {<<"UPDATE `fashion` SET ~i~i~i`type` = ~w, `expire_time` = ~w~i ">>, <<"WHERE `role_id` = ~w AND `fashion_id` = ~w">>}).
-define(DELETE_FASHION, <<"DELETE FROM `fashion` WHERE `role_id` = ~w AND `fashion_id` = ~w">>).
-define(INSERT_UPDATE_FASHION, {<<"INSERT INTO `fashion` (`role_id`, `fashion_id`, `type`, `expire_time`) VALUES ">>, <<"(~i~w, ~w, ~w, ~w~i)">>, <<" ON DUPLICATE KEY UPDATE `type` = VALUES(`type`), `expire_time` = VALUES(`expire_time`)">>}).
-define(SELECT_BY_ROLE_ID, <<"SELECT `role_id`, `fashion_id`, `type`, `expire_time`, 0 AS `flag` FROM `fashion` WHERE `role_id` = ~w">>).
-define(SELECT_BY_FASHION_ID, <<"SELECT `role_id`, `fashion_id`, `type`, `expire_time`, 0 AS `flag` FROM `fashion` WHERE `fashion_id` = ~w">>).
-define(SELECT_JOIN_BY_ROLE_ID, <<"SELECT `fashion`.`role_id`, `fashion`.`fashion_id`, `fashion`.`type`, `fashion`.`expire_time`, IFNULL(`fashion`.`flag`, 0) AS `flag` FROM `fashion` WHERE `fashion`.`role_id` = ~w">>).
-define(SELECT_JOIN_BY_FASHION_ID, <<"SELECT `fashion`.`role_id`, `fashion`.`fashion_id`, `fashion`.`type`, `fashion`.`expire_time`, IFNULL(`fashion`.`flag`, 0) AS `flag` FROM `fashion` WHERE `fashion`.`fashion_id` = ~w">>).
-define(UPDATE_ROLE_ID, <<"UPDATE `fashion` SET `role_id` = ~w WHERE `role_id` = ~w AND `fashion_id` = ~w">>).

%% @doc insert
-spec insert(Fashion :: #fashion{}) -> InsertIdOrAffectedRows :: non_neg_integer().
insert(Fashion) ->
    Sql = parser:format(?INSERT_FASHION, Fashion),
    db:insert(Sql).

%% @doc select
-spec select(RoleId :: integer(), FashionId :: integer()) -> FashionList :: [#fashion{}].
select(RoleId, FashionId) ->
    Sql = parser:format(?SELECT_FASHION, [RoleId, FashionId]),
    Data = db:select(Sql),
    parser:convert(Data, fashion).

%% @doc update
-spec update(Fashion :: #fashion{}) -> AffectedRows :: non_neg_integer().
update(Fashion) ->
    Sql = <<(parser:format(element(1, ?UPDATE_FASHION), Fashion))/binary, (parser:format(element(2, ?UPDATE_FASHION), [Fashion#fashion.role_id, Fashion#fashion.fashion_id]))/binary>>,
    db:update(Sql).

%% @doc delete
-spec delete(RoleId :: integer(), FashionId :: integer()) -> AffectedRows :: non_neg_integer().
delete(RoleId, FashionId) ->
    Sql = parser:format(?DELETE_FASHION, [RoleId, FashionId]),
    db:delete(Sql).


%% @doc insert_update
-spec insert_update(FashionList :: [#fashion{}] | ets:tab()) -> NewFashionList :: [#fashion{}].
insert_update(FashionList) ->
    {Sql, NewFashionList} = parser:collect_into(FashionList, ?INSERT_UPDATE_FASHION, #fashion.flag),
    db:insert(Sql),
    NewFashionList.

%% @doc select
-spec select_by_role_id(RoleId :: integer()) -> FashionList :: [#fashion{}].
select_by_role_id(RoleId) ->
    Sql = parser:format(?SELECT_BY_ROLE_ID, [RoleId]),
    Data = db:select(Sql),
    parser:convert(Data, fashion).

%% @doc select
-spec select_by_fashion_id(FashionId :: integer()) -> FashionList :: [#fashion{}].
select_by_fashion_id(FashionId) ->
    Sql = parser:format(?SELECT_BY_FASHION_ID, [FashionId]),
    Data = db:select(Sql),
    parser:convert(Data, fashion).

%% @doc update
-spec update_role_id(UpdateRoleId :: integer(), RoleId :: integer(), FashionId :: integer()) -> non_neg_integer().
update_role_id(UpdateRoleId, RoleId, FashionId) ->
    Sql = parser:format(?UPDATE_ROLE_ID, [UpdateRoleId, RoleId, FashionId]),
    db:update(Sql).

