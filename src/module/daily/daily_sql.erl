-module(daily_sql).
-export([insert/1]).
-export([select/2]).
-export([update/1]).
-export([delete/2]).
-export([insert_update/1]).
-export([select_by_role_id/1]).
-include("daily.hrl").

-define(INSERT_DAILY, <<"INSERT INTO `daily` (`role_id`, `daily_id`, `is_award`) VALUES (~i~w, ~w, ~w~i)">>).
-define(SELECT_DAILY, <<"SELECT `role_id`, `daily_id`, `is_award`, 0 AS `flag` FROM `daily` WHERE `role_id` = ~w AND `daily_id` = ~w">>).
-define(UPDATE_DAILY, {<<"UPDATE `daily` SET ~i~i~i`is_award` = ~w~i ">>, <<"WHERE `role_id` = ~w AND `daily_id` = ~w">>}).
-define(DELETE_DAILY, <<"DELETE FROM `daily` WHERE `role_id` = ~w AND `daily_id` = ~w">>).
-define(INSERT_UPDATE_DAILY, {<<"INSERT INTO `daily` (`role_id`, `daily_id`, `is_award`) VALUES ">>, <<"(~i~w, ~w, ~w~i)">>, <<" ON DUPLICATE KEY UPDATE `is_award` = VALUES(`is_award`)">>}).
-define(SELECT_BY_ROLE_ID, <<"SELECT `role_id`, `daily_id`, `is_award`, 0 AS `flag` FROM `daily` WHERE `role_id` = ~w">>).
-define(SELECT_JOIN_BY_ROLE_ID, <<"SELECT `daily`.`role_id`, `daily`.`daily_id`, `daily`.`is_award`, IFNULL(`daily`.`flag`, 0) AS `flag` FROM `daily` WHERE `daily`.`role_id` = ~w">>).

%% @doc insert
-spec insert(Daily :: #daily{}) -> InsertIdOrAffectedRows :: non_neg_integer().
insert(Daily) ->
    Sql = parser:format(?INSERT_DAILY, Daily),
    db:insert(Sql).

%% @doc select
-spec select(RoleId :: integer(), DailyId :: integer()) -> DailyList :: [#daily{}].
select(RoleId, DailyId) ->
    Sql = parser:format(?SELECT_DAILY, [RoleId, DailyId]),
    Data = db:select(Sql),
    parser:convert(Data, daily).

%% @doc update
-spec update(Daily :: #daily{}) -> AffectedRows :: non_neg_integer().
update(Daily) ->
    Sql = <<(parser:format(element(1, ?UPDATE_DAILY), Daily))/binary, (parser:format(element(2, ?UPDATE_DAILY), [Daily#daily.role_id, Daily#daily.daily_id]))/binary>>,
    db:update(Sql).

%% @doc delete
-spec delete(RoleId :: integer(), DailyId :: integer()) -> AffectedRows :: non_neg_integer().
delete(RoleId, DailyId) ->
    Sql = parser:format(?DELETE_DAILY, [RoleId, DailyId]),
    db:delete(Sql).


%% @doc insert_update
-spec insert_update(DailyList :: [#daily{}] | ets:tab()) -> NewDailyList :: [#daily{}].
insert_update(DailyList) ->
    {Sql, NewDailyList} = parser:collect_into(DailyList, ?INSERT_UPDATE_DAILY, #daily.flag),
    db:insert(Sql),
    NewDailyList.

%% @doc select
-spec select_by_role_id(RoleId :: integer()) -> DailyList :: [#daily{}].
select_by_role_id(RoleId) ->
    Sql = parser:format(?SELECT_BY_ROLE_ID, [RoleId]),
    Data = db:select(Sql),
    parser:convert(Data, daily).

