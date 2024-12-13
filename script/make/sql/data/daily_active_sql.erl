-module(daily_active_sql).
-export([insert/1]).
-export([select/1]).
-export([update/1]).
-include("daily.hrl").

%% @doc insert into daily_active
-spec insert(DailyActive :: #daily_active{}) -> InsertIdOrAffectedRows :: non_neg_integer().
insert(DailyActive) ->
    db:insert(<<"INSERT INTO `daily_active` (`role_id`, `stage_id`, `score`) VALUES (:1:, :2:, :3:)">>, DailyActive).

%% @doc select from daily_active
-spec select(RoleId :: non_neg_integer()) -> Rows :: [#daily_active{}].
select(RoleId) ->
    Data = db:select(<<"SELECT `role_id`, `stage_id`, `score` FROM `daily_active` WHERE `role_id` = ?">>, [RoleId]),
    parser:convert(Data, daily_active).

%% @doc update into daily_active
-spec update(DailyActive :: #daily_active{}) -> AffectedRows :: non_neg_integer().
update(DailyActive) ->
    db:update(<<"UPDATE `daily_active` SET `stage_id` = :2:, `score` = :3: WHERE `role_id` = :1:">>, DailyActive).
