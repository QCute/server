-module(daily_active_sql).
-export([insert/1]).
-export([select/1]).
-export([update/1]).
-include("daily.hrl").

%% @doc insert into daily_active
-spec insert(DailyActive :: #daily_active{}) -> InsertIdOrAffectedRows :: non_neg_integer().
insert(#daily_active{role_id = RoleId, stage_id = StageId, score = Score}) ->
    db:insert(<<"INSERT INTO `daily_active` (`role_id`, `stage_id`, `score`) VALUES (?, ?, ?)">>, [RoleId, StageId, Score]).

%% @doc select from daily_active
-spec select(RoleId :: non_neg_integer()) -> Rows :: [#daily_active{}].
select(RoleId) ->
    Data = db:select(<<"SELECT `role_id`, `stage_id`, `score` FROM `daily_active` WHERE `role_id` = ?">>, [RoleId]),
    parser:convert(Data, daily_active).

%% @doc update into daily_active
-spec update(#daily_active{}) -> AffectedRows :: non_neg_integer().
update(#daily_active{role_id = RoleId, stage_id = StageId, score = Score}) ->
    db:update(<<"UPDATE `daily_active` SET `stage_id` = ?, `score` = ? WHERE `role_id` = ?">>, [RoleId, StageId, Score, RoleId]).
