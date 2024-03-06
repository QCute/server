-module(daily_sql).
-export([save/1]).
-export([select/1]).
-include("daily.hrl").

%% @doc insert into daily
-spec save(DailyList :: [#daily{}] | ets:tab()) -> NewDailyList :: [#daily{}].
save(DailyList) ->
    db:save(<<"INSERT INTO `daily` (`role_id`, `daily_id`, `is_award`) VALUES">>, <<"(?, ?, ?)">>, <<"">>, DailyList, fun(#daily{role_id = RoleId, daily_id = DailyId, is_award = IsAward}) -> [RoleId, DailyId, IsAward] end, #daily.flag).

%% @doc select from daily
-spec select(RoleId :: non_neg_integer()) -> Rows :: [#daily{}].
select(RoleId) ->
    Data = db:select(<<"SELECT `role_id`, `daily_id`, `is_award` FROM `daily` WHERE `role_id` = ?">>, [RoleId]),
    parser:convert(Data, daily).
