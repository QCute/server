-module(count_sql).
-export([save/1]).
-export([select/1]).
-include("count.hrl").

%% @doc insert into count
-spec save(CountList :: [#count{}] | ets:tab()) -> NewCountList :: [#count{}].
save(CountList) ->
    db:save(<<"INSERT INTO `count` (`role_id`, `type`, `today_number`, `week_number`, `total_number`, `time`) VALUES">>, <<"(?, ?, ?, ?, ?, ?)">>, <<"">>, CountList, fun(#count{role_id = RoleId, type = Type, today_number = TodayNumber, week_number = WeekNumber, total_number = TotalNumber, time = Time}) -> [RoleId, Type, TodayNumber, WeekNumber, TotalNumber, Time] end, #count.flag).

%% @doc select from count
-spec select(RoleId :: non_neg_integer()) -> Rows :: [#count{}].
select(RoleId) ->
    Data = db:select(<<"SELECT `role_id`, `type`, `today_number`, `week_number`, `total_number`, `time` FROM `count` WHERE `role_id` = ?">>, [RoleId]),
    parser:convert(Data, count).
