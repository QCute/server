-module(daily_sql).
-export([save/1]).
-export([select/1]).
-include("daily.hrl").

%% @doc insert into daily
-spec save(DailyList :: [#daily{}] | ets:tab()) -> NewDailyList :: [#daily{}].
save(DailyList) ->
    db:save_into(<<"INSERT INTO `daily` (`role_id`, `daily_id`, `is_award`) VALUES">>, <<"(:2:, :3:, :4:)">>, <<"ON DUPLICATE KEY UPDATE `role_id` = VALUES(`role_id`), `daily_id` = VALUES(`daily_id`), `is_award` = VALUES(`is_award`)">>, DailyList, #daily.flag).

%% @doc select from daily
-spec select(RoleId :: non_neg_integer()) -> Rows :: [#daily{}].
select(RoleId) ->
    Data = db:select(<<"SELECT `role_id`, `daily_id`, `is_award`, `flag` FROM `daily` WHERE `role_id` = ?">>, [RoleId]),
    parser:convert(Data, daily).
