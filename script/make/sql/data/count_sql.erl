-module(count_sql).
-export([save/1]).
-export([select/1]).
-include("count.hrl").

%% @doc insert into count
-spec save(CountList :: [#count{}] | ets:tab()) -> NewCountList :: [#count{}].
save(CountList) ->
    db:save_into(<<"INSERT INTO `count` (`role_id`, `type`, `today_number`, `week_number`, `total_number`, `time`) VALUES">>, <<"(:2:, :3:, :4:, :5:, :6:, :7:)">>, <<"ON DUPLICATE KEY UPDATE `role_id` = VALUES(`role_id`), `type` = VALUES(`type`), `today_number` = VALUES(`today_number`), `week_number` = VALUES(`week_number`), `total_number` = VALUES(`total_number`), `time` = VALUES(`time`)">>, CountList, #count.flag).

%% @doc select from count
-spec select(RoleId :: non_neg_integer()) -> Rows :: [#count{}].
select(RoleId) ->
    Data = db:select(<<"SELECT `role_id`, `type`, `today_number`, `week_number`, `total_number`, `time`, `flag` FROM `count` WHERE `role_id` = ?">>, [RoleId]),
    parser:convert(Data, count).
