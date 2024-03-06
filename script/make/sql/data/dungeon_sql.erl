-module(dungeon_sql).
-export([save/1]).
-export([select/1]).
-include("dungeon.hrl").

%% @doc insert into dungeon
-spec save(DungeonList :: [#dungeon{}] | ets:tab()) -> NewDungeonList :: [#dungeon{}].
save(DungeonList) ->
    db:save_into(<<"INSERT INTO `dungeon` (`role_id`, `dungeon_id`, `type`, `today_number`, `total_number`, `is_pass`) VALUES">>, <<"(:2:, :3:, :4:, :5:, :6:, :7:)">>, <<"ON DUPLICATE KEY UPDATE `role_id` = VALUES(`role_id`), `dungeon_id` = VALUES(`dungeon_id`), `type` = VALUES(`type`), `today_number` = VALUES(`today_number`), `total_number` = VALUES(`total_number`), `is_pass` = VALUES(`is_pass`)">>, DungeonList, #dungeon.flag).

%% @doc select from dungeon
-spec select(RoleId :: non_neg_integer()) -> Rows :: [#dungeon{}].
select(RoleId) ->
    Data = db:select(<<"SELECT `role_id`, `dungeon_id`, `type`, `today_number`, `total_number`, `is_pass`, `flag` FROM `dungeon` WHERE `role_id` = ?">>, [RoleId]),
    parser:convert(Data, dungeon).
