-module(task_sql).
-export([save/1]).
-export([select/1]).
-include("task.hrl").

%% @doc insert into task
-spec save(TaskList :: [#task{}] | ets:tab()) -> NewTaskList :: [#task{}].
save(TaskList) ->
    db:save_into(<<"INSERT INTO `task` (`role_id`, `task_id`, `type`, `number`, `is_award`) VALUES">>, <<"(:2:, :3:, :4:, :5:, :6:)">>, <<"ON DUPLICATE KEY UPDATE `role_id` = VALUES(`role_id`), `task_id` = VALUES(`task_id`), `type` = VALUES(`type`), `number` = VALUES(`number`), `is_award` = VALUES(`is_award`)">>, TaskList, #task.flag).

%% @doc select from task
-spec select(RoleId :: non_neg_integer()) -> Rows :: [#task{}].
select(RoleId) ->
    Data = db:select(<<"SELECT `role_id`, `task_id`, `type`, `number`, `is_award`, `flag` FROM `task` WHERE `role_id` = ?">>, [RoleId]),
    parser:convert(Data, task).
