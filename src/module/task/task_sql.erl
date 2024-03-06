-module(task_sql).
-export([save/1]).
-export([select/1]).
-include("task.hrl").

%% @doc insert into task
-spec save(TaskList :: [#task{}] | ets:tab()) -> NewTaskList :: [#task{}].
save(TaskList) ->
    db:save(<<"INSERT INTO `task` (`role_id`, `task_id`, `type`, `number`, `is_award`) VALUES">>, <<"(?, ?, ?, ?, ?)">>, <<"">>, TaskList, fun(#task{role_id = RoleId, task_id = TaskId, type = Type, number = Number, is_award = IsAward}) -> [RoleId, TaskId, Type, Number, IsAward] end, #task.flag).

%% @doc select from task
-spec select(RoleId :: non_neg_integer()) -> Rows :: [#task{}].
select(RoleId) ->
    Data = db:select(<<"SELECT `role_id`, `task_id`, `type`, `number`, `is_award` FROM `task` WHERE `role_id` = ?">>, [RoleId]),
    parser:convert(Data, task).
