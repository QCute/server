-module(task_sql).
-export([insert/1]).
-export([select/2]).
-export([update/1]).
-export([delete/2]).
-export([insert_update/1]).
-export([select_by_role_id/1]).
-include("task.hrl").

-define(INSERT_TASK, <<"INSERT INTO `task` (`role_id`, `task_id`, `type`, `number`, `is_award`) VALUES (~i~w, ~w, ~w, ~w, ~w~i)">>).
-define(SELECT_TASK, <<"SELECT `role_id`, `task_id`, `type`, `number`, `is_award`, 0 AS `flag` FROM `task` WHERE `role_id` = ~w AND `type` = ~w">>).
-define(UPDATE_TASK, {<<"UPDATE `task` SET ~i~i`task_id` = ~w, ~i`number` = ~w, `is_award` = ~w~i ">>, <<"WHERE `role_id` = ~w AND `type` = ~w">>}).
-define(DELETE_TASK, <<"DELETE FROM `task` WHERE `role_id` = ~w AND `type` = ~w">>).
-define(INSERT_UPDATE_TASK, {<<"INSERT INTO `task` (`role_id`, `task_id`, `type`, `number`, `is_award`) VALUES ">>, <<"(~i~w, ~w, ~w, ~w, ~w~i)">>, <<" ON DUPLICATE KEY UPDATE `task_id` = VALUES(`task_id`), `number` = VALUES(`number`), `is_award` = VALUES(`is_award`)">>}).
-define(SELECT_BY_ROLE_ID, <<"SELECT `role_id`, `task_id`, `type`, `number`, `is_award`, 0 AS `flag` FROM `task` WHERE `role_id` = ~w">>).
-define(SELECT_JOIN_BY_ROLE_ID, <<"SELECT `task`.`role_id`, `task`.`task_id`, `task`.`type`, `task`.`number`, `task`.`is_award`, IFNULL(`task`.`flag`, 0) AS `flag` FROM `task` WHERE `task`.`role_id` = ~w">>).

%% @doc insert
-spec insert(Task :: #task{}) -> InsertIdOrAffectedRows :: non_neg_integer().
insert(Task) ->
    Sql = parser:format(?INSERT_TASK, Task),
    db:insert(Sql).

%% @doc select
-spec select(RoleId :: integer(), Type :: integer()) -> TaskList :: [#task{}].
select(RoleId, Type) ->
    Sql = parser:format(?SELECT_TASK, [RoleId, Type]),
    Data = db:select(Sql),
    parser:convert(Data, task).

%% @doc update
-spec update(Task :: #task{}) -> AffectedRows :: non_neg_integer().
update(Task) ->
    Sql = <<(parser:format(element(1, ?UPDATE_TASK), Task))/binary, (parser:format(element(2, ?UPDATE_TASK), [Task#task.role_id, Task#task.type]))/binary>>,
    db:update(Sql).

%% @doc delete
-spec delete(RoleId :: integer(), Type :: integer()) -> AffectedRows :: non_neg_integer().
delete(RoleId, Type) ->
    Sql = parser:format(?DELETE_TASK, [RoleId, Type]),
    db:delete(Sql).


%% @doc insert_update
-spec insert_update(TaskList :: [#task{}] | ets:tab()) -> NewTaskList :: [#task{}].
insert_update(TaskList) ->
    {Sql, NewTaskList} = parser:collect_into(TaskList, ?INSERT_UPDATE_TASK, #task.flag),
    db:insert(Sql),
    NewTaskList.

%% @doc select
-spec select_by_role_id(RoleId :: integer()) -> TaskList :: [#task{}].
select_by_role_id(RoleId) ->
    Sql = parser:format(?SELECT_BY_ROLE_ID, [RoleId]),
    Data = db:select(Sql),
    parser:convert(Data, task).

