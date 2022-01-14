%%%-------------------------------------------------------------------
%%% @doc
%%% task
%%% @end
%%%-------------------------------------------------------------------
-module(task).
%% API
-export([load/1, save/1]).
-export([query/1]).
-export([accept/2, submit/2]).
-export([update/2]).
%% Includes
-include("common.hrl").
-include("protocol.hrl").
-include("event.hrl").
-include("user.hrl").
-include("dungeon.hrl").
-include("task.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc load
-spec load(User :: #user{}) -> NewUser :: #user{}.
load(User = #user{role_id = RoleId}) ->
    TaskList = task_sql:select_by_role_id(RoleId),
    lists:foldl(fun(Task = #task{task_id = TaskId}, AccUser = #user{task = AccTaskList}) -> {NewUser, NewTask} = check(AccUser, Task, task_data:get(TaskId)), NewUser#user{task = [NewTask | AccTaskList]} end, User, TaskList).

%% @doc save
-spec save(User :: #user{}) -> NewUser :: #user{}.
save(User = #user{task = Task}) ->
    NewTask = task_sql:insert_update(Task),
    User#user{task = NewTask}.

%% @doc query
-spec query(User :: #user{}) -> ok().
query(#user{task = Task}) ->
    {ok, Task}.

%% @doc accept
-spec accept(User :: #user{}, TaskId :: non_neg_integer()) -> ok() | error().
accept(User, TaskId) ->
    case task_data:get(TaskId) of
        TaskData = #task_data{} ->
            check_pre(User, TaskData);
        _ ->
            {error, configure_not_found}
    end.

check_pre(User = #user{task = Task}, TaskData = #task_data{type = Type, pre_id = PreTaskId}) ->
    case lists:keyfind(Type, #task.type, Task) of
        false when PreTaskId == 0 ->
            check_condition(User, TaskData);
        #task{number = 0, task_id = PreTaskId} ->
            check_condition(User, TaskData);
        #task{number = Number} when 0 < Number ->
            {error, task_pre_not_completed};
        #task{task_id = TaskId} when TaskId =/= PreTaskId ->
            {error, task_not_next};
        _ ->
            {error, task_not_found}
    end.

check_condition(User, TaskData = #task_data{condition = Condition}) ->
    case user_checker:check(User, Condition) of
        ok ->
            accept_cost(User, TaskData);
        _ ->
            {error, condition_not_met}
    end.

accept_cost(User, TaskData = #task_data{cost = Cost}) ->
    case item:cost(User, Cost, task) of
        {ok, NewUser} ->
            accept_update(NewUser, TaskData);
        _ ->
            {error, item_not_enough}
    end.

accept_update(User = #user{role_id = RoleId, task = TaskList}, TaskData = #task_data{task_id = TaskId, type = Type, number = Number}) ->
    Task = #task{role_id = RoleId, task_id = TaskId, type = Type, number = Number, flag = 1},
    %% check it completed when accept
    {NewUser, NewTask} = check(User, Task, TaskData),
    NewTaskList = lists:keystore(Type, #task.type, TaskList, NewTask),
    %% update task list
    user_sender:send(NewUser, ?PROTOCOL_TASK_QUERY, [NewTask]),
    {ok, ok, NewUser#user{task = NewTaskList}}.

%% update task when accept
check(User, Task = #task{number = Number}, TaskData = #task_data{event = Event, target = Target, compare = Compare}) ->
    %% check current target and number
    {CheckTarget, CheckNumber} = handle_check(User, TaskData),
    %% update target number
    NewNumber = update_number(Number, Target, Compare, CheckTarget, CheckNumber),
    case NewNumber of
        0 ->
            {User, Task#task{number = NewNumber}};
        _ ->
            {user_event:add_trigger(User, #trigger{name = Event, module = ?MODULE, function = update}), Task#task{number = NewNumber}}
    end.

%% task check module map @here

%% check role level
handle_check(User, #task_data{event = event_level_upgrade}) ->
    {role:level(User), 1};

%% check role guild id
handle_check(User, #task_data{event = event_guild_join}) ->
    {role:guild_id(User), 1};

%% check passed any dungeon
handle_check(User, #task_data{event = event_dungeon_passed}) ->
    {0, dungeon:get_number(User)};

%% check passed exp dungeon
handle_check(User, #task_data{event = event_dungeon_exp_passed}) ->
    {dungeon:get_current(User, ?DUNGEON_TYPE_EXP), 1};

%% check passed copper dungeon
handle_check(User, #task_data{event = event_dungeon_copper_passed}) ->
    {dungeon:get_current(User, ?DUNGEON_TYPE_COPPER), 1};

%% check add number of friend
handle_check(User, #task_data{event = event_friend_add}) ->
    {0, friend:get_number(User)};

%% check buy any number of shop
handle_check(User, #task_data{event = event_shop_buy, target = 0}) ->
    {0, shop:get_number(User)};

%% check buy this target number of shop
handle_check(User, #task_data{event = event_shop_buy, target = Target}) ->
    {Target, shop:get_number(User, Target)};

%% none of all
handle_check(_, _) ->
    {0, 0}.

%% @doc submit
-spec submit(User :: #user{}, TaskId :: non_neg_integer()) -> ok() | error().
submit(User, TaskId) ->
    case task_data:get(TaskId) of
        TaskData = #task_data{} ->
            award(User, TaskData);
        _ ->
            {error, configure_not_found}
    end.

award(User = #user{role_id = RoleId, task = TaskList}, #task_data{task_id = TaskId, type = Type, award = Award}) ->
    case lists:keyfind(Type, #task.type, TaskList) of
        Task = #task{number = 0, is_award = 0} ->
            {ok, AwardUser} = item:add(User, Award, ?MODULE),
            NewTask = Task#task{is_award = 1, flag = 1},
            NewTaskList = lists:keystore(Type, #task.type, TaskList, NewTask),
            %% log
            log:task_log(RoleId, TaskId, time:now()),
            {ok, ok, AwardUser#user{task = NewTaskList}};
        #task{is_award = 1} ->
            %% award received
            {error, task_already_submitted};
        #task{} ->
            %% number great then zero
            {error, task_not_completed};
        _ ->
            {error, task_not_found}
    end.



%% @doc update task when event happen
-spec update(User :: #user{}, Event :: tuple()) -> {ok | remove, NewUser :: #user{}}.
update(User = #user{task = Task}, Event) ->
    {NewUser, NewTask, UpdateTask, Result} = update_task_loop(User, Event, Task, [], [], remove),
    _ = UpdateTask =/= [] andalso user_sender:send(User, ?PROTOCOL_TASK_QUERY, UpdateTask) == ok,
    {Result, NewUser#user{task = NewTask}}.

%% update per task
update_task_loop(User, _, [], List, Update, Result) ->
    {User, List, Update, Result};
update_task_loop(User, Event, [Task = #task{task_id = TaskId} | T], List, Update, Result) ->
    case do_update_task(User, Task, task_data:get(TaskId), Event) of
        NewTask = #task{number = 0} ->
            %% task completed, remove event trigger(by default)
            update_task_loop(User, Event, T, [NewTask | List], [NewTask | Update], Result);
        NewTask = #task{} ->
            %% task not finish, keep it
            update_task_loop(User, Event, T, [NewTask | List], [NewTask | Update], ok);
        _ ->
            update_task_loop(User, Event, T, [Task | List], Update, Result)
    end.

%% update task detail @here if the default update handler not satisfy



do_update_task(_User, Task = #task{number = Number}, #task_data{event = Event, compare = Compare, target = Target}, #event{name = Event, target = EventTarget, number = EventNumber}) ->
    NewNumber = update_number(Number, Target, Compare, EventTarget, EventNumber),
    Task#task{number = NewNumber, flag = 1};

do_update_task(_User, _Task, _TaskData, _Event) ->
    error.

%% update number with compare mode

%% do not compare check target and current target
update_number(OldNumber, _, nc, _, NewNumber) ->
    max(OldNumber - NewNumber, 0);

%% check target equal then current target
update_number(OldNumber, Target, eq, Target, NewNumber) ->
    max(OldNumber - NewNumber, 0);

%% check target great then current target
update_number(OldNumber, Target, gt, ThisTarget, NewNumber) when Target < ThisTarget ->
    max(OldNumber - NewNumber, 0);

%% check target great then equal current target
update_number(OldNumber, Target, ge, ThisTarget, NewNumber) when Target =< ThisTarget ->
    max(OldNumber - NewNumber, 0);

%% none of all
update_number(OldNumber, _, _, _, _) ->
    OldNumber.

%%%===================================================================
%%% Internal functions
%%%===================================================================
