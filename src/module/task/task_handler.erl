-module(task_handler).
-export([handle/3]).

handle(User, 11201, []) ->
    task:query(User);

handle(User, 11202, TaskId) ->
    task:accept(User, TaskId);

handle(User, 11203, TaskId) ->
    task:submit(User, TaskId);

handle(_, Protocol, Data) ->
    {error, Protocol, Data}.
