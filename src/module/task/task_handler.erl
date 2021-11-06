-module(task_handler).
-export([handle/3]).

handle(11201, User, []) ->
    task:query(User);

handle(11202, User, TaskId) ->
    task:accept(User, TaskId);

handle(11203, User, TaskId) ->
    task:submit(User, TaskId);

handle(Protocol, _, Data) ->
    {error, Protocol, Data}.
