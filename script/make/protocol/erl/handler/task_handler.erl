-module(task_handler).
-export([handle/3]).
-export([send_query/2]).
-export([send_accept/3]).
-export([send_submit/2]).
-include("user.hrl").

handle(User, 11201, {}) ->
    task:query(User);

handle(User, 11202, Data) ->
    task:accept(User, Data);

handle(User, 11203, Data) ->
    task:submit(User, Data);

handle(_, Protocol, Data) ->
    {error, Protocol, Data}.

send_query(User, Data) ->
    {ok, Binary} = task_protocol:encode(11201, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_accept(User, Result, Task) ->
    {ok, Binary} = task_protocol:encode(11202, {Result, Task}),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_submit(User, Data) ->
    {ok, Binary} = task_protocol:encode(11203, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

