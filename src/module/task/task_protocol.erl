-module(task_protocol).
-export([read/2, write/2]).
-include("task.hrl").


-spec read(Protocol :: non_neg_integer(), Binary :: binary()) -> {ok, [integer() | binary() | list()]} | {error, Protocol :: non_neg_integer(), Binary :: binary()}.
read(11201, <<>>) ->
    {ok, []};

read(11202, <<TaskId:32>>) ->
    {ok, TaskId};

read(11203, <<TaskId:32>>) ->
    {ok, TaskId};

read(Protocol, Binary) ->
    {error, Protocol, Binary}.


-spec write(Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()) -> {ok, binary()} | {error, Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()}.
write(11201, List) ->
    ListBinary = protocol:write_list(fun(#task{task_id = TaskId, number = Number, is_award = IsAward}) -> <<TaskId:32, Number:16, IsAward:8>> end, List),
    {ok, protocol:pack(11201, <<ListBinary/binary>>)};

write(11202, [Result, #task{task_id = TaskId, number = Number, is_award = IsAward}]) ->
    {ok, protocol:pack(11202, <<(protocol:text(Result))/binary, TaskId:32, Number:16, IsAward:8>>)};

write(11203, Result) ->
    {ok, protocol:pack(11203, <<(protocol:text(Result))/binary>>)};

write(Protocol, Data) ->
    {error, Protocol, Data}.

