-module(task_protocol).
-export([decode/2, encode/2]).
-include("task.hrl").
-include("task.hrl").

-spec decode(Protocol :: non_neg_integer(), Binary :: binary()) -> {ok, [integer() | binary() | list()]} | {error, Protocol :: non_neg_integer(), Binary :: binary()}.
decode(11201, _Rest_ = <<_/binary>>) ->
    {ok, []};

decode(11202, _Rest_ = <<_/binary>>) ->
    <<TaskId:32, _TaskIdRest_/binary>> = _Rest_,
    {ok, TaskId};

decode(11203, _Rest_ = <<_/binary>>) ->
    <<TaskId:32, _TaskIdRest_/binary>> = _Rest_,
    {ok, TaskId};

decode(Protocol, Binary) ->
    {error, Protocol, Binary}.


-spec encode(Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()) -> {ok, binary()} | {error, Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()}.
encode(11201, List) ->
    Data11201 = <<(encode_list_11201(<<>>, 0, List))/binary>>,
    {ok, <<(byte_size(Data11201)):16, 11201:16, Data11201/binary>>};

encode(11202, [Result, #task{task_id = TaskId, number = Number, is_award = IsAward}]) ->
    Data11202 = <<(protocol:text(Result))/binary, TaskId:32, Number:16, IsAward:8>>,
    {ok, <<(byte_size(Data11202)):16, 11202:16, Data11202/binary>>};

encode(11203, Result) ->
    Data11203 = <<(protocol:text(Result))/binary>>,
    {ok, <<(byte_size(Data11203)):16, 11203:16, Data11203/binary>>};

encode(Protocol, Data) ->
    {error, Protocol, Data}.

encode_list_11201(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_list_11201(Acc = <<_/binary>>, Length, [#task{task_id = TaskId, number = Number, is_award = IsAward} | List]) ->
    encode_list_11201(<<Acc/binary, TaskId:32, Number:16, IsAward:8>>, Length + 1, List).

