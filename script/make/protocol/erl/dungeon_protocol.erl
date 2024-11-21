-module(dungeon_protocol).
-export([decode/2, encode/2]).
-include("dungeon.hrl").

-spec decode(Protocol :: non_neg_integer(), Binary :: binary()) -> {ok, [integer() | binary() | list()]} | {error, Protocol :: non_neg_integer(), Binary :: binary()}.
decode(17001, _Rest_ = <<_/binary>>) ->
    {ok, {}};

decode(17002, _Rest_ = <<_/binary>>) ->
    <<Data:32, _DataRest_/binary>> = _Rest_,
    {ok, Data};



decode(17005, _Rest_ = <<_/binary>>) ->
    {ok, {}};

decode(Protocol, Binary) ->
    {error, Protocol, Binary}.


-spec encode(Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()) -> {ok, binary()} | {error, Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()}.
encode(17001, Data) ->
    Data17001 = <<(encode_data_17001(<<>>, 0, Data))/binary>>,
    {ok, <<(byte_size(Data17001)):16, 17001:16, Data17001/binary>>};

encode(17002, Data) ->
    Data17002 = <<(protocol:text(Data))/binary>>,
    {ok, <<(byte_size(Data17002)):16, 17002:16, Data17002/binary>>};

encode(17003, Data) ->
    Data17003 = <<(protocol:text(Data))/binary>>,
    {ok, <<(byte_size(Data17003)):16, 17003:16, Data17003/binary>>};

encode(17004, Data) ->
    Data17004 = <<(protocol:text(Data))/binary>>,
    {ok, <<(byte_size(Data17004)):16, 17004:16, Data17004/binary>>};

encode(17005, Data) ->
    Data17005 = <<(protocol:text(Data))/binary>>,
    {ok, <<(byte_size(Data17005)):16, 17005:16, Data17005/binary>>};

encode(Protocol, Data) ->
    {error, Protocol, Data}.

encode_data_17001(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_data_17001(Acc = <<_/binary>>, Length, [#dungeon{dungeon_id = DungeonId, today_number = TodayNumber, total_number = TotalNumber} | Data]) ->
    encode_data_17001(<<Acc/binary, DungeonId:32, TodayNumber:16, TotalNumber:16>>, Length + 1, Data).

