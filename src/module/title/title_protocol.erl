-module(title_protocol).
-export([decode/2, encode/2]).
-include("title.hrl").


-spec decode(Protocol :: non_neg_integer(), Binary :: binary()) -> {ok, [integer() | binary() | list()]} | {error, Protocol :: non_neg_integer(), Binary :: binary()}.
decode(11901, _Rest_ = <<_/binary>>) ->
    {ok, []};

decode(Protocol, Binary) ->
    {error, Protocol, Binary}.


-spec encode(Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()) -> {ok, binary()} | {error, Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()}.
encode(11901, List) ->
    Data11901 = <<(encode_list_11901(<<>>, 0, List))/binary>>,
    {ok, <<(byte_size(Data11901)):16, 11901:16, Data11901/binary>>};

encode(11902, List) ->
    Data11902 = <<(encode_list_11902(<<>>, 0, List))/binary>>,
    {ok, <<(byte_size(Data11902)):16, 11902:16, Data11902/binary>>};

encode(Protocol, Data) ->
    {error, Protocol, Data}.

encode_list_11901(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_list_11901(Acc = <<_/binary>>, Length, [#title{title_id = TitleId, expire_time = ExpireTime} | List]) ->
    encode_list_11901(<<Acc/binary, TitleId:32, ExpireTime:32>>, Length + 1, List).

encode_list_11902(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_list_11902(Acc = <<_/binary>>, Length, [#title{title_id = TitleId} | List]) ->
    encode_list_11902(<<Acc/binary, TitleId:32>>, Length + 1, List).

