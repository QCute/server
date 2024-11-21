-module(notice_protocol).
-export([decode/2, encode/2]).
-include("notice.hrl").

-spec decode(Protocol :: non_neg_integer(), Binary :: binary()) -> {ok, [integer() | binary() | list()]} | {error, Protocol :: non_neg_integer(), Binary :: binary()}.
decode(50001, _Rest_ = <<_/binary>>) ->
    {ok, {}};


decode(Protocol, Binary) ->
    {error, Protocol, Binary}.


-spec encode(Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()) -> {ok, binary()} | {error, Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()}.
encode(50001, Data) ->
    Data50001 = <<(encode_data_50001(<<>>, 0, Data))/binary>>,
    {ok, <<(byte_size(Data50001)):16, 50001:16, Data50001/binary>>};

encode(50002, {Scope, Type, Title, Msg}) ->
    Data50002 = <<Scope:8, Type:8, (byte_size(Title)):16, (Title)/binary, (byte_size(Msg)):16, (Msg)/binary>>,
    {ok, <<(byte_size(Data50002)):16, 50002:16, Data50002/binary>>};

encode(Protocol, Data) ->
    {error, Protocol, Data}.

encode_data_50001(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_data_50001(Acc = <<_/binary>>, Length, [#notice_role{notice_id = NoticeId, receive_time = ReceiveTime, read_time = ReadTime, title = Title, content = Content} | Data]) ->
    encode_data_50001(<<Acc/binary, NoticeId:64, ReceiveTime:32, ReadTime:32, (byte_size(Title)):16, (Title)/binary, (byte_size(Content)):16, (Content)/binary>>, Length + 1, Data).

