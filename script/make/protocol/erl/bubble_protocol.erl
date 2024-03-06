-module(bubble_protocol).
-export([decode/2, encode/2]).
-include("bubble.hrl").

-spec decode(Protocol :: non_neg_integer(), Binary :: binary()) -> {ok, [integer() | binary() | list()]} | {error, Protocol :: non_neg_integer(), Binary :: binary()}.
decode(12101, _Rest_ = <<_/binary>>) ->
    {ok, {}};


decode(Protocol, Binary) ->
    {error, Protocol, Binary}.


-spec encode(Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()) -> {ok, binary()} | {error, Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()}.
encode(12101, ) ->
    Data12101 = <<(encode__12101(<<>>, 0, ))/binary>>,
    {ok, <<(byte_size(Data12101)):16, 12101:16, Data12101/binary>>};

encode(12102, ) ->
    Data12102 = <<(encode__12102(<<>>, 0, ))/binary>>,
    {ok, <<(byte_size(Data12102)):16, 12102:16, Data12102/binary>>};

encode(Protocol, Data) ->
    {error, Protocol, Data}.

encode__12101(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode__12101(Acc = <<_/binary>>, Length, [#bubble{bubble_id = BubbleId, expire_time = ExpireTime} | ]) ->
    encode__12101(<<Acc/binary, BubbleId:32, ExpireTime:32>>, Length + 1, ).

encode__12102(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode__12102(Acc = <<_/binary>>, Length, [Item | ]) ->
    encode__12102(<<Acc/binary, Item:32>>, Length + 1, ).

