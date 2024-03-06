-module(friend_protocol).
-export([decode/2, encode/2]).
-include("friend.hrl").

-spec decode(Protocol :: non_neg_integer(), Binary :: binary()) -> {ok, [integer() | binary() | list()]} | {error, Protocol :: non_neg_integer(), Binary :: binary()}.
decode(11501, _Rest_ = <<_/binary>>) ->
    {ok, {}};

decode(11502, _Rest_ = <<_/binary>>) ->
    <<:64, _Rest_/binary>> = _Rest_,
    {ok, };

decode(11503, _Rest_ = <<_/binary>>) ->
    <<:64, _Rest_/binary>> = _Rest_,
    {ok, };

decode(11504, _Rest_ = <<_/binary>>) ->
    <<:64, _Rest_/binary>> = _Rest_,
    {ok, };

decode(11505, _Rest_ = <<_/binary>>) ->
    <<:64, _Rest_/binary>> = _Rest_,
    {ok, };

decode(11506, _Rest_ = <<_/binary>>) ->
    <<:64, _Rest_/binary>> = _Rest_,
    {ok, };

decode(Protocol, Binary) ->
    {error, Protocol, Binary}.


-spec encode(Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()) -> {ok, binary()} | {error, Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()}.
encode(11501, ) ->
    Data11501 = <<(encode__11501(<<>>, 0, ))/binary>>,
    {ok, <<(byte_size(Data11501)):16, 11501:16, Data11501/binary>>};

encode(11502, ) ->
    Data11502 = <<(protocol:text())/binary>>,
    {ok, <<(byte_size(Data11502)):16, 11502:16, Data11502/binary>>};

encode(11503, ) ->
    Data11503 = <<(protocol:text())/binary>>,
    {ok, <<(byte_size(Data11503)):16, 11503:16, Data11503/binary>>};

encode(11504, {Result, FriendRoleId}) ->
    Data11504 = <<(protocol:text(Result))/binary, FriendRoleId:64>>,
    {ok, <<(byte_size(Data11504)):16, 11504:16, Data11504/binary>>};

encode(11505, {Result, FriendRoleId}) ->
    Data11505 = <<(protocol:text(Result))/binary, FriendRoleId:64>>,
    {ok, <<(byte_size(Data11505)):16, 11505:16, Data11505/binary>>};

encode(11506, {Result, FriendRoleId}) ->
    Data11506 = <<(protocol:text(Result))/binary, FriendRoleId:64>>,
    {ok, <<(byte_size(Data11506)):16, 11506:16, Data11506/binary>>};

encode(Protocol, Data) ->
    {error, Protocol, Data}.

encode__11501(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode__11501(Acc = <<_/binary>>, Length, [#friend{friend_role_id = FriendRoleId, friend_name = FriendName, relation = Relation, time = Time} | ]) ->
    encode__11501(<<Acc/binary, FriendRoleId:64, (byte_size(FriendName)):16, (FriendName)/binary, Relation:8, Time:32>>, Length + 1, ).

