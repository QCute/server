-module(achievement_protocol).
-export([decode/2, encode/2]).
-include("count.hrl").
-include("achievement.hrl").

-spec decode(Protocol :: non_neg_integer(), Binary :: binary()) -> {ok, [integer() | binary() | list()]} | {error, Protocol :: non_neg_integer(), Binary :: binary()}.
decode(12301, _Rest_ = <<_/binary>>) ->
    {ok, []};

decode(12202, _Rest_ = <<_/binary>>) ->
    {ok, []};

decode(12203, _Rest_ = <<_/binary>>) ->
    <<AchievementId:32, _AchievementIdRest_/binary>> = _Rest_,
    {ok, AchievementId};

decode(Protocol, Binary) ->
    {error, Protocol, Binary}.


-spec encode(Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()) -> {ok, binary()} | {error, Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()}.
encode(12301, List) ->
    Data12301 = <<(encode_list_12301(<<>>, 0, List))/binary>>,
    {ok, <<(byte_size(Data12301)):16, 12301:16, Data12301/binary>>};

encode(12202, List) ->
    Data12202 = <<(encode_list_12202(<<>>, 0, List))/binary>>,
    {ok, <<(byte_size(Data12202)):16, 12202:16, Data12202/binary>>};

encode(12203, Result) ->
    Data12203 = <<(protocol:text(Result))/binary>>,
    {ok, <<(byte_size(Data12203)):16, 12203:16, Data12203/binary>>};

encode(Protocol, Data) ->
    {error, Protocol, Data}.

encode_list_12301(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_list_12301(Acc = <<_/binary>>, Length, [#count{type = Type, total_number = TotalNumber} | List]) ->
    encode_list_12301(<<Acc/binary, Type:32, TotalNumber:32>>, Length + 1, List).

encode_list_12202(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_list_12202(Acc = <<_/binary>>, Length, [#achievement{achievement_id = AchievementId, type = Type} | List]) ->
    encode_list_12202(<<Acc/binary, AchievementId:32, Type:32>>, Length + 1, List).

