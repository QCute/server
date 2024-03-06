-module(map_protocol).
-export([decode/2, encode/2]).
-include("attribute.hrl").
-include("map.hrl").

-spec decode(Protocol :: non_neg_integer(), Binary :: binary()) -> {ok, [integer() | binary() | list()]} | {error, Protocol :: non_neg_integer(), Binary :: binary()}.
decode(20001, _Rest_ = <<_/binary>>) ->
    {ok, []};

decode(20011, _Rest_ = <<_/binary>>) ->
    {ok, []};

decode(20012, _Rest_ = <<_/binary>>) ->
    <<X:16, _XRest_/binary>> = _Rest_,
    <<Y:16, _YRest_/binary>> = _XRest_,
    {ok, [X, Y]};


decode(20014, _Rest_ = <<_/binary>>) ->
    <<SkillId:32, _SkillIdRest_/binary>> = _Rest_,
    <<TargetListLength:16, _TargetListLengthRest_/binary>> = _SkillIdRest_,
    {TargetListByteSize, TargetList} = decode_target_list_20014(_TargetListLengthRest_, 0, TargetListLength, []),
    <<_:TargetListByteSize/binary, _TargetListRest_/binary>> = _TargetListLengthRest_,
    {ok, [SkillId, TargetList]};

decode(Protocol, Binary) ->
    {error, Protocol, Binary}.

decode_target_list_20014(<<_/binary>>, Size, 0, List) ->
    {Size, List};
decode_target_list_20014(_Rest_ = <<_/binary>>, Size, TargetListLength, List) ->
    <<TargetId:64, _TargetIdRest_/binary>> = _Rest_,
    decode_target_list_20014(_TargetIdRest_, Size + byte_size(_Rest_) - byte_size(_TargetIdRest_), TargetListLength - 1, [TargetId | List]).


-spec encode(Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()) -> {ok, binary()} | {error, Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()}.
encode(20001, []) ->
    Data20001 = <<>>,
    {ok, <<(byte_size(Data20001)):16, 20001:16, Data20001/binary>>};

encode(20011, List) ->
    Data20011 = <<(encode_list_20011(<<>>, 0, List))/binary>>,
    {ok, <<(byte_size(Data20011)):16, 20011:16, Data20011/binary>>};

encode(20012, #fighter{id = Id, x = X, y = Y}) ->
    Data20012 = <<Id:64, X:16, Y:16>>,
    {ok, <<(byte_size(Data20012)):16, 20012:16, Data20012/binary>>};

encode(20013, #fighter{id = Id}) ->
    Data20013 = <<Id:64>>,
    {ok, <<(byte_size(Data20013)):16, 20013:16, Data20013/binary>>};

encode(20014, [FighterId, PerformSkillId, List]) ->
    Data20014 = <<FighterId:64, PerformSkillId:32, (encode_list_20014(<<>>, 0, List))/binary>>,
    {ok, <<(byte_size(Data20014)):16, 20014:16, Data20014/binary>>};

encode(Protocol, Data) ->
    {error, Protocol, Data}.

encode_list_20011(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_list_20011(Acc = <<_/binary>>, Length, [#fighter{id = Id, type = Type, attribute = #attribute{fc = Fc, hp = Hp, health = Health}, skill = Skill, buff = Buff, x = X, y = Y} | List]) ->
    encode_list_20011(<<Acc/binary, Id:64, Type:8, Fc:64, Hp:64, Health:64, (encode_skill_20011(<<>>, 0, Skill))/binary, (encode_buff_20011(<<>>, 0, Buff))/binary, X:16, Y:16>>, Length + 1, List).

encode_buff_20011(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_buff_20011(Acc = <<_/binary>>, Length, [#battle_buff{buff_id = BuffId, expire_time = ExpireTime, overlap = Overlap} | Buff]) ->
    encode_buff_20011(<<Acc/binary, BuffId:32, ExpireTime:32, Overlap:32>>, Length + 1, Buff).

encode_skill_20011(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_skill_20011(Acc = <<_/binary>>, Length, [#battle_skill{skill_id = SkillId, time = Time, number = Number} | Skill]) ->
    encode_skill_20011(<<Acc/binary, SkillId:32, Time:32, Number:32>>, Length + 1, Skill).

encode_list_20014(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_list_20014(Acc = <<_/binary>>, Length, [#fighter{id = Id, type = Type, attribute = #attribute{fc = Fc, hp = Hp, health = Health}, skill = Skill, buff = Buff, x = X, y = Y} | List]) ->
    encode_list_20014(<<Acc/binary, Id:64, Type:8, Fc:64, Hp:64, Health:64, (encode_skill_20014(<<>>, 0, Skill))/binary, (encode_buff_20014(<<>>, 0, Buff))/binary, X:16, Y:16>>, Length + 1, List).

encode_buff_20014(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_buff_20014(Acc = <<_/binary>>, Length, [#battle_buff{buff_id = BuffId, expire_time = ExpireTime, overlap = Overlap} | Buff]) ->
    encode_buff_20014(<<Acc/binary, BuffId:32, ExpireTime:32, Overlap:32>>, Length + 1, Buff).

encode_skill_20014(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_skill_20014(Acc = <<_/binary>>, Length, [#battle_skill{skill_id = SkillId, time = Time, number = Number} | Skill]) ->
    encode_skill_20014(<<Acc/binary, SkillId:32, Time:32, Number:32>>, Length + 1, Skill).

