-module(map_protocol).
-export([decode/2, encode/2]).
-include("map.hrl").
-include("attribute.hrl").

-spec decode(Protocol :: non_neg_integer(), Binary :: binary()) -> {ok, [integer() | binary() | list()]} | {error, Protocol :: non_neg_integer(), Binary :: binary()}.
decode(20001, _Rest_ = <<_/binary>>) ->
    {ok, {}};


decode(20012, _Rest_ = <<_/binary>>) ->
    <<X:16, _XRest_/binary>> = _Rest_,
    <<Y:16, _YRest_/binary>> = _XRest_,
    {ok, {X, Y}};


decode(20014, _Rest_ = <<_/binary>>) ->
    <<SkillId:32, _SkillIdRest_/binary>> = _Rest_,
    <<TargetListLength:16, _TargetListLengthRest_/binary>> = _SkillIdRest_,
    {TargetListByteSize, TargetList} = decode_target_list_20014(_TargetListLengthRest_, 0, TargetListLength, []),
    <<_:TargetListByteSize/binary, _TargetListRest_/binary>> = _TargetListLengthRest_,
    {ok, {SkillId, TargetList}};

decode(Protocol, Binary) ->
    {error, Protocol, Binary}.

decode_target_list_20014(<<_/binary>>, Size, 0, List) ->
    {Size, List};
decode_target_list_20014(_Rest_ = <<_/binary>>, Size, Length, List) ->
    <<TargetListData:64, _TargetListDataRest_/binary>> = _Rest_,
    decode_target_list_20014(_TargetListDataRest_, Size + byte_size(_Rest_) - byte_size(_TargetListDataRest_), Length - 1, [TargetListData | List]).


-spec encode(Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()) -> {ok, binary()} | {error, Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()}.
encode(20001, #map{map_no = MapNo, map_id = MapId, fighter = Fighter}) ->
    Data20001 = <<MapNo:64, MapId:32, (encode_fighter_20001(<<>>, 0, Fighter))/binary>>,
    {ok, <<(byte_size(Data20001)):16, 20001:16, Data20001/binary>>};

encode(20011, Data) ->
    Data20011 = <<(encode_data_20011(<<>>, 0, Data))/binary>>,
    {ok, <<(byte_size(Data20011)):16, 20011:16, Data20011/binary>>};

encode(20012, #fighter{id = Id, x = X, y = Y}) ->
    Data20012 = <<Id:64, X:16, Y:16>>,
    {ok, <<(byte_size(Data20012)):16, 20012:16, Data20012/binary>>};

encode(20013, #fighter{id = Id}) ->
    Data20013 = <<Id:64>>,
    {ok, <<(byte_size(Data20013)):16, 20013:16, Data20013/binary>>};

encode(20014, {FighterId, PerformSkillId, FighterList}) ->
    Data20014 = <<FighterId:64, PerformSkillId:32, (encode_fighter_list_20014(<<>>, 0, FighterList))/binary>>,
    {ok, <<(byte_size(Data20014)):16, 20014:16, Data20014/binary>>};

encode(Protocol, Data) ->
    {error, Protocol, Data}.

encode_fighter_20001(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_fighter_20001(Acc = <<_/binary>>, Length, [#fighter{id = FighterId, type = FighterType, attribute = #attribute{fc = FighterAttributeFc, hp = FighterAttributeHp, health = FighterAttributeHealth}, skill = FighterSkill, buff = FighterBuff, x = FighterX, y = FighterY} | Fighter]) ->
    encode_fighter_20001(<<Acc/binary, FighterId:64, FighterType:8, FighterAttributeFc:64, FighterAttributeHp:64, FighterAttributeHealth:64, (encode_fighter_skill_20001(<<>>, 0, FighterSkill))/binary, (encode_fighter_buff_20001(<<>>, 0, FighterBuff))/binary, FighterX:16, FighterY:16>>, Length + 1, Fighter).

encode_fighter_skill_20001(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_fighter_skill_20001(Acc = <<_/binary>>, Length, [#battle_skill{skill_id = FighterSkillSkillId, time = FighterSkillTime, number = FighterSkillNumber} | FighterSkill]) ->
    encode_fighter_skill_20001(<<Acc/binary, FighterSkillSkillId:32, FighterSkillTime:32, FighterSkillNumber:32>>, Length + 1, FighterSkill).

encode_fighter_buff_20001(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_fighter_buff_20001(Acc = <<_/binary>>, Length, [#battle_buff{buff_id = FighterBuffBuffId, expire_time = FighterBuffExpireTime, overlap = FighterBuffOverlap} | FighterBuff]) ->
    encode_fighter_buff_20001(<<Acc/binary, FighterBuffBuffId:32, FighterBuffExpireTime:32, FighterBuffOverlap:32>>, Length + 1, FighterBuff).

encode_data_20011(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_data_20011(Acc = <<_/binary>>, Length, [#fighter{id = Id, type = Type, attribute = #attribute{fc = AttributeFc, hp = AttributeHp, health = AttributeHealth}, skill = Skill, buff = Buff, x = X, y = Y} | Data]) ->
    encode_data_20011(<<Acc/binary, Id:64, Type:8, AttributeFc:64, AttributeHp:64, AttributeHealth:64, (encode_skill_20011(<<>>, 0, Skill))/binary, (encode_buff_20011(<<>>, 0, Buff))/binary, X:16, Y:16>>, Length + 1, Data).

encode_skill_20011(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_skill_20011(Acc = <<_/binary>>, Length, [#battle_skill{skill_id = SkillSkillId, time = SkillTime, number = SkillNumber} | Skill]) ->
    encode_skill_20011(<<Acc/binary, SkillSkillId:32, SkillTime:32, SkillNumber:32>>, Length + 1, Skill).

encode_buff_20011(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_buff_20011(Acc = <<_/binary>>, Length, [#battle_buff{buff_id = BuffBuffId, expire_time = BuffExpireTime, overlap = BuffOverlap} | Buff]) ->
    encode_buff_20011(<<Acc/binary, BuffBuffId:32, BuffExpireTime:32, BuffOverlap:32>>, Length + 1, Buff).

encode_fighter_list_20014(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_fighter_list_20014(Acc = <<_/binary>>, Length, [#fighter{id = FighterListId, type = FighterListType, attribute = #attribute{fc = FighterListAttributeFc, hp = FighterListAttributeHp, health = FighterListAttributeHealth}, skill = FighterListSkill, buff = FighterListBuff, x = FighterListX, y = FighterListY} | FighterList]) ->
    encode_fighter_list_20014(<<Acc/binary, FighterListId:64, FighterListType:8, FighterListAttributeFc:64, FighterListAttributeHp:64, FighterListAttributeHealth:64, (encode_fighter_list_skill_20014(<<>>, 0, FighterListSkill))/binary, (encode_fighter_list_buff_20014(<<>>, 0, FighterListBuff))/binary, FighterListX:16, FighterListY:16>>, Length + 1, FighterList).

encode_fighter_list_skill_20014(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_fighter_list_skill_20014(Acc = <<_/binary>>, Length, [#battle_skill{skill_id = FighterListSkillSkillId, time = FighterListSkillTime, number = FighterListSkillNumber} | FighterListSkill]) ->
    encode_fighter_list_skill_20014(<<Acc/binary, FighterListSkillSkillId:32, FighterListSkillTime:32, FighterListSkillNumber:32>>, Length + 1, FighterListSkill).

encode_fighter_list_buff_20014(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_fighter_list_buff_20014(Acc = <<_/binary>>, Length, [#battle_buff{buff_id = FighterListBuffBuffId, expire_time = FighterListBuffExpireTime, overlap = FighterListBuffOverlap} | FighterListBuff]) ->
    encode_fighter_list_buff_20014(<<Acc/binary, FighterListBuffBuffId:32, FighterListBuffExpireTime:32, FighterListBuffOverlap:32>>, Length + 1, FighterListBuff).

