-module(test_protocol).
-export([decode/2, encode/2]).
-include("item.hrl").

-spec decode(Protocol :: non_neg_integer(), Binary :: binary()) -> {ok, [integer() | binary() | list()]} | {error, Protocol :: non_neg_integer(), Binary :: binary()}.
decode(65531, _Rest_ = <<_/binary>>) ->
    <<Data:16/signed, _DataRest_/binary>> = _Rest_,
    {ok, Data};

decode(65532, _Rest_ = <<_/binary>>) ->
    <<DataLength:16, _DataLengthRest_/binary>> = _Rest_,
    {DataByteSize, Data} = decode_data_65532(_DataLengthRest_, 0, DataLength, []),
    <<_:DataByteSize/binary, _DataRest_/binary>> = _DataLengthRest_,
    {ok, Data};

decode(65533, _Rest_ = <<_/binary>>) ->
    <<DataLength:16, _DataLengthRest_/binary>> = _Rest_,
    {DataByteSize, Data} = decode_data_65533(_DataLengthRest_, 0, DataLength, []),
    <<_:DataByteSize/binary, _DataRest_/binary>> = _DataLengthRest_,
    {ok, Data};

decode(65534, _Rest_ = <<_/binary>>) ->
    <<Id:32, _IdRest_/binary>> = _Rest_,
    <<TheItemDataItemId:32, _TheItemDataItemIdRest_/binary>> = _IdRest_,
    <<TheItemDataType:16, _TheItemDataTypeRest_/binary>> = _TheItemDataItemIdRest_,
    <<NameLsLength:16, _NameLsLengthRest_/binary>> = _TheItemDataTypeRest_,
    {NameLsByteSize, NameLs} = decode_name_ls_65534(_NameLsLengthRest_, 0, NameLsLength, []),
    <<_:NameLsByteSize/binary, _NameLsRest_/binary>> = _NameLsLengthRest_,
    <<NameLssLength:16, _NameLssLengthRest_/binary>> = _NameLsRest_,
    {NameLssByteSize, NameLss} = decode_name_lss_65534(_NameLssLengthRest_, 0, NameLssLength, []),
    <<_:NameLssByteSize/binary, _NameLssRest_/binary>> = _NameLssLengthRest_,
    {ok, {Id, #item_data{item_id = TheItemDataItemId, type = TheItemDataType}, {NameLs, NameLss}}};

decode(65535, _Rest_ = <<_/binary>>) ->
    <<Binary:6/binary, _BinaryRest_/binary>> = _Rest_,
    <<BooleanFlag:8, _BooleanRest_/binary>> = _BinaryRest_,
    Boolean = type:to_boolean(BooleanFlag),
    <<U8:8, _U8Rest_/binary>> = _BooleanRest_,
    <<U16:16, _U16Rest_/binary>> = _U8Rest_,
    <<U32:32, _U32Rest_/binary>> = _U16Rest_,
    <<U64:64, _U64Rest_/binary>> = _U32Rest_,
    <<I8:8/signed, _I8Rest_/binary>> = _U64Rest_,
    <<I16:16/signed, _I16Rest_/binary>> = _I8Rest_,
    <<I32:32/signed, _I32Rest_/binary>> = _I16Rest_,
    <<I64:64/signed, _I64Rest_/binary>> = _I32Rest_,
    <<F32:32/float, _F32Rest_/binary>> = _I64Rest_,
    <<F64:64/float, _F64Rest_/binary>> = _F32Rest_,
    <<StrBinaryLength:16, StrBinary:StrBinaryLength/binary, _StrRest_/binary>> = _F64Rest_,
    Str = unicode:characters_to_list(StrBinary),
    <<BstLength:16, Bst:BstLength/binary, _BstRest_/binary>> = _StrRest_,
    <<TupleBinary:6/binary, _TupleBinaryRest_/binary>> = _BstRest_,
    <<TupleSubU8:8, _TupleSubU8Rest_/binary>> = _TupleBinaryRest_,
    <<TupleSubStrBinaryLength:16, TupleSubStrBinary:TupleSubStrBinaryLength/binary, _TupleSubStrRest_/binary>> = _TupleSubU8Rest_,
    TupleSubStr = unicode:characters_to_list(TupleSubStrBinary),
    <<TupleListLength:16, _TupleListLengthRest_/binary>> = _TupleSubStrRest_,
    {TupleListByteSize, TupleList} = decode_tuple_list_65535(_TupleListLengthRest_, 0, TupleListLength, []),
    <<_:TupleListByteSize/binary, _TupleListRest_/binary>> = _TupleListLengthRest_,
    <<TupleSingleLength:16, _TupleSingleLengthRest_/binary>> = _TupleListRest_,
    {TupleSingleByteSize, TupleSingle} = decode_tuple_single_65535(_TupleSingleLengthRest_, 0, TupleSingleLength, []),
    <<_:TupleSingleByteSize/binary, _TupleSingleRest_/binary>> = _TupleSingleLengthRest_,
    <<IndexListLength:16, _IndexListLengthRest_/binary>> = _TupleSingleRest_,
    {IndexListByteSize, IndexList} = decode_index_list_65535(_IndexListLengthRest_, 0, IndexListLength, []),
    <<_:IndexListByteSize/binary, _IndexListRest_/binary>> = _IndexListLengthRest_,
    <<KeyListLength:16, _KeyListLengthRest_/binary>> = _IndexListRest_,
    {KeyListByteSize, KeyList} = decode_key_list_65535(_KeyListLengthRest_, 0, KeyListLength, []),
    <<_:KeyListByteSize/binary, _KeyListRest_/binary>> = _KeyListLengthRest_,
    {ok, {Binary, Boolean, U8, U16, U32, U64, I8, I16, I32, I64, F32, F64, Str, Bst, {TupleBinary, {TupleSubU8, TupleSubStr}, TupleList, TupleSingle}, IndexList, KeyList}};

decode(Protocol, Binary) ->
    {error, Protocol, Binary}.

decode_data_65532(<<_/binary>>, Size, 0, List) ->
    {Size, List};
decode_data_65532(_Rest_ = <<_/binary>>, Size, Length, List) ->
    <<Data:32, _DataRest_/binary>> = _Rest_,
    decode_data_65532(_DataRest_, Size + byte_size(_Rest_) - byte_size(_DataRest_), Length - 1, [Data | List]).

decode_data_65533(<<_/binary>>, Size, 0, List) ->
    {Size, List};
decode_data_65533(_Rest_ = <<_/binary>>, Size, Length, List) ->
    <<Id:64, _IdRest_/binary>> = _Rest_,
    decode_data_65533(_IdRest_, Size + byte_size(_Rest_) - byte_size(_IdRest_), Length - 1, [{Id} | List]).

decode_name_ls_65534(<<_/binary>>, Size, 0, List) ->
    {Size, List};
decode_name_ls_65534(_Rest_ = <<_/binary>>, Size, Length, List) ->
    <<NameLsIx:32, _NameLsIxRest_/binary>> = _Rest_,
    <<NameLsNxLength:16, NameLsNx:NameLsNxLength/binary, _NameLsNxRest_/binary>> = _NameLsIxRest_,
    <<NameLsRxLength:16, _NameLsRxLengthRest_/binary>> = _NameLsNxRest_,
    {NameLsRxByteSize, NameLsRx} = decode_name_ls_rx_65534(_NameLsRxLengthRest_, 0, NameLsRxLength, []),
    <<_:NameLsRxByteSize/binary, _NameLsRxRest_/binary>> = _NameLsRxLengthRest_,
    <<NameLsSxLength:16, _NameLsSxLengthRest_/binary>> = _NameLsRxRest_,
    {NameLsSxByteSize, NameLsSx} = decode_name_ls_sx_65534(_NameLsSxLengthRest_, 0, NameLsSxLength, []),
    <<_:NameLsSxByteSize/binary, _NameLsSxRest_/binary>> = _NameLsSxLengthRest_,
    decode_name_ls_65534(_NameLsSxRest_, Size + byte_size(_Rest_) - byte_size(_NameLsSxRest_), Length - 1, [{NameLsIx, NameLsNx, NameLsRx, NameLsSx} | List]).

decode_name_ls_rx_65534(<<_/binary>>, Size, 0, List) ->
    {Size, List};
decode_name_ls_rx_65534(_Rest_ = <<_/binary>>, Size, Length, List) ->
    <<NameLsRxData:32, _NameLsRxDataRest_/binary>> = _Rest_,
    decode_name_ls_rx_65534(_NameLsRxDataRest_, Size + byte_size(_Rest_) - byte_size(_NameLsRxDataRest_), Length - 1, [NameLsRxData | List]).

decode_name_ls_sx_65534(<<_/binary>>, Size, 0, List) ->
    {Size, List};
decode_name_ls_sx_65534(_Rest_ = <<_/binary>>, Size, Length, List) ->
    <<NameLsSxIi:32, _NameLsSxIiRest_/binary>> = _Rest_,
    <<NameLsSxNnLength:16, NameLsSxNn:NameLsSxNnLength/binary, _NameLsSxNnRest_/binary>> = _NameLsSxIiRest_,
    decode_name_ls_sx_65534(_NameLsSxNnRest_, Size + byte_size(_Rest_) - byte_size(_NameLsSxNnRest_), Length - 1, [{NameLsSxIi, NameLsSxNn} | List]).

decode_name_lss_65534(<<_/binary>>, Size, 0, List) ->
    {Size, List};
decode_name_lss_65534(_Rest_ = <<_/binary>>, Size, Length, List) ->
    <<NameLssData:8, _NameLssDataRest_/binary>> = _Rest_,
    decode_name_lss_65534(_NameLssDataRest_, Size + byte_size(_Rest_) - byte_size(_NameLssDataRest_), Length - 1, [NameLssData | List]).

decode_tuple_list_65535(<<_/binary>>, Size, 0, List) ->
    {Size, List};
decode_tuple_list_65535(_Rest_ = <<_/binary>>, Size, Length, List) ->
    <<TupleListI16:16/signed, _TupleListI16Rest_/binary>> = _Rest_,
    <<TupleListBstLength:16, TupleListBst:TupleListBstLength/binary, _TupleListBstRest_/binary>> = _TupleListI16Rest_,
    decode_tuple_list_65535(_TupleListBstRest_, Size + byte_size(_Rest_) - byte_size(_TupleListBstRest_), Length - 1, [{TupleListI16, TupleListBst} | List]).

decode_tuple_single_65535(<<_/binary>>, Size, 0, List) ->
    {Size, List};
decode_tuple_single_65535(_Rest_ = <<_/binary>>, Size, Length, List) ->
    <<TupleSingleDataFlag:8, _TupleSingleDataRest_/binary>> = _Rest_,
    TupleSingleData = type:to_boolean(TupleSingleDataFlag),
    decode_tuple_single_65535(_TupleSingleDataRest_, Size + byte_size(_Rest_) - byte_size(_TupleSingleDataRest_), Length - 1, [TupleSingleData | List]).

decode_index_list_65535(<<_/binary>>, Size, 0, List) ->
    {Size, List};
decode_index_list_65535(_Rest_ = <<_/binary>>, Size, Length, List) ->
    <<IndexListBinary:6/binary, _IndexListBinaryRest_/binary>> = _Rest_,
    <<IndexListSubU8:8, _IndexListSubU8Rest_/binary>> = _IndexListBinaryRest_,
    <<IndexListSubStrBinaryLength:16, IndexListSubStrBinary:IndexListSubStrBinaryLength/binary, _IndexListSubStrRest_/binary>> = _IndexListSubU8Rest_,
    IndexListSubStr = unicode:characters_to_list(IndexListSubStrBinary),
    <<IndexListListLength:16, _IndexListListLengthRest_/binary>> = _IndexListSubStrRest_,
    {IndexListListByteSize, IndexListList} = decode_index_list_list_65535(_IndexListListLengthRest_, 0, IndexListListLength, []),
    <<_:IndexListListByteSize/binary, _IndexListListRest_/binary>> = _IndexListListLengthRest_,
    <<IndexListSingleLength:16, _IndexListSingleLengthRest_/binary>> = _IndexListListRest_,
    {IndexListSingleByteSize, IndexListSingle} = decode_index_list_single_65535(_IndexListSingleLengthRest_, 0, IndexListSingleLength, []),
    <<_:IndexListSingleByteSize/binary, _IndexListSingleRest_/binary>> = _IndexListSingleLengthRest_,
    decode_index_list_65535(_IndexListSingleRest_, Size + byte_size(_Rest_) - byte_size(_IndexListSingleRest_), Length - 1, [{IndexListBinary, {IndexListSubU8, IndexListSubStr}, IndexListList, IndexListSingle} | List]).

decode_index_list_list_65535(<<_/binary>>, Size, 0, List) ->
    {Size, List};
decode_index_list_list_65535(_Rest_ = <<_/binary>>, Size, Length, List) ->
    <<IndexListListI16:16/signed, _IndexListListI16Rest_/binary>> = _Rest_,
    <<IndexListListBstLength:16, IndexListListBst:IndexListListBstLength/binary, _IndexListListBstRest_/binary>> = _IndexListListI16Rest_,
    decode_index_list_list_65535(_IndexListListBstRest_, Size + byte_size(_Rest_) - byte_size(_IndexListListBstRest_), Length - 1, [{IndexListListI16, IndexListListBst} | List]).

decode_index_list_single_65535(<<_/binary>>, Size, 0, List) ->
    {Size, List};
decode_index_list_single_65535(_Rest_ = <<_/binary>>, Size, Length, List) ->
    <<IndexListSingleDataFlag:8, _IndexListSingleDataRest_/binary>> = _Rest_,
    IndexListSingleData = type:to_boolean(IndexListSingleDataFlag),
    decode_index_list_single_65535(_IndexListSingleDataRest_, Size + byte_size(_Rest_) - byte_size(_IndexListSingleDataRest_), Length - 1, [IndexListSingleData | List]).

decode_key_list_65535(<<_/binary>>, Size, 0, List) ->
    {Size, List};
decode_key_list_65535(_Rest_ = <<_/binary>>, Size, Length, List) ->
    <<KeyListBinary:6/binary, _KeyListBinaryRest_/binary>> = _Rest_,
    <<KeyListBooleanFlag:8, _KeyListBooleanRest_/binary>> = _KeyListBinaryRest_,
    KeyListBoolean = type:to_boolean(KeyListBooleanFlag),
    <<KeyListU8:8, _KeyListU8Rest_/binary>> = _KeyListBooleanRest_,
    <<KeyListU16:16, _KeyListU16Rest_/binary>> = _KeyListU8Rest_,
    <<KeyListU32:32, _KeyListU32Rest_/binary>> = _KeyListU16Rest_,
    <<KeyListU64:64, _KeyListU64Rest_/binary>> = _KeyListU32Rest_,
    <<KeyListI8:8/signed, _KeyListI8Rest_/binary>> = _KeyListU64Rest_,
    <<KeyListI16:16/signed, _KeyListI16Rest_/binary>> = _KeyListI8Rest_,
    <<KeyListI32:32/signed, _KeyListI32Rest_/binary>> = _KeyListI16Rest_,
    <<KeyListI64:64/signed, _KeyListI64Rest_/binary>> = _KeyListI32Rest_,
    <<KeyListF32:32/float, _KeyListF32Rest_/binary>> = _KeyListI64Rest_,
    <<KeyListF64:64/float, _KeyListF64Rest_/binary>> = _KeyListF32Rest_,
    <<KeyListStrBinaryLength:16, KeyListStrBinary:KeyListStrBinaryLength/binary, _KeyListStrRest_/binary>> = _KeyListF64Rest_,
    KeyListStr = unicode:characters_to_list(KeyListStrBinary),
    <<KeyListBstLength:16, KeyListBst:KeyListBstLength/binary, _KeyListBstRest_/binary>> = _KeyListStrRest_,
    decode_key_list_65535(_KeyListBstRest_, Size + byte_size(_Rest_) - byte_size(_KeyListBstRest_), Length - 1, [{KeyListBinary, KeyListBoolean, KeyListU8, KeyListU16, KeyListU32, KeyListU64, KeyListI8, KeyListI16, KeyListI32, KeyListI64, KeyListF32, KeyListF64, KeyListStr, KeyListBst} | List]).


-spec encode(Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()) -> {ok, binary()} | {error, Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()}.
encode(65531, Data) ->
    Data65531 = <<Data:16/signed>>,
    {ok, <<(byte_size(Data65531)):16, 65531:16, Data65531/binary>>};

encode(65532, Data) ->
    Data65532 = <<(encode_data_65532(<<>>, 0, Data))/binary>>,
    {ok, <<(byte_size(Data65532)):16, 65532:16, Data65532/binary>>};

encode(65533, Data) ->
    Data65533 = <<(encode_data_65533(<<>>, 0, Data))/binary>>,
    {ok, <<(byte_size(Data65533)):16, 65533:16, Data65533/binary>>};

encode(65534, Data) ->
    Data65534 = <<(encode_data_65534(<<>>, 0, Data))/binary>>,
    {ok, <<(byte_size(Data65534)):16, 65534:16, Data65534/binary>>};

encode(65535, {Binary, Boolean, U8, U16, U32, U64, I8, I16, I32, I64, F32, F64, Str, Bst, {TupleBinary, {TupleSubU8, TupleSubStr}, TupleList, TupleSingle}, IndexList, KeyList}) ->
    Data65535 = <<Binary:6/binary, (type:to_flag(Boolean)):8, U8:8, U16:16, U32:32, U64:64, I8:8/signed, I16:16/signed, I32:32/signed, I64:64/signed, F32:32/float, F64:64/float, (begin StrBinary = unicode:characters_to_binary(Str), <<(byte_size(StrBinary)):16, StrBinary/binary>> end)/binary, (byte_size(Bst)):16, (Bst)/binary, TupleBinary:6/binary, TupleSubU8:8, (begin TupleSubStrBinary = unicode:characters_to_binary(TupleSubStr), <<(byte_size(TupleSubStrBinary)):16, TupleSubStrBinary/binary>> end)/binary, (encode_tuple_list_65535(<<>>, 0, TupleList))/binary, (encode_tuple_single_65535(<<>>, 0, TupleSingle))/binary, (encode_index_list_65535(<<>>, 0, IndexList))/binary, (encode_key_list_65535(<<>>, 0, KeyList))/binary>>,
    {ok, <<(byte_size(Data65535)):16, 65535:16, Data65535/binary>>};

encode(Protocol, Data) ->
    {error, Protocol, Data}.

encode_data_65532(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_data_65532(Acc = <<_/binary>>, Length, [Data | Data]) ->
    encode_data_65532(<<Acc/binary, Data:32>>, Length + 1, Data).

encode_data_65533(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_data_65533(Acc = <<_/binary>>, Length, [{Name} | Data]) ->
    encode_data_65533(<<Acc/binary, (protocol:text(Name))/binary>>, Length + 1, Data).

encode_data_65534(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_data_65534(Acc = <<_/binary>>, Length, [{Name} | Data]) ->
    encode_data_65534(<<Acc/binary, Name:32>>, Length + 1, Data).

encode_tuple_list_65535(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_tuple_list_65535(Acc = <<_/binary>>, Length, [{TupleListI16, TupleListBst} | TupleList]) ->
    encode_tuple_list_65535(<<Acc/binary, TupleListI16:16/signed, (byte_size(TupleListBst)):16, (TupleListBst)/binary>>, Length + 1, TupleList).

encode_tuple_single_65535(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_tuple_single_65535(Acc = <<_/binary>>, Length, [TupleSingleData | TupleSingle]) ->
    encode_tuple_single_65535(<<Acc/binary, (type:to_flag(TupleSingleData)):8>>, Length + 1, TupleSingle).

encode_index_list_65535(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_index_list_65535(Acc = <<_/binary>>, Length, [{IndexListBinary, {IndexListSubU8, IndexListSubStr}, IndexListList, IndexListSingle} | IndexList]) ->
    encode_index_list_65535(<<Acc/binary, IndexListBinary:6/binary, IndexListSubU8:8, (begin IndexListSubStrBinary = unicode:characters_to_binary(IndexListSubStr), <<(byte_size(IndexListSubStrBinary)):16, IndexListSubStrBinary/binary>> end)/binary, (encode_index_list_list_65535(<<>>, 0, IndexListList))/binary, (encode_index_list_single_65535(<<>>, 0, IndexListSingle))/binary>>, Length + 1, IndexList).

encode_index_list_list_65535(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_index_list_list_65535(Acc = <<_/binary>>, Length, [{IndexListListI16, IndexListListBst} | IndexListList]) ->
    encode_index_list_list_65535(<<Acc/binary, IndexListListI16:16/signed, (byte_size(IndexListListBst)):16, (IndexListListBst)/binary>>, Length + 1, IndexListList).

encode_index_list_single_65535(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_index_list_single_65535(Acc = <<_/binary>>, Length, [IndexListSingleData | IndexListSingle]) ->
    encode_index_list_single_65535(<<Acc/binary, (type:to_flag(IndexListSingleData)):8>>, Length + 1, IndexListSingle).

encode_key_list_65535(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_key_list_65535(Acc = <<_/binary>>, Length, [{KeyListBinary, KeyListBoolean, KeyListU8, KeyListU16, KeyListU32, KeyListU64, KeyListI8, KeyListI16, KeyListI32, KeyListI64, KeyListF32, KeyListF64, KeyListStr, KeyListBst} | KeyList]) ->
    encode_key_list_65535(<<Acc/binary, KeyListBinary:6/binary, (type:to_flag(KeyListBoolean)):8, KeyListU8:8, KeyListU16:16, KeyListU32:32, KeyListU64:64, KeyListI8:8/signed, KeyListI16:16/signed, KeyListI32:32/signed, KeyListI64:64/signed, KeyListF32:32/float, KeyListF64:64/float, (begin KeyListStrBinary = unicode:characters_to_binary(KeyListStr), <<(byte_size(KeyListStrBinary)):16, KeyListStrBinary/binary>> end)/binary, (byte_size(KeyListBst)):16, (KeyListBst)/binary>>, Length + 1, KeyList).

