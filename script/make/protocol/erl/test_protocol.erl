-module(test_protocol).
-export([decode/2, encode/2]).

-spec decode(Protocol :: non_neg_integer(), Binary :: binary()) -> {ok, [integer() | binary() | list()]} | {error, Protocol :: non_neg_integer(), Binary :: binary()}.
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
    <<IndexListLength:16, _IndexListLengthRest_/binary>> = _BstRest_,
    {IndexListByteSize, IndexList} = decode_index_list_65535(_IndexListLengthRest_, 0, IndexListLength, []),
    <<_:IndexListByteSize/binary, _IndexListRest_/binary>> = _IndexListLengthRest_,
    <<KeyListLength:16, _KeyListLengthRest_/binary>> = _IndexListRest_,
    {KeyListByteSize, KeyList} = decode_key_list_65535(_KeyListLengthRest_, 0, KeyListLength, []),
    <<_:KeyListByteSize/binary, _KeyListRest_/binary>> = _KeyListLengthRest_,
    {ok, [Binary, Boolean, U8, U16, U32, U64, I8, I16, I32, I64, F32, F64, Str, Bst, IndexList, KeyList]};

decode(Protocol, Binary) ->
    {error, Protocol, Binary}.

decode_index_list_65535(<<_/binary>>, Size, 0, List) ->
    {Size, List};
decode_index_list_65535(_Rest_ = <<_/binary>>, Size, IndexListLength, List) ->
    <<ListBinary:6/binary, _ListBinaryRest_/binary>> = _Rest_,
    <<ListBooleanFlag:8, _ListBooleanRest_/binary>> = _ListBinaryRest_,
    ListBoolean = type:to_boolean(ListBooleanFlag),
    <<ListU8:8, _ListU8Rest_/binary>> = _ListBooleanRest_,
    <<ListU16:16, _ListU16Rest_/binary>> = _ListU8Rest_,
    <<ListU32:32, _ListU32Rest_/binary>> = _ListU16Rest_,
    <<ListU64:64, _ListU64Rest_/binary>> = _ListU32Rest_,
    <<ListI8:8/signed, _ListI8Rest_/binary>> = _ListU64Rest_,
    <<ListI16:16/signed, _ListI16Rest_/binary>> = _ListI8Rest_,
    <<ListI32:32/signed, _ListI32Rest_/binary>> = _ListI16Rest_,
    <<ListI64:64/signed, _ListI64Rest_/binary>> = _ListI32Rest_,
    <<ListF32:32/float, _ListF32Rest_/binary>> = _ListI64Rest_,
    <<ListF64:64/float, _ListF64Rest_/binary>> = _ListF32Rest_,
    <<ListStrBinaryLength:16, ListStrBinary:ListStrBinaryLength/binary, _ListStrRest_/binary>> = _ListF64Rest_,
    ListStr = unicode:characters_to_list(ListStrBinary),
    <<ListBstLength:16, ListBst:ListBstLength/binary, _ListBstRest_/binary>> = _ListStrRest_,
    decode_index_list_65535(_ListBstRest_, Size + byte_size(_Rest_) - byte_size(_ListBstRest_), IndexListLength - 1, [{ListBinary, ListBoolean, ListU8, ListU16, ListU32, ListU64, ListI8, ListI16, ListI32, ListI64, ListF32, ListF64, ListStr, ListBst} | List]).

decode_key_list_65535(<<_/binary>>, Size, 0, List) ->
    {Size, List};
decode_key_list_65535(_Rest_ = <<_/binary>>, Size, KeyListLength, List) ->
    <<ListBinary:6/binary, _ListBinaryRest_/binary>> = _Rest_,
    <<ListBooleanFlag:8, _ListBooleanRest_/binary>> = _ListBinaryRest_,
    ListBoolean = type:to_boolean(ListBooleanFlag),
    <<ListU8:8, _ListU8Rest_/binary>> = _ListBooleanRest_,
    <<ListU16:16, _ListU16Rest_/binary>> = _ListU8Rest_,
    <<ListU32:32, _ListU32Rest_/binary>> = _ListU16Rest_,
    <<ListU64:64, _ListU64Rest_/binary>> = _ListU32Rest_,
    <<ListI8:8/signed, _ListI8Rest_/binary>> = _ListU64Rest_,
    <<ListI16:16/signed, _ListI16Rest_/binary>> = _ListI8Rest_,
    <<ListI32:32/signed, _ListI32Rest_/binary>> = _ListI16Rest_,
    <<ListI64:64/signed, _ListI64Rest_/binary>> = _ListI32Rest_,
    <<ListF32:32/float, _ListF32Rest_/binary>> = _ListI64Rest_,
    <<ListF64:64/float, _ListF64Rest_/binary>> = _ListF32Rest_,
    <<ListStrBinaryLength:16, ListStrBinary:ListStrBinaryLength/binary, _ListStrRest_/binary>> = _ListF64Rest_,
    ListStr = unicode:characters_to_list(ListStrBinary),
    <<ListBstLength:16, ListBst:ListBstLength/binary, _ListBstRest_/binary>> = _ListStrRest_,
    decode_key_list_65535(_ListBstRest_, Size + byte_size(_Rest_) - byte_size(_ListBstRest_), KeyListLength - 1, [{ListBinary, ListBoolean, ListU8, ListU16, ListU32, ListU64, ListI8, ListI16, ListI32, ListI64, ListF32, ListF64, ListStr, ListBst} | List]).


-spec encode(Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()) -> {ok, binary()} | {error, Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()}.
encode(65535, [Binary, Boolean, U8, U16, U32, U64, I8, I16, I32, I64, F32, F64, Str, Bst, IndexList, KeyList]) ->
    Data65535 = <<Binary:6/binary, (type:to_flag(Boolean)):8, U8:8, U16:16, U32:32, U64:64, I8:8/signed, I16:16/signed, I32:32/signed, I64:64/signed, F32:32/float, F64:64/float, (begin StrBinary = unicode:characters_to_binary(Str), <<(byte_size(StrBinary)):16, StrBinary/binary>> end)/binary, (byte_size(Bst)):16, (Bst)/binary, (encode_index_list_65535(<<>>, 0, IndexList))/binary, (encode_key_list_65535(<<>>, 0, KeyList))/binary>>,
    {ok, <<(byte_size(Data65535)):16, 65535:16, Data65535/binary>>};

encode(Protocol, Data) ->
    {error, Protocol, Data}.

encode_index_list_65535(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_index_list_65535(Acc = <<_/binary>>, Length, [{ListBinary, ListBoolean, ListU8, ListU16, ListU32, ListU64, ListI8, ListI16, ListI32, ListI64, ListF32, ListF64, ListStr, ListBst} | IndexList]) ->
    encode_index_list_65535(<<Acc/binary, ListBinary:6/binary, (type:to_flag(ListBoolean)):8, ListU8:8, ListU16:16, ListU32:32, ListU64:64, ListI8:8/signed, ListI16:16/signed, ListI32:32/signed, ListI64:64/signed, ListF32:32/float, ListF64:64/float, (begin ListStrBinary = unicode:characters_to_binary(ListStr), <<(byte_size(ListStrBinary)):16, ListStrBinary/binary>> end)/binary, (byte_size(ListBst)):16, (ListBst)/binary>>, Length + 1, IndexList).

encode_key_list_65535(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_key_list_65535(Acc = <<_/binary>>, Length, [{ListBinary, ListBoolean, ListU8, ListU16, ListU32, ListU64, ListI8, ListI16, ListI32, ListI64, ListF32, ListF64, ListStr, ListBst} | KeyList]) ->
    encode_key_list_65535(<<Acc/binary, ListBinary:6/binary, (type:to_flag(ListBoolean)):8, ListU8:8, ListU16:16, ListU32:32, ListU64:64, ListI8:8/signed, ListI16:16/signed, ListI32:32/signed, ListI64:64/signed, ListF32:32/float, ListF64:64/float, (begin ListStrBinary = unicode:characters_to_binary(ListStr), <<(byte_size(ListStrBinary)):16, ListStrBinary/binary>> end)/binary, (byte_size(ListBst)):16, (ListBst)/binary>>, Length + 1, KeyList).

