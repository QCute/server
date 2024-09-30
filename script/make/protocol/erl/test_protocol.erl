-module(test_protocol).
-export([decode/2, encode/2]).

-spec decode(Protocol :: non_neg_integer(), Binary :: binary()) -> {ok, [integer() | binary() | list()]} | {error, Protocol :: non_neg_integer(), Binary :: binary()}.
decode(65535, _Rest_ = <<_/binary>>) ->
    <<Bin:6/binary, _BinRest_/binary>> = _Rest_,
    <<BoolFlag:8, _BoolRest_/binary>> = _BinRest_,
    Bool = type:to_boolean(BoolFlag),
    <<U8:8, _U8Rest_/binary>> = _BoolRest_,
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
    <<Bin:6/binary, _BinRest_/binary>> = _BstRest_,
    <<U8:8, _U8Rest_/binary>> = _BinRest_,
    <<StrBinaryLength:16, StrBinary:StrBinaryLength/binary, _StrRest_/binary>> = _U8Rest_,
    Str = unicode:characters_to_list(StrBinary),
    <<ListLength:16, _ListLengthRest_/binary>> = _StrRest_,
    {ListByteSize, List} = decode_list_65535(_ListLengthRest_, 0, ListLength, []),
    <<_:ListByteSize/binary, _ListRest_/binary>> = _ListLengthRest_,
    <<SingleLength:16, _SingleLengthRest_/binary>> = _ListRest_,
    {SingleByteSize, Single} = decode_single_65535(_SingleLengthRest_, 0, SingleLength, []),
    <<_:SingleByteSize/binary, _SingleRest_/binary>> = _SingleLengthRest_,
    <<IndexListLength:16, _IndexListLengthRest_/binary>> = _SingleRest_,
    {IndexListByteSize, IndexList} = decode_index_list_65535(_IndexListLengthRest_, 0, IndexListLength, []),
    <<_:IndexListByteSize/binary, _IndexListRest_/binary>> = _IndexListLengthRest_,
    <<KeyListLength:16, _KeyListLengthRest_/binary>> = _IndexListRest_,
    {KeyListByteSize, KeyList} = decode_key_list_65535(_KeyListLengthRest_, 0, KeyListLength, []),
    <<_:KeyListByteSize/binary, _KeyListRest_/binary>> = _KeyListLengthRest_,
    {ok, {Bin, Bool, U8, U16, U32, U64, I8, I16, I32, I64, F32, F64, Str, Bst, {Bin, {U8, Str}, List, Single}, IndexList, KeyList}};

decode(Protocol, Binary) ->
    {error, Protocol, Binary}.

decode_key_list_65535(<<_/binary>>, Size, 0, List) ->
    {Size, List};
decode_key_list_65535(_Rest_ = <<_/binary>>, Size, KeyListLength, List) ->
    <<Bin:6/binary, _BinRest_/binary>> = _Rest_,
    <<BoolFlag:8, _BoolRest_/binary>> = _BinRest_,
    Bool = type:to_boolean(BoolFlag),
    <<U8:8, _U8Rest_/binary>> = _BoolRest_,
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
    decode_key_list_65535(_BstRest_, Size + byte_size(_Rest_) - byte_size(_BstRest_), KeyListLength - 1, [{Bin, Bool, U8, U16, U32, U64, I8, I16, I32, I64, F32, F64, Str, Bst} | List]).

decode_index_list_65535(<<_/binary>>, Size, 0, List) ->
    {Size, List};
decode_index_list_65535(_Rest_ = <<_/binary>>, Size, IndexListLength, List) ->
    <<Bin:6/binary, _BinRest_/binary>> = _Rest_,
    <<U8:8, _U8Rest_/binary>> = _BinRest_,
    <<StrBinaryLength:16, StrBinary:StrBinaryLength/binary, _StrRest_/binary>> = _U8Rest_,
    Str = unicode:characters_to_list(StrBinary),
    <<ListLength:16, _ListLengthRest_/binary>> = _StrRest_,
    {ListByteSize, List} = decode_list_65535(_ListLengthRest_, 0, ListLength, []),
    <<_:ListByteSize/binary, _ListRest_/binary>> = _ListLengthRest_,
    <<SingleLength:16, _SingleLengthRest_/binary>> = _ListRest_,
    {SingleByteSize, Single} = decode_single_65535(_SingleLengthRest_, 0, SingleLength, []),
    <<_:SingleByteSize/binary, _SingleRest_/binary>> = _SingleLengthRest_,
    decode_index_list_65535(_SingleRest_, Size + byte_size(_Rest_) - byte_size(_SingleRest_), IndexListLength - 1, [{Bin, {U8, Str}, List, Single} | List]).

decode_single_65535(<<_/binary>>, Size, 0, List) ->
    {Size, List};
decode_single_65535(_Rest_ = <<_/binary>>, Size, SingleLength, List) ->
    <<SingleItem:8, _SingleItemRest_/binary>> = _Rest_,
    decode_single_65535(_SingleItemRest_, Size + byte_size(_Rest_) - byte_size(_SingleItemRest_), SingleLength - 1, [SingleItem | List]).

decode_list_65535(<<_/binary>>, Size, 0, List) ->
    {Size, List};
decode_list_65535(_Rest_ = <<_/binary>>, Size, ListLength, List) ->
    <<I16:16/signed, _I16Rest_/binary>> = _Rest_,
    <<BstLength:16, Bst:BstLength/binary, _BstRest_/binary>> = _I16Rest_,
    decode_list_65535(_BstRest_, Size + byte_size(_Rest_) - byte_size(_BstRest_), ListLength - 1, [{I16, Bst} | List]).

decode_list_65535(<<_/binary>>, Size, 0, List) ->
    {Size, List};
decode_list_65535(_Rest_ = <<_/binary>>, Size, ListLength, List) ->
    <<I16:16/signed, _I16Rest_/binary>> = _Rest_,
    <<BstLength:16, Bst:BstLength/binary, _BstRest_/binary>> = _I16Rest_,
    decode_list_65535(_BstRest_, Size + byte_size(_Rest_) - byte_size(_BstRest_), ListLength - 1, [{I16, Bst} | List]).

decode_single_65535(<<_/binary>>, Size, 0, List) ->
    {Size, List};
decode_single_65535(_Rest_ = <<_/binary>>, Size, SingleLength, List) ->
    <<SingleItem:8, _SingleItemRest_/binary>> = _Rest_,
    decode_single_65535(_SingleItemRest_, Size + byte_size(_Rest_) - byte_size(_SingleItemRest_), SingleLength - 1, [SingleItem | List]).


-spec encode(Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()) -> {ok, binary()} | {error, Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()}.
encode(65535, Data) ->
    Data65535 = <<(protocol:text(Data))/binary>>,
    {ok, <<(byte_size(Data65535)):16, 65535:16, Data65535/binary>>};

encode(Protocol, Data) ->
    {error, Protocol, Data}.

