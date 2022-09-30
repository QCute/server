-module(test_protocol).
-export([read/2, write/2]).


-spec read(Protocol :: non_neg_integer(), Binary :: binary()) -> {ok, [integer() | binary() | list()]} | {error, Protocol :: non_neg_integer(), Binary :: binary()}.
read(65535, <<Binary:6/binary, Boolean:8, U8:8, U16:16, U32:32, U64:64, I8:8/signed, I16:16/signed, I32:32/signed, I64:64/signed, F32:32/float, F64:64/float, StrLength:16, Str:StrLength/binary, BstLength:16, Bst:BstLength/binary, Binary/binary>>) ->
    BooleanFlag = type:to_boolean(Boolean),
    StrString = binary_to_list(Str),
    {IndexList, IndexListRest} = protocol:read_list(fun(IndexListBinary) -> <<ListBinary:6/binary, ListBoolean:8, ListU8:8, ListU16:16, ListU32:32, ListU64:64, ListI8:8/signed, ListI16:16/signed, ListI32:32/signed, ListI64:64/signed, ListF32:32/float, ListF64:64/float, ListStrLength:16, ListStr:ListStrLength/binary, ListBstLength:16, ListBst:ListBstLength/binary, IndexListInnerRest/binary>> = IndexListBinary, ListBooleanFlag = type:to_boolean(ListBoolean), ListStrString = binary_to_list(ListStr), {{ListBinary, ListBooleanFlag, ListU8, ListU16, ListU32, ListU64, ListI8, ListI16, ListI32, ListI64, ListF32, ListF64, ListStrString, ListBst}, IndexListInnerRest} end, Binary),
    {KeyList, _} = protocol:read_list(fun(KeyListBinary) -> <<ListBinary:6/binary, ListBoolean:8, ListU8:8, ListU16:16, ListU32:32, ListU64:64, ListI8:8/signed, ListI16:16/signed, ListI32:32/signed, ListI64:64/signed, ListF32:32/float, ListF64:64/float, ListStrLength:16, ListStr:ListStrLength/binary, ListBstLength:16, ListBst:ListBstLength/binary, KeyListInnerRest/binary>> = KeyListBinary, ListBooleanFlag = type:to_boolean(ListBoolean), ListStrString = binary_to_list(ListStr), {{ListBinary, ListBooleanFlag, ListU8, ListU16, ListU32, ListU64, ListI8, ListI16, ListI32, ListI64, ListF32, ListF64, ListStrString, ListBst}, KeyListInnerRest} end, IndexListRest),
    {ok, [Binary, BooleanFlag, U8, U16, U32, U64, I8, I16, I32, I64, F32, F64, StrString, Bst, IndexList, KeyList]};

read(Protocol, Binary) ->
    {error, Protocol, Binary}.


-spec write(Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()) -> {ok, binary()} | {error, Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()}.
write(65535, [Binary, Boolean, U8, U16, U32, U64, I8, I16, I32, I64, F32, F64, Str, Bst, IndexList, KeyList]) ->
    IndexListBinary = protocol:write_list(fun({ListBinary, ListBoolean, ListU8, ListU16, ListU32, ListU64, ListI8, ListI16, ListI32, ListI64, ListF32, ListF64, ListStr, ListBst}) -> <<ListBinary:6/binary, (type:to_flag(ListBoolean)):8, ListU8:8, ListU16:16, ListU32:32, ListU64:64, ListI8:8/signed, ListI16:16/signed, ListI32:32/signed, ListI64:64/signed, ListF32:32/float, ListF64:64/float, (length(ListStr)):16, (list_to_binary(ListStr))/binary, (byte_size(ListBst)):16, (ListBst)/binary>> end, IndexList),
    KeyListBinary = protocol:write_list(fun({ListBinary, ListBoolean, ListU8, ListU16, ListU32, ListU64, ListI8, ListI16, ListI32, ListI64, ListF32, ListF64, ListStr, ListBst}) -> <<ListBinary:6/binary, (type:to_flag(ListBoolean)):8, ListU8:8, ListU16:16, ListU32:32, ListU64:64, ListI8:8/signed, ListI16:16/signed, ListI32:32/signed, ListI64:64/signed, ListF32:32/float, ListF64:64/float, (length(ListStr)):16, (list_to_binary(ListStr))/binary, (byte_size(ListBst)):16, (ListBst)/binary>> end, KeyList),
    {ok, protocol:pack(65535, <<Binary:6/binary, (type:to_flag(Boolean)):8, U8:8, U16:16, U32:32, U64:64, I8:8/signed, I16:16/signed, I32:32/signed, I64:64/signed, F32:32/float, F64:64/float, (length(Str)):16, (list_to_binary(Str))/binary, (byte_size(Bst)):16, (Bst)/binary, IndexListBinary/binary, KeyListBinary/binary>>)};

write(Protocol, Data) ->
    {error, Protocol, Data}.


