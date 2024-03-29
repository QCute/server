-module(test_handler).
-export([handle/3]).

handle(User, 65535, [Binary, Boolean, U8, U16, U32, U64, I8, I16, I32, I64, F32, F64, Str, Bst, IndexList, KeyList]) ->
    test:test_protocol(User, 65535, Binary, Boolean, U8, U16, U32, U64, I8, I16, I32, I64, F32, F64, Str, Bst, IndexList, KeyList);

handle(_, Protocol, Data) ->
    {error, Protocol, Data}.
