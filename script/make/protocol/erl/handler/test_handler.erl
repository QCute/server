-module(test_handler).
-export([handle/3]).
-export([send_test_protocol/18]).
-include("user.hrl").

handle(User, 65535, [Binary, Boolean, U8, U16, U32, U64, I8, I16, I32, I64, F32, F64, Str, Bst, IndexList, KeyList]) ->
    test:test_protocol(User, 65535, Binary, Boolean, U8, U16, U32, U64, I8, I16, I32, I64, F32, F64, Str, Bst, IndexList, KeyList);

handle(_, Protocol, Data) ->
    {error, Protocol, Data}.

send_test_protocol(User, 65535, Binary, Boolean, U8, U16, U32, U64, I8, I16, I32, I64, F32, F64, Str, Bst, IndexList, KeyList) ->
    {ok, Binary} = test_protocol:encode(65535, [Binary, Boolean, U8, U16, U32, U64, I8, I16, I32, I64, F32, F64, Str, Bst, IndexList, KeyList]),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

