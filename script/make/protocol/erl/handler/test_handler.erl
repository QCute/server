-module(test_handler).
-export([handle/3]).
-export([send_test_single_protocol/3]).
-export([send_test_list_protocol/3]).
-export([send_test_list_tuple_protocol/3]).
-export([send_test_ls_protocol/3]).
-export([send_test_protocol/19]).
-include("user.hrl").

handle(User, 65531, Data) ->
    test:test_single_protocol(User, 65531, Data);

handle(User, 65532, Data) ->
    test:test_list_protocol(User, 65532, Data);

handle(User, 65533, Data) ->
    test:test_list_tuple_protocol(User, 65533, Data);

handle(User, 65534, {Id, TheItemData, Name}) ->
    test:test_ls_protocol(User, 65534, Id, TheItemData, Name);

handle(User, 65535, {Binary, Boolean, U8, U16, U32, U64, I8, I16, I32, I64, F32, F64, Str, Bst, Tuple, IndexList, KeyList}) ->
    test:test_protocol(User, 65535, Binary, Boolean, U8, U16, U32, U64, I8, I16, I32, I64, F32, F64, Str, Bst, Tuple, IndexList, KeyList);

handle(_, Protocol, Data) ->
    {error, Protocol, Data}.

send_test_single_protocol(User, 65531, Data) ->
    {ok, Binary} = test_protocol:encode(65531, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_test_list_protocol(User, 65532, Data) ->
    {ok, Binary} = test_protocol:encode(65532, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_test_list_tuple_protocol(User, 65533, Data) ->
    {ok, Binary} = test_protocol:encode(65533, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_test_ls_protocol(User, 65534, Data) ->
    {ok, Binary} = test_protocol:encode(65534, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_test_protocol(User, 65535, Binary, Boolean, U8, U16, U32, U64, I8, I16, I32, I64, F32, F64, Str, Bst, Tuple, IndexList, KeyList) ->
    {ok, Binary} = test_protocol:encode(65535, {Binary, Boolean, U8, U16, U32, U64, I8, I16, I32, I64, F32, F64, Str, Bst, Tuple, IndexList, KeyList}),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

