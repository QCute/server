%%%-------------------------------------------------------------------
%%! +pc unicode
%%% @doc
%%% protocol test code
%%% @end
%%%-------------------------------------------------------------------
-module(test).
-mode(compile).
-compile(nowarn_export_all).
-compile(export_all).

%%%===================================================================
%%% API functions
%%%===================================================================

%% 65533
test_single_protocol(User, Protocol, Single) ->
    io:format("~ts~n", [pretty:print([User, Protocol, Single])]).

%% 65534
test_list_protocol(User, Protocol, List) ->
    io:format("~ts~n", [pretty:print([User, Protocol, List])]).

%% 65535
test_protocol(User, Protocol, Binary, Boolean, U8, U16, U32, U64, I8, I16, I32, I64, F32, F64, Str, Bst, Tuple, IndexList, KeyList) ->
    io:format("~ts~n", pretty:print([User, Protocol, Binary, Boolean, U8, U16, U32, U64, I8, I16, I32, I64, F32, F64, Str, Bst, Tuple, IndexList, KeyList])).

protocol_test() ->
    %% Binary, Boolean, U8, U16, U32, U64, I8, I16, I32, I64, F32, F64, Str, Bst, IndexList, KeyList
    Data = {
        <<"abcdef">>,
        true,

        1,
        2,
        3,
        4,

        4,
        3,
        2,
        1,

        1.23,
        4.56,
        "一23",
        <<"1二三"/utf8>>,

        {
            <<"abcdef">>,
            {
                95,
                "xyz"
            },
            [
                {456, <<"wow">>},
                {369, <<"oops">>}
            ],

            [true, false, false, true, false]
        },

        [{
            <<"abcdef">>,
            {
                95,
                "xyz"
            },
            [
                {456, <<"wow">>},
                {369, <<"oops">>}
            ],

            [true, false, false, true, false]
        }],
        [{
            <<"abcdef">>,
            false,

            1,
            2,
            3,
            4,

            4,
            3,
            2,
            1,

            1.23,
            4.56,

            "一23",
            <<"1二三"/utf8>>
        }]
    },
    {ok, Binary} = user_router:encode(65535, Data),
    io:format("~tp~n", [Binary]),
    <<Length:16, 65535:16, Packet:Length/binary>> = Binary,
    {ok, Result} = user_router:decode(65535, Packet),
    io:format("~ts~n", [pretty:print(Result)]).
