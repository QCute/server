%%%-------------------------------------------------------------------
%%% @doc
%%% make protocol define to lua io/metadata code
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_maker_lua).
-export([format_code/3, format_meta/2]).
-export([parse_meta_lua/2]).
-export([parse_encode_lua/2, parse_decode_lua/2]).
-include("../../../include/serialize.hrl").
%% ast metadata
-record(meta, {name = [], type, comment = [], explain = [], key}).
%% file
-record(file, {import = [], export = [], function = [], extra = []}).
%% language code set
-record(set, {code = #file{}, meta = #file{}, handler = #file{}}).
%% protocol data
-record(data, {protocol = 0, erl = #set{}, lua = #set{}, js = #set{}, cs = #set{}, html = #set{}}).
%%%===================================================================
%%% API functions
%%%===================================================================
format_code(LuaName, EncodeList, DecodeList) ->
    %% encode
    LuaEncode = [Encode || #data{lua = #set{code = #file{function = Encode}}} <- EncodeList, Encode =/= []],
    LuaEncodeDefault = lists:concat([

    ]),
    Encode = lists:append(LuaEncode, [LuaEncodeDefault]),

    %% decode
    LuaDecode = [Decode || #data{lua = #set{code = #file{function = Decode}}} <- DecodeList, Decode =/= []],
    LuaDecodeDefault = lists:concat([

    ]),
    Decode = lists:append(LuaDecode, [LuaDecodeDefault]),

    lists:concat([
        "function encode", LuaName, "(offset, protocol, data)", "\n",
        "    ", string:join(Encode, ""), "\n",
        "    ", "    ", "error(string.format('unknown protocol define: %d', protocol))", "\n"
        "    ", "end", "\n",
        "end", "\n\n",
        "function decode", LuaName, "(offset, protocol, data)", "\n",
        "    ", string:join(Decode, ""), "\n",
        "    ", "    ", "error(string.format('unknown protocol define: %d', protocol))", "\n"
        "    ", "end", "\n",
        "end"
    ]).

format_meta(_LuaName, List) ->
    LuaMetaInner = string:join([lists:concat([
        "    ", "[", Protocol, "] = {", "\n",
        "    ", "    ", "[\"comment\"] = \"", Comment, "\",", "\n",
        "    ", "    ", "[\"write\"] = ", Read, ",", "\n",
        "    ", "    ", "[\"read\"] = ", Write, "\n",
        "    ", "}"
    ]) || {Protocol, Comment, #data{lua = #set{meta = #file{extra = Read}}}, #data{lua = #set{meta = #file{extra = Write}}}} <- List, Protocol =/= 0], ",\n"),
    lists:concat([
        "return", " ", "{", "\n",
        LuaMetaInner, "\n",
        "}"
    ]).

%%%===================================================================
%%% meta
%%%===================================================================
%% lua meta
parse_meta_lua(_, []) ->
    "{}";
parse_meta_lua(_, Meta) ->
    %% start with 3 tabs(4 space) padding
    Padding = lists:duplicate(2, "    "),
    MetaData = parse_meta_lua_loop(Meta, 3, []),
    %% format one protocol define
    %% lists:concat(["        [", Protocol, "] = {\n", MetaData, "\n        }"]).
    Code = lists:concat(["{\n", MetaData, "\n", Padding, "}"]),

    #file{extra = Code}.

parse_meta_lua_loop([], _, List) ->
    %% construct as a list
    string:join(lists:reverse(List), ",\n");

parse_meta_lua_loop([#meta{name = _, type = zero} | T], Depth, List) ->
    parse_meta_lua_loop(T, Depth, List);

parse_meta_lua_loop([#meta{name = Name, type = binary, explain = Length, comment = Comment} | T], Depth, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    Code = lists:flatten(io_lib:format("~s{name = \"~s\", type = \"~s\", comment = \"~ts\", explain = ~w}", [Padding, word:to_lower_hump(Name), binary, Comment, Length])),
    parse_meta_lua_loop(T, Depth, [Code | List]);

parse_meta_lua_loop([#meta{name = Name, type = Type, explain = [], comment = Comment} | T], Depth, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    Code = lists:flatten(io_lib:format("~s{name = \"~s\", type = \"~s\", comment = \"~ts\", explain = ~w}", [Padding, word:to_lower_hump(Name), Type, Comment, {}])),
    parse_meta_lua_loop(T, Depth, [Code | List]);

parse_meta_lua_loop([#meta{name = _, type = tuple, comment = _, explain = Explain} | T], Depth, List) ->
    %% recursive
    SubCodes = parse_meta_lua_loop(Explain, Depth, []),
    parse_meta_lua_loop(T, Depth, [SubCodes | List]);

parse_meta_lua_loop([#meta{name = _, type = record, comment = _, explain = Explain} | T], Depth, List) ->
    %% recursive
    SubCodes = parse_meta_lua_loop(Explain, Depth, []),
    parse_meta_lua_loop(T, Depth, [SubCodes | List]);

parse_meta_lua_loop([#meta{name = Name, type = list, comment = Comment, explain = Explain, key = undefined} | T], Depth, List) ->
    %% recursive
    SubCodes = parse_meta_lua_loop(Explain, Depth + 1, []),
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    Code = lists:flatten(io_lib:format("~s{name = \"~s\", type = \"~s\", comment = \"~ts\", explain = {\n~ts\n~s}}", [Padding, word:to_lower_hump(Name), list, Comment, SubCodes, Padding])),
    parse_meta_lua_loop(T, Depth, [Code | List]);

parse_meta_lua_loop([#meta{name = Name, type = list, comment = Comment, explain = Explain, key = Key} | T], Depth, List) ->
    %% recursive
    SubCodes = parse_meta_lua_loop(Explain, Depth + 1, []),
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    Code = lists:flatten(io_lib:format("~s{name = \"~s\", type = \"~s\", comment = \"~ts\", key = \"~ts\", explain = {\n~ts\n~s}}", [Padding, word:to_lower_hump(Name), map, Comment, word:to_lower_hump(Key), SubCodes, Padding])),
    parse_meta_lua_loop(T, Depth, [Code | List]);

parse_meta_lua_loop([#meta{name = Name, type = ets, comment = Comment, explain = Explain, key = undefined} | T], Depth, List) ->
    %% recursive
    SubCodes = parse_meta_lua_loop(Explain, Depth + 1, []),
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    Code = lists:flatten(io_lib:format("~s{name = \"~s\", type = \"~s\", comment = \"~ts\", explain = {\n~ts\n~s}}", [Padding, word:to_lower_hump(Name), list, Comment, SubCodes, Padding])),
    parse_meta_lua_loop(T, Depth, [Code | List]);

parse_meta_lua_loop([#meta{name = Name, type = ets, comment = Comment, explain = Explain, key = Key} | T], Depth, List) ->
    %% recursive
    SubCodes = parse_meta_lua_loop(Explain, Depth + 1, []),
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    Code = lists:flatten(io_lib:format("~s{name = \"~s\", type = \"~s\", comment = \"~ts\", key = \"~ts\", explain = {\n~ts\n~s}}", [Padding, word:to_lower_hump(Name), map, Comment, word:to_lower_hump(Key), SubCodes, Padding])),
    parse_meta_lua_loop(T, Depth, [Code | List]).

%%%===================================================================
%%% encode
%%%===================================================================
%% lua code
parse_encode_lua(Protocol, Meta) ->
    %% start with 3 tabs(4 space) padding
    Padding = lists:duplicate(1, "    "),
    {_, Codes} = parse_encode_lua_loop(Meta, 2, "data", [], []),

    %% codes format
    CodesDefault = lists:concat([
        Padding, "    ", "return table", "\n"
    ]),
    CodesBlock = lists:append(Codes, [CodesDefault]),

    %% format one protocol define
    Code = lists:concat([
        "if protocol == ", Protocol, " then", "\n",
        Padding, "    ", "local offset = offset", "\n",
        Padding, "    ", "local table = {}", "\n",
        string:join(CodesBlock, "\n"),
        Padding, "else"
    ]),

    #file{function = Code}.

%% lua code
parse_encode_lua_loop([], _, _, Fields, List) ->
    %% construct as a list
    %% {string:join(lists:reverse(Fields), ", "), string:join(lists:reverse(List), "\n")};
    {lists:reverse(Fields), lists:reverse(List)};

parse_encode_lua_loop([#meta{name = Name, type = binary, comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = ", ScopeArgs, "[\"", HumpName, "\"]", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_encode_lua_loop(T, Depth, ScopeArgs, [HumpName | Fields], [Code | List]);

parse_encode_lua_loop([#meta{name = Name, type = bool, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = string.pack(\">I1\", ", ScopeArgs, "[\"", HumpName, "\"] ~= 0 and 1 or 0)", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_encode_lua_loop(T, Depth, ScopeArgs, [HumpName | Fields], [Code | List]);

parse_encode_lua_loop([#meta{name = Name, type = u8, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = string.pack(\">I1\", ", ScopeArgs, "[\"", HumpName, "\"])", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_encode_lua_loop(T, Depth, ScopeArgs, [HumpName | Fields], [Code | List]);

parse_encode_lua_loop([#meta{name = Name, type = u16, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = string.pack(\">I2\", ", ScopeArgs, "[\"", HumpName, "\"])", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_encode_lua_loop(T, Depth, ScopeArgs, [HumpName | Fields], [Code | List]);

parse_encode_lua_loop([#meta{name = Name, type = u32, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = string.pack(\">I4\", ", ScopeArgs, "[\"", HumpName, "\"])", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_encode_lua_loop(T, Depth, ScopeArgs, [HumpName | Fields], [Code | List]);

parse_encode_lua_loop([#meta{name = Name, type = u64, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = string.pack(\">I8\", ", ScopeArgs, "[\"", HumpName, "\"])", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_encode_lua_loop(T, Depth, ScopeArgs, [HumpName | Fields], [Code | List]);

parse_encode_lua_loop([#meta{name = Name, type = i8, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = string.pack(\">i1\", ", ScopeArgs, "[\"", HumpName, "\"])", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_encode_lua_loop(T, Depth, ScopeArgs, [HumpName | Fields], [Code | List]);

parse_encode_lua_loop([#meta{name = Name, type = i16, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = string.pack(\">i2\", ", ScopeArgs, "[\"", HumpName, "\"])", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_encode_lua_loop(T, Depth, ScopeArgs, [HumpName | Fields], [Code | List]);

parse_encode_lua_loop([#meta{name = Name, type = i32, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = string.pack(\">i4\", ", ScopeArgs, "[\"", HumpName, "\"])", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_encode_lua_loop(T, Depth, ScopeArgs, [HumpName | Fields], [Code | List]);

parse_encode_lua_loop([#meta{name = Name, type = i64, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = string.pack(\">i8\", ", ScopeArgs, "[\"", HumpName, "\"])", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_encode_lua_loop(T, Depth, ScopeArgs, [HumpName | Fields], [Code | List]);

parse_encode_lua_loop([#meta{name = Name, type = f32, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = string.pack(\">f\", ", ScopeArgs, "[\"", HumpName, "\"])", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_encode_lua_loop(T, Depth, ScopeArgs, [HumpName | Fields], [Code | List]);

parse_encode_lua_loop([#meta{name = Name, type = f64, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = string.pack(\">d\", ", ScopeArgs, "[\"", HumpName, "\"])", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_encode_lua_loop(T, Depth, ScopeArgs, [HumpName | Fields], [Code | List]);

parse_encode_lua_loop([#meta{name = Name, type = str, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = string.pack(\">s2\", ", ScopeArgs, "[\"", HumpName, "\"])", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_encode_lua_loop(T, Depth, ScopeArgs, [HumpName | Fields], [Code | List]);

parse_encode_lua_loop([#meta{name = Name, type = bst, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = string.pack(\">s2\", ", ScopeArgs, "[\"", HumpName, "\"])", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_encode_lua_loop(T, Depth, ScopeArgs, [HumpName | Fields], [Code | List]);

parse_encode_lua_loop([#meta{name = Name, type = rst, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = string.pack(\">s2\", ", ScopeArgs, "[\"", HumpName, "\"])", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_encode_lua_loop(T, Depth, ScopeArgs, [HumpName | Fields], [Code | List]);

parse_encode_lua_loop([#meta{name = _, type = tuple, explain = Explain} | T], Depth, ScopeArgs, Names, List) ->
    %% recursive
    {SubNames, SubCodes} = parse_encode_lua_loop(Explain, Depth, ScopeArgs, Names, List),

    %% flatten
    parse_encode_lua_loop(T, Depth, ScopeArgs, lists:reverse(SubNames), lists:reverse(SubCodes));

parse_encode_lua_loop([#meta{name = _, type = record, explain = Explain} | T], Depth, ScopeArgs, Names, List) ->
    %% recursive
    {SubNames, SubCodes} = parse_encode_lua_loop(Explain, Depth, ScopeArgs, Names, List),

    %% flatten
    parse_encode_lua_loop(T, Depth, ScopeArgs, lists:reverse(SubNames), lists:reverse(SubCodes));

parse_encode_lua_loop([#meta{name = Name, type = list, comment = Comment, explain = Explain, key = undefined} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    %% recursive
    {_, SubCodes} = parse_encode_lua_loop(Explain, Depth + 1, lists:concat([HumpName, "Table[", HumpName, "Index]"]), [], []),
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", HumpName, "Table = ", ScopeArgs, "[\"", HumpName, "\"]", "\n",
        Padding, "table[offset] = string.pack(\">I2\", #", HumpName, "Table)", "\n",
        Padding, "offset = offset + 1", "\n",
        Padding, "for ", HumpName, "Index = 1, #", HumpName, "Table do", "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "end"
    ],
    parse_encode_lua_loop(T, Depth, ScopeArgs, [HumpName | Fields], [Code | List]);

parse_encode_lua_loop([#meta{name = Name, type = list, comment = Comment, explain = [#meta{explain = SubExplain}]} | T], Depth, ScopeArgs, Fields, List) ->

    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    %% recursive
    {_, SubCodes} = parse_encode_lua_loop(SubExplain, Depth + 1, lists:concat([HumpName, "ItemData"]), [], []),
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", HumpName, "Table = ", ScopeArgs, "[\"", HumpName, "\"]", "\n",
        Padding, "table[offset] = string.pack(\">I2\", #", HumpName, "Table)", "\n",
        Padding, "offset = offset + 1", "\n",
        Padding, "for _, ", HumpName, "ItemData in pairs(", HumpName, "Table) do", "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "end"
    ],
    parse_encode_lua_loop(T, Depth, ScopeArgs, [HumpName | Fields], [Code | List]).

%%%===================================================================
%%% decode
%%%===================================================================
%% lua code
parse_decode_lua(Protocol, Meta) ->
    %% start with 3 tabs(4 space) padding
    Padding = lists:duplicate(1, "    "),
    {Fields, Codes} = parse_decode_lua_loop(Meta, 2, "data", [], []),

    %% codes format
    CodesDefault = lists:concat([
        Padding, "    ", "return {", string:join([lists:concat([SubName, " = ", SubName]) || SubName <- Fields], ", "), "}", "\n"
    ]),
    CodesBlock = lists:append(Codes, [CodesDefault]),

    %% format one protocol define
    Code = lists:concat([
        "if protocol == ", Protocol, " then", "\n",
        Padding, "    ", "local offset = offset", "\n",
        string:join(CodesBlock, "\n"),
        Padding, "else"
    ]),

    #file{function = Code}.

%% lua code
parse_decode_lua_loop([], _, _, Fields, List) ->
    %% construct as a list
    %% {string:join([lists:concat([Name, " = ", Name]) || Name <- lists:reverse(Fields)], ", "), string:join(lists:reverse(List), "\n")};
    {lists:reverse(Fields), lists:reverse(List)};

parse_decode_lua_loop([#meta{type = zero} | T], Depth, ScopeArgs, Fields, List) ->

    parse_decode_lua_loop(T, Depth, ScopeArgs, Fields, List);

parse_decode_lua_loop([#meta{name = Name, type = binary, comment = Comment, explain = Explain} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", HumpName, " = string.unpack(\"c", integer_to_list(Explain), "\", data, offset)", "\n",
        Padding, "offset = offset + ", integer_to_list(Explain)
    ],
    parse_decode_lua_loop(T, Depth, ScopeArgs, [HumpName | Fields], [Code | List]);

parse_decode_lua_loop([#meta{name = Name, type = bool, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", HumpName, " = string.unpack(\">I1\", data, offset) ~= 0", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_decode_lua_loop(T, Depth, ScopeArgs, [HumpName | Fields], [Code | List]);

parse_decode_lua_loop([#meta{name = Name, type = u8, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", HumpName, " = string.unpack(\">I1\", data, offset)", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_decode_lua_loop(T, Depth, ScopeArgs, [HumpName | Fields], [Code | List]);

parse_decode_lua_loop([#meta{name = Name, type = u16, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", HumpName, " = string.unpack(\">I2\", data, offset)", "\n",
        Padding, "offset = offset + 2"
    ],
    parse_decode_lua_loop(T, Depth, ScopeArgs, [HumpName | Fields], [Code | List]);

parse_decode_lua_loop([#meta{name = Name, type = u32, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", HumpName, " = string.unpack(\">I4\", data, offset)", "\n",
        Padding, "offset = offset + 4"
    ],
    parse_decode_lua_loop(T, Depth, ScopeArgs, [HumpName | Fields], [Code | List]);

parse_decode_lua_loop([#meta{name = Name, type = u64, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", HumpName, " = string.unpack(\">I8\", data, offset)", "\n",
        Padding, "offset = offset + 8"
    ],
    parse_decode_lua_loop(T, Depth, ScopeArgs, [HumpName | Fields], [Code | List]);

parse_decode_lua_loop([#meta{name = Name, type = i8, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", HumpName, " = string.unpack(\">i1\", data, offset)", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_decode_lua_loop(T, Depth, ScopeArgs, [HumpName | Fields], [Code | List]);

parse_decode_lua_loop([#meta{name = Name, type = i16, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", HumpName, " = string.unpack(\">i2\", data, offset)", "\n",
        Padding, "offset = offset + 2"
    ],
    parse_decode_lua_loop(T, Depth, ScopeArgs, [HumpName | Fields], [Code | List]);

parse_decode_lua_loop([#meta{name = Name, type = i32, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", HumpName, " = string.unpack(\">i4\", data, offset)", "\n",
        Padding, "offset = offset + 4"
    ],
    parse_decode_lua_loop(T, Depth, ScopeArgs, [HumpName | Fields], [Code | List]);

parse_decode_lua_loop([#meta{name = Name, type = i64, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", HumpName, " = string.unpack(\">i8\", data, offset)", "\n",
        Padding, "offset = offset + 8"
    ],
    parse_decode_lua_loop(T, Depth, ScopeArgs, [HumpName | Fields], [Code | List]);

parse_decode_lua_loop([#meta{name = Name, type = f32, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", HumpName, " = string.unpack(\">f\", data, offset)", "\n",
        Padding, "offset = offset + 4"
    ],
    parse_decode_lua_loop(T, Depth, ScopeArgs, [HumpName | Fields], [Code | List]);

parse_decode_lua_loop([#meta{name = Name, type = f64, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", HumpName, " = string.unpack(\">d\", data, offset)", "\n",
        Padding, "offset = offset + 8"
    ],
    parse_decode_lua_loop(T, Depth, ScopeArgs, [HumpName | Fields], [Code | List]);

parse_decode_lua_loop([#meta{name = Name, type = str, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", HumpName, " = string.unpack(\">s2\", data, offset)", "\n",
        Padding, "offset = offset + 2 + string.len(", HumpName, ")"
    ],
    parse_decode_lua_loop(T, Depth, ScopeArgs, [HumpName | Fields], [Code | List]);

parse_decode_lua_loop([#meta{name = Name, type = bst, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", HumpName, " = string.unpack(\">s2\", data, offset)", "\n",
        Padding, "offset = offset + 2 + string.len(", HumpName, ")"
    ],
    parse_decode_lua_loop(T, Depth, ScopeArgs, [HumpName | Fields], [Code | List]);

parse_decode_lua_loop([#meta{name = Name, type = rst, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", HumpName, " = string.unpack(\">s2\", data, offset)", "\n",
        Padding, "offset = offset + 2 + string.len(", HumpName, ")"
    ],
    parse_decode_lua_loop(T, Depth, ScopeArgs, [HumpName | Fields], [Code | List]);

parse_decode_lua_loop([#meta{name = _, type = tuple, comment = _, explain = Explain} | T], Depth, ScopeArgs, Fields, List) ->
    %% recursive
    {SubFields, SubCodes} = parse_decode_lua_loop(Explain, Depth, ScopeArgs, Fields, List),

    %% flatten
    parse_decode_lua_loop(T, Depth, ScopeArgs, lists:reverse(SubFields), lists:reverse(SubCodes));

parse_decode_lua_loop([#meta{name = _, type = record, comment = _, explain = Explain} | T], Depth, ScopeArgs, Fields, List) ->
    %% recursive
    {SubFields, SubCodes} = parse_decode_lua_loop(Explain, Depth, ScopeArgs, Fields, List),

    %% flatten
    parse_decode_lua_loop(T, Depth, ScopeArgs, lists:reverse(SubFields), lists:reverse(SubCodes));

parse_decode_lua_loop([#meta{name = Name, type = list, comment = Comment, explain = Explain, key = undefined} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    %% recursive
    {SubFields, SubCodes} = parse_decode_lua_loop(Explain, Depth + 1, HumpName, [], []),
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", HumpName, " = {}", "\n",
        Padding, "local ", HumpName, "Length = string.unpack(\">I2\", data, offset)", "\n",
        Padding, "offset = offset + 2", "\n",
        Padding, "for ", HumpName, "Index = 1, ", HumpName, "Length do", "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "    ", HumpName, "[", HumpName, "Index] = {", string:join([lists:concat([SubName, " = ", SubName]) || SubName <- SubFields], ", "), "}", "\n",
        Padding, "end"
    ],
    parse_decode_lua_loop(T, Depth, ScopeArgs, [HumpName | Fields], [Code | List]);

parse_decode_lua_loop([#meta{name = Name, type = list, comment = Comment, explain = [#meta{explain = SubExplain}], key = Key} | T], Depth, ScopeArgs, Fields, List) ->
    Field = lists:keyfind(Key, #meta.name, SubExplain),
    Field == undefined andalso erlang:throw(lists:flatten(io_lib:format("Cound not found field ~ts in explain", [Key]))),
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    %% recursive
    {SubFields, SubCodes} = parse_decode_lua_loop(SubExplain, Depth + 1, HumpName, [], []),
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", HumpName, " = {}", "\n",
        Padding, "local ", HumpName, "Length = string.unpack(\">I2\", data, offset)", "\n",
        Padding, "offset = offset + 2", "\n",
        Padding, "for ", HumpName, "Index = 1, ", HumpName, "Length do", "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "    ", HumpName, "[", word:to_lower_hump(Key), "] = {", string:join([lists:concat([SubName, " = ", SubName]) || SubName <- SubFields], ", "), "}", "\n",
        Padding, "end"
    ],
    parse_decode_lua_loop(T, Depth, ScopeArgs, [HumpName | Fields], [Code | List]);

parse_decode_lua_loop([#meta{name = Name, type = ets, comment = Comment, explain = Explain, key = undefined} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    %% recursive
    {SubFields, SubCodes} = parse_decode_lua_loop(Explain, Depth + 1, HumpName, [], []),
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", HumpName, " = {}", "\n",
        Padding, "local ", HumpName, "Length = string.unpack(\">I2\", data, offset)", "\n",
        Padding, "offset = offset + 2", "\n",
        Padding, "for ", HumpName, "Index = 1, ", HumpName, "Length do", "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "    ", HumpName, "[", HumpName, "Index] = {", string:join([lists:concat([SubName, " = ", SubName]) || SubName <- SubFields], ", "), "}", "\n",
        Padding, "end"
    ],
    parse_decode_lua_loop(T, Depth, ScopeArgs, [HumpName | Fields], [Code | List]);

parse_decode_lua_loop([#meta{name = Name, type = ets, comment = Comment, explain = [#meta{explain = SubExplain}], key = Key} | T], Depth, ScopeArgs, Fields, List) ->
    Field = lists:keyfind(Key, #meta.name, SubExplain),
    Field == undefined andalso erlang:throw(lists:flatten(io_lib:format("Cound not found field ~ts in explain", [Key]))),
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    %% recursive
    {SubFields, SubCodes} = parse_decode_lua_loop(SubExplain, Depth + 1, HumpName, [], []),
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", HumpName, " = {}", "\n",
        Padding, "local ", HumpName, "Length = string.unpack(\">I2\", data, offset)", "\n",
        Padding, "offset = offset + 2", "\n",
        Padding, "for ", HumpName, "Index = 1, ", HumpName, "Length do", "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "    ", HumpName, "[", word:to_lower_hump(Key), "] = {", string:join([lists:concat([SubName, " = ", SubName]) || SubName <- SubFields], ", "), "}", "\n",
        Padding, "end"
    ],
    parse_decode_lua_loop(T, Depth, ScopeArgs, [HumpName | Fields], [Code | List]).
