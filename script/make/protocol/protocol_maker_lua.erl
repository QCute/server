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
        LuaName, " = {}", "\n",
        "\n"
        "function ", LuaName, ".encode(offset, protocol, data)", "\n",
        "    ", string:join(Encode, ""), "\n",
        "    ", "    ", "error(string.format('unknown protocol define: %d', protocol))", "\n"
        "    ", "end", "\n",
        "end", "\n\n",
        "function ", LuaName, ".decode(offset, protocol, data)", "\n",
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
parse_meta_lua(_, Meta) ->
    %% start with 3 tabs(4 space) padding
    Padding = lists:duplicate(2, "    "),
    MetaData = parse_meta_lua_loop([Meta], 3, []),
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

parse_meta_lua_loop([#meta{name = Name, type = Type, explain = undefined, comment = Comment} | T], Depth, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    Code = lists:flatten(io_lib:format("~s{name = \"~s\", type = \"~s\", comment = \"~ts\", explain = ~w}", [Padding, word:to_lower_hump(Name), Type, Comment, {}])),
    parse_meta_lua_loop(T, Depth, [Code | List]);

parse_meta_lua_loop([#meta{name = Name, type = Type = tuple, explain = Explain, comment = Comment} | T], Depth, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),

    %% recursive
    SubCodes = parse_meta_lua_loop(tuple_to_list(Explain), Depth + 1, []),

    %% format one field
    Code = lists:flatten(io_lib:format("~s{name = \"~s\", type = \"~s\", comment = \"~ts\", explain = {\n~ts\n~s}}", [Padding, word:to_lower_hump(Name), Type, Comment, SubCodes, Padding])),

    parse_meta_lua_loop(T, Depth, [Code | List]);

parse_meta_lua_loop([#meta{name = Name, type = Type = record, explain = Explain, comment = Comment} | T], Depth, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),

    %% recursive
    SubCodes = parse_meta_lua_loop(tuple_to_list(Explain), Depth + 1, []),

    %% format one field
    Code = lists:flatten(io_lib:format("~s{name = \"~s\", type = \"~s\", comment = \"~ts\", explain = {\n~ts\n~s}}", [Padding, word:to_lower_hump(Name), Type, Comment, SubCodes, Padding])),

    parse_meta_lua_loop(T, Depth, [Code | List]);

parse_meta_lua_loop([#meta{name = Name, type = Type = maps, explain = Explain, comment = Comment} | T], Depth, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),

    %% recursive
    SubCodes = parse_meta_lua_loop(maps:values(Explain), Depth + 1, []),

    %% format one field
    Code = lists:flatten(io_lib:format("~s{name = \"~s\", type = \"~s\", comment = \"~ts\", explain = {\n~ts\n~s}}", [Padding, word:to_lower_hump(Name), Type, Comment, SubCodes, Padding])),

    parse_meta_lua_loop(T, Depth, [Code | List]);

parse_meta_lua_loop([#meta{name = Name, type = list, explain = Explain = [#meta{type = SubType} | _], comment = Comment, key = undefined} | T], Depth, List) ->
    %% recursive
    SubCodes = parse_meta_lua_loop(Explain, Depth + 1, []),
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    TargetLeft = ["{" || SubType == tuple orelse SubType == record orelse SubType == maps],
    TargetRight = ["}" || SubType == tuple orelse SubType == record orelse SubType == maps],
    Code = lists:flatten(io_lib:format("~s{name = \"~s\", type = \"~s\", comment = \"~ts\", explain = ~ts\n~ts\n~s~ts}", [Padding, word:to_lower_hump(Name), list, Comment, TargetLeft, SubCodes, Padding, TargetRight])),
    parse_meta_lua_loop(T, Depth, [Code | List]);

parse_meta_lua_loop([#meta{name = Name, type = list, explain = Explain = [#meta{type = SubType} | _], comment = Comment, key = Key} | T], Depth, List) ->
    %% recursive
    SubCodes = parse_meta_lua_loop(Explain, Depth + 1, []),
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    TargetLeft = ["{" || SubType == tuple orelse SubType == record orelse SubType == maps],
    TargetRight = ["}" || SubType == tuple orelse SubType == record orelse SubType == maps],
    Code = lists:flatten(io_lib:format("~s{name = \"~s\", type = \"~s\", comment = \"~ts\", key = \"~ts\", explain = ~ts\n~ts\n~s~ts}", [Padding, word:to_lower_hump(Name), map, Comment, word:to_lower_hump(Key), TargetLeft, SubCodes, Padding, TargetRight])),
    parse_meta_lua_loop(T, Depth, [Code | List]);

parse_meta_lua_loop([#meta{name = Name, type = ets, explain = Explain = [#meta{type = SubType} | _], comment = Comment, key = undefined} | T], Depth, List) ->
    %% recursive
    SubCodes = parse_meta_lua_loop(Explain, Depth + 1, []),
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    TargetLeft = ["{" || SubType == tuple orelse SubType == record orelse SubType == maps],
    TargetRight = ["}" || SubType == tuple orelse SubType == record orelse SubType == maps],
    Code = lists:flatten(io_lib:format("~s{name = \"~s\", type = \"~s\", comment = \"~ts\", explain = ~ts\n~ts\n~s~ts}", [Padding, word:to_lower_hump(Name), list, Comment, TargetLeft, SubCodes, Padding, TargetRight])),
    parse_meta_lua_loop(T, Depth, [Code | List]);

parse_meta_lua_loop([#meta{name = Name, type = ets, explain = Explain = [#meta{type = SubType} | _], comment = Comment, key = Key} | T], Depth, List) ->
    %% recursive
    SubCodes = parse_meta_lua_loop(Explain, Depth + 1, []),
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    TargetLeft = ["{" || SubType == tuple orelse SubType == record orelse SubType == maps],
    TargetRight = ["}" || SubType == tuple orelse SubType == record orelse SubType == maps],
    Code = lists:flatten(io_lib:format("~s{name = \"~s\", type = \"~s\", comment = \"~ts\", key = \"~ts\", explain = ~ts\n~ts\n~s~ts}", [Padding, word:to_lower_hump(Name), map, Comment, word:to_lower_hump(Key), TargetLeft, SubCodes, Padding, TargetRight])),
    parse_meta_lua_loop(T, Depth, [Code | List]).

%%%===================================================================
%%% encode
%%%===================================================================
%% lua code
parse_encode_lua(Protocol, Meta = #meta{name = Name, type = Type}) ->
    %% start with 3 tabs(4 space) padding
    Padding = lists:duplicate(1, "    "),

    NewName = maps:get(Name == [] andalso ?IS_UNIT(Type), #{true => "data", false => Name}),
    {_, Codes} = parse_encode_lua_loop([Meta#meta{name = NewName}], 2, Type, "data", [], []),

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
parse_encode_lua_loop([], _, _, _, Fields, List) ->
    %% construct as a list
    %% {string:join(lists:reverse(Fields), ", "), string:join(lists:reverse(List), "\n")};
    {lists:reverse(Fields), lists:reverse(List)};

parse_encode_lua_loop([#meta{name = Name, type = binary, comment = Comment} | T], Depth, Parent, Scope, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Target = [["[\"", HumpName, "\"]"] || Parent == tuple orelse Parent == record orelse Parent == maps],
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = ", Scope, Target, "\n",
        Padding, "offset = offset + 1"
    ],
    parse_encode_lua_loop(T, Depth, Parent, Scope, [HumpName | Fields], [Code | List]);

parse_encode_lua_loop([#meta{name = Name, type = bool, explain = undefined, comment = Comment} | T], Depth, Parent, Scope, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Target = [["[\"", HumpName, "\"]"] || Parent == tuple orelse Parent == record orelse Parent == maps],
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = string.pack(\">I1\", ", Scope, Target, " ~= 0 and 1 or 0)", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_encode_lua_loop(T, Depth, Parent, Scope, [HumpName | Fields], [Code | List]);

parse_encode_lua_loop([#meta{name = Name, type = u8, explain = undefined, comment = Comment} | T], Depth, Parent, Scope, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Target = [["[\"", HumpName, "\"]"] || Parent == tuple orelse Parent == record orelse Parent == maps],
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = string.pack(\">I1\", ", Scope, Target, ")", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_encode_lua_loop(T, Depth, Parent, Scope, [HumpName | Fields], [Code | List]);

parse_encode_lua_loop([#meta{name = Name, type = u16, explain = undefined, comment = Comment} | T], Depth, Parent, Scope, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Target = [["[\"", HumpName, "\"]"] || Parent == tuple orelse Parent == record orelse Parent == maps],
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = string.pack(\">I2\", ", Scope, Target, ")", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_encode_lua_loop(T, Depth, Parent, Scope, [HumpName | Fields], [Code | List]);

parse_encode_lua_loop([#meta{name = Name, type = u32, explain = undefined, comment = Comment} | T], Depth, Parent, Scope, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Target = [["[\"", HumpName, "\"]"] || Parent == tuple orelse Parent == record orelse Parent == maps],
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = string.pack(\">I4\", ", Scope, Target, ")", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_encode_lua_loop(T, Depth, Parent, Scope, [HumpName | Fields], [Code | List]);

parse_encode_lua_loop([#meta{name = Name, type = u64, explain = undefined, comment = Comment} | T], Depth, Parent, Scope, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Target = [["[\"", HumpName, "\"]"] || Parent == tuple orelse Parent == record orelse Parent == maps],
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = string.pack(\">I8\", ", Scope, Target, ")", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_encode_lua_loop(T, Depth, Parent, Scope, [HumpName | Fields], [Code | List]);

parse_encode_lua_loop([#meta{name = Name, type = i8, explain = undefined, comment = Comment} | T], Depth, Parent, Scope, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Target = [["[\"", HumpName, "\"]"] || Parent == tuple orelse Parent == record orelse Parent == maps],
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = string.pack(\">i1\", ", Scope, Target, ")", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_encode_lua_loop(T, Depth, Parent, Scope, [HumpName | Fields], [Code | List]);

parse_encode_lua_loop([#meta{name = Name, type = i16, explain = undefined, comment = Comment} | T], Depth, Parent, Scope, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Target = [["[\"", HumpName, "\"]"] || Parent == tuple orelse Parent == record orelse Parent == maps],
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = string.pack(\">i2\", ", Scope, Target, ")", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_encode_lua_loop(T, Depth, Parent, Scope, [HumpName | Fields], [Code | List]);

parse_encode_lua_loop([#meta{name = Name, type = i32, explain = undefined, comment = Comment} | T], Depth, Parent, Scope, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Target = [["[\"", HumpName, "\"]"] || Parent == tuple orelse Parent == record orelse Parent == maps],
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = string.pack(\">i4\", ", Scope, Target, ")", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_encode_lua_loop(T, Depth, Parent, Scope, [HumpName | Fields], [Code | List]);

parse_encode_lua_loop([#meta{name = Name, type = i64, explain = undefined, comment = Comment} | T], Depth, Parent, Scope, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Target = [["[\"", HumpName, "\"]"] || Parent == tuple orelse Parent == record orelse Parent == maps],
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = string.pack(\">i8\", ", Scope, Target, ")", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_encode_lua_loop(T, Depth, Parent, Scope, [HumpName | Fields], [Code | List]);

parse_encode_lua_loop([#meta{name = Name, type = f32, explain = undefined, comment = Comment} | T], Depth, Parent, Scope, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Target = [["[\"", HumpName, "\"]"] || Parent == tuple orelse Parent == record orelse Parent == maps],
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = string.pack(\">f\", ", Scope, Target, ")", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_encode_lua_loop(T, Depth, Parent, Scope, [HumpName | Fields], [Code | List]);

parse_encode_lua_loop([#meta{name = Name, type = f64, explain = undefined, comment = Comment} | T], Depth, Parent, Scope, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Target = [["[\"", HumpName, "\"]"] || Parent == tuple orelse Parent == record orelse Parent == maps],
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = string.pack(\">d\", ", Scope, Target, ")", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_encode_lua_loop(T, Depth, Parent, Scope, [HumpName | Fields], [Code | List]);

parse_encode_lua_loop([#meta{name = Name, type = str, explain = undefined, comment = Comment} | T], Depth, Parent, Scope, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Target = [["[\"", HumpName, "\"]"] || Parent == tuple orelse Parent == record orelse Parent == maps],
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = string.pack(\">s2\", ", Scope, Target, ")", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_encode_lua_loop(T, Depth, Parent, Scope, [HumpName | Fields], [Code | List]);

parse_encode_lua_loop([#meta{name = Name, type = bst, explain = undefined, comment = Comment} | T], Depth, Parent, Scope, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Target = [["[\"", HumpName, "\"]"] || Parent == tuple orelse Parent == record orelse Parent == maps],
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = string.pack(\">s2\", ", Scope, Target, ")", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_encode_lua_loop(T, Depth, Parent, Scope, [HumpName | Fields], [Code | List]);

parse_encode_lua_loop([#meta{name = Name, type = rst, explain = undefined, comment = Comment} | T], Depth, Parent, Scope, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Target = [["[\"", HumpName, "\"]"] || Parent == tuple orelse Parent == record orelse Parent == maps],
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = string.pack(\">s2\", ", Scope, Target, ")", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_encode_lua_loop(T, Depth, Parent, Scope, [HumpName | Fields], [Code | List]);

parse_encode_lua_loop([#meta{name = Name, type = tuple, explain = Explain} | T], Depth, Parent, Scope, Names, List) ->

    HumpName = word:to_lower_hump(Name),
    Target = [["[\"", HumpName, "\"]"] || Parent == tuple orelse Parent == record orelse Parent == maps],

    %% recursive
    {SubNames, SubCodes} = parse_encode_lua_loop(tuple_to_list(Explain), Depth, tuple, lists:concat([Scope, Target]), Names, List),

    %% flatten
    parse_encode_lua_loop(T, Depth, Parent, Scope, lists:reverse(SubNames), lists:reverse(SubCodes));

parse_encode_lua_loop([#meta{name = Name, type = record, explain = Explain} | T], Depth, Parent, Scope, Names, List) ->

    HumpName = word:to_lower_hump(Name),
    Target = [["[\"", HumpName, "\"]"] || Parent == tuple orelse Parent == record orelse Parent == maps],

    SubExplain = [Meta || Meta = #meta{} <- tuple_to_list(Explain)],

    %% recursive
    {SubNames, SubCodes} = parse_encode_lua_loop(SubExplain, Depth, record, lists:concat([Scope, Target]), Names, List),

    %% flatten
    parse_encode_lua_loop(T, Depth, Parent, Scope, lists:reverse(SubNames), lists:reverse(SubCodes));

parse_encode_lua_loop([#meta{name = Name, type = maps, explain = Explain} | T], Depth, Parent, Scope, Names, List) ->

    HumpName = word:to_lower_hump(Name),
    Target = [["[\"", HumpName, "\"]"] || Parent == tuple orelse Parent == record orelse Parent == maps],

    %% recursive
    {SubNames, SubCodes} = parse_encode_lua_loop(maps:values(Explain), Depth, maps, lists:concat([Scope, Target]), Names, List),

    %% flatten
    parse_encode_lua_loop(T, Depth, Parent, Scope, lists:reverse(SubNames), lists:reverse(SubCodes));

parse_encode_lua_loop([#meta{name = Name, type = list, explain = Explain, comment = Comment, key = undefined} | T], Depth, Parent, Scope, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Target = [["[\"", HumpName, "\"]"] || Parent == tuple orelse Parent == record orelse Parent == maps],
    SubExplain = [Meta#meta{name = maps:get(SubName, #{[] => lists:concat([HumpName, "Item"])}, SubName)} || Meta = #meta{name = SubName} <- Explain],
    %% recursive
    {_, SubCodes} = parse_encode_lua_loop(SubExplain, Depth + 1, list, lists:concat([HumpName, "ItemData"]), [], []),
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", HumpName, "Table = ", Scope, Target, "", "\n",
        Padding, "table[offset] = string.pack(\">I2\", #", HumpName, "Table)", "\n",
        Padding, "offset = offset + 1", "\n",
        Padding, "for ", HumpName, "Index = 1, #", HumpName, "Table do", "\n",
        Padding, "    ", "local ", HumpName, "ItemData = ", HumpName, "Table[", HumpName, "Index]", "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "end"
    ],
    parse_encode_lua_loop(T, Depth, Parent, Scope, [HumpName | Fields], [Code | List]);

parse_encode_lua_loop([#meta{name = Name, type = list, explain = Explain, comment = Comment, key = _} | T], Depth, Parent, Scope, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Target = [["[\"", HumpName, "\"]"] || Parent == tuple orelse Parent == record orelse Parent == maps],
    SubExplain = [Meta#meta{name = maps:get(SubName, #{[] => lists:concat([HumpName, "Item"])}, SubName)} || Meta = #meta{name = SubName} <- Explain],
    %% recursive
    {_, SubCodes} = parse_encode_lua_loop(SubExplain, Depth + 1, list, lists:concat([HumpName, "ItemData"]), [], []),
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", HumpName, "Table = ", Scope, Target, "", "\n",
        Padding, "table[offset] = string.pack(\">I2\", #", HumpName, "Table)", "\n",
        Padding, "offset = offset + 1", "\n",
        Padding, "for _, ", HumpName, "ItemData in pairs(", HumpName, "Table) do", "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "end"
    ],
    parse_encode_lua_loop(T, Depth, Parent, Scope, [HumpName | Fields], [Code | List]).

%%%===================================================================
%%% decode
%%%===================================================================
%% lua code
parse_decode_lua(Protocol, Meta = #meta{name = Name, type = Type}) ->
    %% start with 3 tabs(4 space) padding
    Padding = lists:duplicate(1, "    "),

    NewName = maps:get(Name == [] andalso ?IS_UNIT(Type), #{true => "data", false => Name}),
    {Fields, Codes} = parse_decode_lua_loop([Meta#meta{name = NewName}], 2, [], []),

    %% codes format
    CodesDefault = lists:concat([
        Padding, "    ", "return ", string:join(Fields, ", "), "", "\n"
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
parse_decode_lua_loop([], _, Fields, List) ->
    %% construct as a list
    %% {string:join([lists:concat([Name, " = ", Name]) || Name <- lists:reverse(Fields)], ", "), string:join(lists:reverse(List), "\n")};
    {lists:reverse(Fields), lists:reverse(List)};

parse_decode_lua_loop([#meta{type = zero} | T], Depth, Fields, List) ->

    parse_decode_lua_loop(T, Depth, Fields, List);

parse_decode_lua_loop([#meta{name = Name, type = binary, explain = Explain, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", HumpName, " = string.unpack(\"c", integer_to_list(Explain), "\", data, offset)", "\n",
        Padding, "offset = offset + ", integer_to_list(Explain)
    ],
    parse_decode_lua_loop(T, Depth, [HumpName | Fields], [Code | List]);

parse_decode_lua_loop([#meta{name = Name, type = bool, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", HumpName, " = string.unpack(\">I1\", data, offset) ~= 0", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_decode_lua_loop(T, Depth, [HumpName | Fields], [Code | List]);

parse_decode_lua_loop([#meta{name = Name, type = u8, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", HumpName, " = string.unpack(\">I1\", data, offset)", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_decode_lua_loop(T, Depth, [HumpName | Fields], [Code | List]);

parse_decode_lua_loop([#meta{name = Name, type = u16, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", HumpName, " = string.unpack(\">I2\", data, offset)", "\n",
        Padding, "offset = offset + 2"
    ],
    parse_decode_lua_loop(T, Depth, [HumpName | Fields], [Code | List]);

parse_decode_lua_loop([#meta{name = Name, type = u32, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", HumpName, " = string.unpack(\">I4\", data, offset)", "\n",
        Padding, "offset = offset + 4"
    ],
    parse_decode_lua_loop(T, Depth, [HumpName | Fields], [Code | List]);

parse_decode_lua_loop([#meta{name = Name, type = u64, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", HumpName, " = string.unpack(\">I8\", data, offset)", "\n",
        Padding, "offset = offset + 8"
    ],
    parse_decode_lua_loop(T, Depth, [HumpName | Fields], [Code | List]);

parse_decode_lua_loop([#meta{name = Name, type = i8, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", HumpName, " = string.unpack(\">i1\", data, offset)", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_decode_lua_loop(T, Depth, [HumpName | Fields], [Code | List]);

parse_decode_lua_loop([#meta{name = Name, type = i16, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", HumpName, " = string.unpack(\">i2\", data, offset)", "\n",
        Padding, "offset = offset + 2"
    ],
    parse_decode_lua_loop(T, Depth, [HumpName | Fields], [Code | List]);

parse_decode_lua_loop([#meta{name = Name, type = i32, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", HumpName, " = string.unpack(\">i4\", data, offset)", "\n",
        Padding, "offset = offset + 4"
    ],
    parse_decode_lua_loop(T, Depth, [HumpName | Fields], [Code | List]);

parse_decode_lua_loop([#meta{name = Name, type = i64, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", HumpName, " = string.unpack(\">i8\", data, offset)", "\n",
        Padding, "offset = offset + 8"
    ],
    parse_decode_lua_loop(T, Depth, [HumpName | Fields], [Code | List]);

parse_decode_lua_loop([#meta{name = Name, type = f32, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", HumpName, " = string.unpack(\">f\", data, offset)", "\n",
        Padding, "offset = offset + 4"
    ],
    parse_decode_lua_loop(T, Depth, [HumpName | Fields], [Code | List]);

parse_decode_lua_loop([#meta{name = Name, type = f64, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", HumpName, " = string.unpack(\">d\", data, offset)", "\n",
        Padding, "offset = offset + 8"
    ],
    parse_decode_lua_loop(T, Depth, [HumpName | Fields], [Code | List]);

parse_decode_lua_loop([#meta{name = Name, type = str, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", HumpName, " = string.unpack(\">s2\", data, offset)", "\n",
        Padding, "offset = offset + 2 + string.len(", HumpName, ")"
    ],
    parse_decode_lua_loop(T, Depth, [HumpName | Fields], [Code | List]);

parse_decode_lua_loop([#meta{name = Name, type = bst, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", HumpName, " = string.unpack(\">s2\", data, offset)", "\n",
        Padding, "offset = offset + 2 + string.len(", HumpName, ")"
    ],
    parse_decode_lua_loop(T, Depth, [HumpName | Fields], [Code | List]);

parse_decode_lua_loop([#meta{name = Name, type = rst, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", HumpName, " = string.unpack(\">s2\", data, offset)", "\n",
        Padding, "offset = offset + 2 + string.len(", HumpName, ")"
    ],
    parse_decode_lua_loop(T, Depth, [HumpName | Fields], [Code | List]);

parse_decode_lua_loop([#meta{name = Name, type = tuple, explain = Explain, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),

    %% recursive
    {SubFields, SubCodes} = parse_decode_lua_loop(tuple_to_list(Explain), Depth, [], []),

    Code = [
        Padding, "-- ", Comment, "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "-- object", "\n",
        Padding, "local ", HumpName, " = {", string:join([lists:concat([SubName, " = ", SubName]) || SubName <- SubFields], ", "), "}"
    ],

    %% flatten
    parse_decode_lua_loop(T, Depth, [HumpName | Fields], [Code | List]);

parse_decode_lua_loop([#meta{name = Name, type = record, explain = Explain, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),

    SubExplain = [Meta || Meta = #meta{} <- tuple_to_list(Explain)],

    %% recursive
    {SubFields, SubCodes} = parse_decode_lua_loop(SubExplain, Depth, [], []),

    Code = [
        Padding, "-- ", Comment, "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "-- object", "\n",
        Padding, "local ", HumpName, " = {", string:join([lists:concat([SubName, " = ", SubName]) || SubName <- SubFields], ", "), "}"
    ],

    %% flatten
    parse_decode_lua_loop(T, Depth, [HumpName | Fields], [Code | List]);

parse_decode_lua_loop([#meta{name = Name, type = maps, explain = Explain, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),

    %% recursive
    {SubFields, SubCodes} = parse_decode_lua_loop(maps:values(Explain), Depth, [], []),

    Code = [
        Padding, "-- ", Comment, "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "-- object", "\n",
        Padding, "local ", HumpName, " = {", string:join([lists:concat([SubName, " = ", SubName]) || SubName <- SubFields], ", "), "}"
    ],

    %% flatten
    parse_decode_lua_loop(T, Depth, [HumpName | Fields], [Code | List]);

parse_decode_lua_loop([#meta{name = Name, type = list, explain = Explain, comment = Comment, key = undefined} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    SubExplain = [Meta#meta{name = maps:get(SubName, #{[] => lists:concat([HumpName, "Item"])}, SubName)} || Meta = #meta{name = SubName} <- Explain],
    %% recursive
    {SubFields, SubCodes} = parse_decode_lua_loop(SubExplain, Depth + 1, [], []),
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", HumpName, " = {}", "\n",
        Padding, "local ", HumpName, "Length = string.unpack(\">I2\", data, offset)", "\n",
        Padding, "offset = offset + 2", "\n",
        Padding, "for ", HumpName, "Index = 1, ", HumpName, "Length do", "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "    ", HumpName, "[", HumpName, "Index] = ", string:join(SubFields, ", "), "\n",
        Padding, "end"
    ],
    parse_decode_lua_loop(T, Depth, [HumpName | Fields], [Code | List]);

parse_decode_lua_loop([#meta{name = Name, type = list, explain = Explain, comment = Comment, key = Key} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    SubExplain = [Meta#meta{name = maps:get(SubName, #{[] => lists:concat([HumpName, "Item"])}, SubName)} || Meta = #meta{name = SubName} <- Explain],
    %% recursive
    {SubFields, SubCodes} = parse_decode_lua_loop(SubExplain, Depth + 1, [], []),
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", HumpName, " = {}", "\n",
        Padding, "local ", HumpName, "Length = string.unpack(\">I2\", data, offset)", "\n",
        Padding, "offset = offset + 2", "\n",
        Padding, "for ", HumpName, "Index = 1, ", HumpName, "Length do", "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "    ", HumpName, "[", word:to_lower_hump(Key), "] = ", string:join(SubFields, ", "), "\n",
        Padding, "end"
    ],
    parse_decode_lua_loop(T, Depth, [HumpName | Fields], [Code | List]);

parse_decode_lua_loop([#meta{name = Name, type = ets, explain = Explain, comment = Comment, key = []} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    SubExplain = [Meta#meta{name = maps:get(SubName, #{[] => lists:concat([HumpName, "Item"])}, SubName)} || Meta = #meta{name = SubName} <- Explain],
    %% recursive
    {SubFields, SubCodes} = parse_decode_lua_loop(SubExplain, Depth + 1, [], []),
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", HumpName, " = {}", "\n",
        Padding, "local ", HumpName, "Length = string.unpack(\">I2\", data, offset)", "\n",
        Padding, "offset = offset + 2", "\n",
        Padding, "for ", HumpName, "Index = 1, ", HumpName, "Length do", "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "    ", HumpName, "[", HumpName, "Index] = ", string:join(SubFields, ", "), "\n",
        Padding, "end"
    ],
    parse_decode_lua_loop(T, Depth, [HumpName | Fields], [Code | List]);

parse_decode_lua_loop([#meta{name = Name, type = ets, explain = Explain, comment = Comment, key = [Key]} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    SubExplain = [Meta#meta{name = maps:get(SubName, #{[] => lists:concat([HumpName, "Item"])}, SubName)} || Meta = #meta{name = SubName} <- Explain],
    %% recursive
    {SubFields, SubCodes} = parse_decode_lua_loop(SubExplain, Depth + 1, [], []),
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", HumpName, " = {}", "\n",
        Padding, "local ", HumpName, "Length = string.unpack(\">I2\", data, offset)", "\n",
        Padding, "offset = offset + 2", "\n",
        Padding, "for ", HumpName, "Index = 1, ", HumpName, "Length do", "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "    ", HumpName, "[", word:to_lower_hump(Key), "] = ", string:join(SubFields, ", "), "\n",
        Padding, "end"
    ],
    parse_decode_lua_loop(T, Depth, [HumpName | Fields], [Code | List]).
