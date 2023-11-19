%%%-------------------------------------------------------------------
%%% @doc
%%% make protocol define to lua io/metadata code
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_maker_lua).
-export([format_code/3, format_meta/2]).
-export([parse_meta/2]).
-export([parse_encode_class/4, parse_decode_class/4]).
-export([parse_encode/2, parse_decode/2]).
-include("../../../include/serialize.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
format_code(Name, EncodeList, DecodeList) ->
    %% export
    ImportExport = [[Import, "\n", "\n", Export] || {#data{lua = #set{code = #file{import = Import}}}, #data{lua = #set{code = #file{export = Export}}}} <- lists:zip(EncodeList, DecodeList), Import =/= [] orelse Export =/= []],

    %% encode
    Encode = [Encode || #data{lua = #set{code = #file{function = Encode}}} <- EncodeList, Encode =/= []],
    EncodeDefault = lists:concat([

    ]),
    EncodeCode = lists:append(Encode, [EncodeDefault]),

    %% decode
    Decode = [Decode || #data{lua = #set{code = #file{function = Decode}}} <- DecodeList, Decode =/= []],
    DecodeDefault = lists:concat([

    ]),
    DecodeCode = lists:append(Decode, [DecodeDefault]),

    lists:concat([
        string:join(ImportExport, "\n\n"),
        "\n",
        "\n",
        Name, " = {}", "\n",
        "\n"
        "function ", Name, ".encode(offset, protocol, data)", "\n",
        "    ", string:join(EncodeCode, ""), "\n",
        "    ", "    ", "error(string.format('unknown protocol define: %d', protocol))", "\n"
        "    ", "end", "\n",
        "end", "\n\n",
        "function ", Name, ".decode(offset, protocol, bytes)", "\n",
        "    ", string:join(DecodeCode, ""), "\n",
        "    ", "    ", "error(string.format('unknown protocol define: %d', protocol))", "\n"
        "    ", "end", "\n",
        "end"
    ]).

format_meta(_, List) ->
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
parse_meta(_, Meta) ->
    %% start with 1 tabs(4 space) padding
    %% Padding = lists:duplicate(1, "    "),
    Code = parse_meta_loop([Meta], 2, []),
    %% format one protocol define
    %% lists:concat(["        [", Protocol, "] = {\n", MetaData, "\n        }"]).

    #file{extra = string:trim(Code)}.

parse_meta_loop([], _, List) ->
    %% construct as a list
    string:join(lists:reverse(List), ",\n");

parse_meta_loop([#meta{name = _, type = zero} | T], Depth, List) ->
    parse_meta_loop(T, Depth, List);

parse_meta_loop([#meta{name = Name, type = binary, explain = Length, comment = Comment} | T], Depth, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    Code = lists:flatten(io_lib:format("~s{name = \"~s\", type = \"~s\", comment = \"~ts\", explain = ~w}", [Padding, word:to_lower_hump(Name), binary, Comment, Length])),
    parse_meta_loop(T, Depth, [Code | List]);

parse_meta_loop([#meta{name = Name, type = Type, explain = undefined, comment = Comment} | T], Depth, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    Code = lists:flatten(io_lib:format("~s{name = \"~s\", type = \"~s\", comment = \"~ts\", explain = ~w}", [Padding, word:to_lower_hump(Name), Type, Comment, {}])),
    parse_meta_loop(T, Depth, [Code | List]);

parse_meta_loop([#meta{name = Name, type = tuple, explain = Explain, comment = Comment} | T], Depth, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),

    %% recursive
    SubCodes = parse_meta_loop(tuple_to_list(Explain), Depth + 1, []),

    %% format one field
    Code = lists:flatten(io_lib:format("~s{name = \"~s\", type = \"~s\", comment = \"~ts\", explain = {\n~ts\n~s}}", [Padding, word:to_lower_hump(Name), map, Comment, SubCodes, Padding])),

    parse_meta_loop(T, Depth, [Code | List]);

parse_meta_loop([#meta{name = Name, type = record, explain = Explain, comment = Comment} | T], Depth, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),

    SubExplain = [Meta || Meta = #meta{} <- tuple_to_list(Explain)],

    %% recursive
    SubCodes = parse_meta_loop(SubExplain, Depth + 1, []),

    %% format one field
    Code = lists:flatten(io_lib:format("~s{name = \"~s\", type = \"~s\", comment = \"~ts\", explain = {\n~ts\n~s}}", [Padding, word:to_lower_hump(Name), map, Comment, SubCodes, Padding])),

    parse_meta_loop(T, Depth, [Code | List]);

parse_meta_loop([#meta{name = Name, type = maps, explain = Explain, comment = Comment} | T], Depth, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),

    %% recursive
    SubCodes = parse_meta_loop(maps:values(Explain), Depth + 1, []),

    %% format one field
    Code = lists:flatten(io_lib:format("~s{name = \"~s\", type = \"~s\", comment = \"~ts\", explain = {\n~ts\n~s}}", [Padding, word:to_lower_hump(Name), map, Comment, SubCodes, Padding])),

    parse_meta_loop(T, Depth, [Code | List]);

parse_meta_loop([#meta{name = Name, type = list, explain = Explain, comment = Comment, key = undefined} | T], Depth, List) ->
    %% recursive
    SubCodes = parse_meta_loop(Explain, Depth + 1, []),
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    Code = lists:flatten(io_lib:format("~s{name = \"~s\", type = \"~s\", comment = \"~ts\", explain = {\n~ts\n~s}}", [Padding, word:to_lower_hump(Name), list, Comment, SubCodes, Padding])),
    parse_meta_loop(T, Depth, [Code | List]);

parse_meta_loop([#meta{name = Name, type = list, explain = Explain, comment = Comment, key = Key} | T], Depth, List) ->
    %% recursive
    SubCodes = parse_meta_loop(Explain, Depth + 1, []),
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    Code = lists:flatten(io_lib:format("~s{name = \"~s\", type = \"~s\", comment = \"~ts\", key = \"~ts\", explain = {\n~ts\n~s}}", [Padding, word:to_lower_hump(Name), list, Comment, word:to_lower_hump(Key), SubCodes, Padding])),
    parse_meta_loop(T, Depth, [Code | List]);

parse_meta_loop([#meta{name = Name, type = ets, explain = Explain, comment = Comment, key = undefined} | T], Depth, List) ->
    %% recursive
    SubCodes = parse_meta_loop(Explain, Depth + 1, []),
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    Code = lists:flatten(io_lib:format("~s{name = \"~s\", type = \"~s\", comment = \"~ts\", explain = {\n~ts\n~s}}", [Padding, word:to_lower_hump(Name), list, Comment, SubCodes, Padding])),
    parse_meta_loop(T, Depth, [Code | List]);

parse_meta_loop([#meta{name = Name, type = ets, explain = Explain, comment = Comment, key = Key} | T], Depth, List) ->
    %% recursive
    SubCodes = parse_meta_loop(Explain, Depth + 1, []),
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    Code = lists:flatten(io_lib:format("~s{name = \"~s\", type = \"~s\", comment = \"~ts\", key = \"~ts\", explain = {\n~ts\n~s}}", [Padding, word:to_lower_hump(Name), list, Comment, word:to_lower_hump(Key), SubCodes, Padding])),
    parse_meta_loop(T, Depth, [Code | List]).

%%%===================================================================
%%% encode class
%%%===================================================================
%% code
parse_encode_class(_, #meta{}, undefined, _) ->
    #file{};
parse_encode_class(Protocol, Meta = #meta{}, #handler{module = Module, function = Function, alias = Alias}, Name) ->

    {_, Codes} = parse_class_loop([Meta], 1, [], []),

    case Alias of
        true ->
            Sub = Module;
        false ->
            Sub = Function;
        _ ->
            Sub = Alias
    end,

    %% format one protocol define
    Code = lists:concat([
        "--- @class ", string:replace(word:to_hump(Name), "Protocol", ""), word:to_hump(Sub), "Request", "\n",
        "--- @field protocol number ", Protocol, "\n",
        "--- @field ", "data", " ", string:join(Codes, "")
    ]),

    #file{import = Code}.


%% code
parse_decode_class(_, #meta{}, undefined, _) ->
    #file{};
parse_decode_class(Protocol, Meta, #handler{module = Module, function = Function, alias = Alias}, Name) ->

    {_, Codes} = parse_class_loop([Meta], 1, [], []),

    case Alias of
        true ->
            Sub = Module;
        false ->
            Sub = Function;
        _ ->
            Sub = Alias
    end,

    %% format one protocol define
    Code = lists:concat([
        "--- @class ", string:replace(word:to_hump(Name), "Protocol", ""), word:to_hump(Sub), "Request", "\n",
        "--- @field protocol number ", Protocol, "\n",
        "--- @field ", "data", " ", string:join(Codes, "")
    ]),

    #file{export = Code}.


parse_class_loop([], _, Fields, List) ->
    %% construct as a list
    {lists:reverse(Fields), lists:reverse(List)};

parse_class_loop([#meta{type = zero} | T], Depth, Fields, List) ->

    parse_class_loop(T, Depth, Fields, List);

parse_class_loop([Meta = #meta{type = binary} | T], Depth, Fields, List) ->

    parse_class_loop(T, Depth, [Meta | Fields], ["string" | List]);

parse_class_loop([Meta = #meta{type = bool, explain = undefined} | T], Depth, Fields, List) ->

    parse_class_loop(T, Depth, [Meta | Fields], ["boolean" | List]);

parse_class_loop([Meta = #meta{type = u8, explain = undefined} | T], Depth, Fields, List) ->

    parse_class_loop(T, Depth, [Meta | Fields], ["integer" | List]);

parse_class_loop([Meta = #meta{type = u16, explain = undefined} | T], Depth, Fields, List) ->

    parse_class_loop(T, Depth, [Meta | Fields], ["integer" | List]);

parse_class_loop([Meta = #meta{type = u32, explain = undefined} | T], Depth, Fields, List) ->

    parse_class_loop(T, Depth, [Meta | Fields], ["integer" | List]);

parse_class_loop([Meta = #meta{type = u64, explain = undefined} | T], Depth, Fields, List) ->

    parse_class_loop(T, Depth, [Meta | Fields], ["integer" | List]);

parse_class_loop([Meta = #meta{type = i8, explain = undefined} | T], Depth, Fields, List) ->

    parse_class_loop(T, Depth, [Meta | Fields], ["integer" | List]);

parse_class_loop([Meta = #meta{type = i16, explain = undefined} | T], Depth, Fields, List) ->

    parse_class_loop(T, Depth, [Meta | Fields], ["integer" | List]);

parse_class_loop([Meta = #meta{type = i32, explain = undefined} | T], Depth, Fields, List) ->

    parse_class_loop(T, Depth, [Meta | Fields], ["integer" | List]);

parse_class_loop([Meta = #meta{type = i64, explain = undefined} | T], Depth, Fields, List) ->

    parse_class_loop(T, Depth, [Meta | Fields], ["integer" | List]);

parse_class_loop([Meta = #meta{type = f32, explain = undefined} | T], Depth, Fields, List) ->

    parse_class_loop(T, Depth, [Meta | Fields], ["number" | List]);

parse_class_loop([Meta = #meta{type = f64, explain = undefined} | T], Depth, Fields, List) ->

    parse_class_loop(T, Depth, [Meta | Fields], ["number" | List]);

parse_class_loop([Meta = #meta{type = str, explain = undefined} | T], Depth, Fields, List) ->

    parse_class_loop(T, Depth, [Meta | Fields], ["string" | List]);

parse_class_loop([Meta = #meta{type = bst, explain = undefined} | T], Depth, Fields, List) ->

    parse_class_loop(T, Depth, [Meta | Fields], ["string" | List]);

parse_class_loop([Meta = #meta{type = ast, explain = undefined} | T], Depth, Fields, List) ->

    parse_class_loop(T, Depth, [Meta | Fields], ["string" | List]);

parse_class_loop([Meta = #meta{type = tuple, explain = Explain} | T], Depth, Names, List) ->
    %% alignment padding
    IndentPadding = lists:duplicate(Depth - 1, "    "),
    Padding = lists:duplicate(Depth, "    "),

    SubExplain = tuple_to_list(Explain),

    %% recursive
    {SubNames, SubCodes} = parse_class_loop(SubExplain, Depth + 1, [], []),

    Code = [
        "{",
        [
            begin
                Line = lists:flatten(lists:concat(["\n", "---", " ", Padding, word:to_lower_hump(SubName), ": ", SubCode, ","])),
                LastLine = lists:last(string:tokens(Line, "\n")),
                SubPadding = lists:duplicate(max(0, 100 - lists:flatlength(LastLine)), " "),
                lists:concat([Line, SubPadding, "-- ", SubComment])
            end
            ||
            {#meta{name = SubName, comment = SubComment}, SubCode} <- lists:zip(SubNames, SubCodes)
        ],
        "\n",
        "---", " ", IndentPadding, "}"
    ],

    %% flatten
    parse_class_loop(T, Depth, [Meta | Names], [Code | List]);

parse_class_loop([Meta = #meta{type = record, explain = Explain} | T], Depth, Names, List) ->
    %% alignment padding
    IndentPadding = lists:duplicate(Depth - 1, "    "),
    Padding = lists:duplicate(Depth, "    "),

    SubExplain = [SubMeta || SubMeta = #meta{} <- tuple_to_list(Explain)],

    %% recursive
    {SubNames, SubCodes} = parse_class_loop(SubExplain, Depth + 1, [], []),

    Code = [
        "{",
        [
            begin
                Line = lists:flatten(lists:concat(["\n", "---", " ", Padding, word:to_lower_hump(SubName), ": ", SubCode, ","])),
                LastLine = lists:last(string:tokens(Line, "\n")),
                SubPadding = lists:duplicate(max(0, 100 - lists:flatlength(LastLine)), " "),
                lists:concat([Line, SubPadding, "-- ", SubComment])
            end
            ||
            {#meta{name = SubName, comment = SubComment}, SubCode} <- lists:zip(SubNames, SubCodes)
        ],
        "\n",
        "---", " ", IndentPadding, "}"
    ],

    %% flatten
    parse_class_loop(T, Depth, [Meta | Names], [Code | List]);

parse_class_loop([Meta = #meta{type = maps, explain = Explain} | T], Depth, Names, List) ->
    %% alignment padding
    IndentPadding = lists:duplicate(Depth - 1, "    "),
    Padding = lists:duplicate(Depth, "    "),

    SubExplain = maps:values(Explain),

    %% recursive
    {SubNames, SubCodes} = parse_class_loop(SubExplain, Depth + 1, [], []),

    Code = [
        "{",
        [
            begin
                Line = lists:flatten(lists:concat(["\n", "---", " ", Padding, word:to_lower_hump(SubName), ": ", SubCode, ","])),
                LastLine = lists:last(string:tokens(Line, "\n")),
                SubPadding = lists:duplicate(max(0, 100 - lists:flatlength(LastLine)), " "),
                lists:concat([Line, SubPadding, "-- ", SubComment])
            end
            ||
            {#meta{name = SubName, comment = SubComment}, SubCode} <- lists:zip(SubNames, SubCodes)
        ],
        "\n",
        "---", " ", IndentPadding, "}"
    ],

    %% flatten
    parse_class_loop(T, Depth, [Meta | Names], [Code | List]);

parse_class_loop([Meta = #meta{type = list, explain = Explain, key = undefined} | T], Depth, Fields, List) ->

    %% recursive
    {_, SubCodes} = parse_class_loop(Explain, Depth, [], []),

    Code = ["", string:join(SubCodes, ""), "[]"],

    parse_class_loop(T, Depth, [Meta | Fields], [Code | List]);

parse_class_loop([Meta = #meta{type = list, explain = Explain, key = _} | T], Depth, Fields, List) ->

    %% recursive
    {_, SubCodes} = parse_class_loop(Explain, Depth, [], []),

    Code = ["table<", "boolean|integer|number|string, ", string:join(SubCodes, ""), ">"],

    parse_class_loop(T, Depth, [Meta | Fields], [Code | List]);

parse_class_loop([Meta = #meta{type = ets, explain = Explain, key = []} | T], Depth, Fields, List) ->

    %% recursive
    {_, SubCodes} = parse_class_loop(Explain, Depth, [], []),

    Code = ["", string:join(SubCodes, ""), "[]"],

    parse_class_loop(T, Depth, [Meta | Fields], [Code | List]);

parse_class_loop([Meta = #meta{type = ets, explain = Explain, key = _} | T], Depth, Fields, List) ->

    %% recursive
    {_, SubCodes} = parse_class_loop(Explain, Depth, [], []),

    Code = ["table<", "boolean|integer|number|string, ", string:join(SubCodes, ""), ">"],

    parse_class_loop(T, Depth, [Meta | Fields], [Code | List]).

%%%===================================================================
%%% encode
%%%===================================================================
%% code
parse_encode(Protocol, Meta = #meta{}) ->
    %% start with 3 tabs(4 space) padding
    Padding = lists:duplicate(1, "    "),

    {_, Codes} = parse_encode_loop([Meta], 2, [], []),

    %% codes format
    CodesDefault = lists:concat([
        Padding, "    ", "return table", "\n"
    ]),
    CodesBlock = lists:append(Codes, [CodesDefault]),

    %% format one protocol define
    Code = lists:concat([
        "if protocol == ", Protocol, " then", "\n",
        Padding, "    ", "local table = {}", "\n",
        string:join(CodesBlock, "\n"),
        Padding, "else"
    ]),

    #file{function = Code}.

%% code
parse_encode_loop([], _, Fields, List) ->
    %% construct as a list
    {lists:reverse(Fields), lists:reverse(List)};

parse_encode_loop([#meta{path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = binary, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),
    PathGetName = [word:to_lower_hump(Prefix), word:to_hump(Suffix), ["." || Get =/= []], word:to_lower_hump(Get)],

    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = ", PathGetName, "\n",
        Padding, "offset = offset + 1"
    ],
    parse_encode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_encode_loop([#meta{path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = bool, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),
    PathGetName = [word:to_lower_hump(Prefix), word:to_hump(Suffix), ["." || Get =/= []], word:to_lower_hump(Get)],

    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = string.pack(\">I1\", ", PathGetName, " ~= 0 and 1 or 0)", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_encode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_encode_loop([#meta{path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = u8, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),
    PathGetName = [word:to_lower_hump(Prefix), word:to_hump(Suffix), ["." || Get =/= []], word:to_lower_hump(Get)],

    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = string.pack(\">I1\", ", PathGetName, ")", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_encode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_encode_loop([#meta{path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = u16, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),
    PathGetName = [word:to_lower_hump(Prefix), word:to_hump(Suffix), ["." || Get =/= []], word:to_lower_hump(Get)],

    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = string.pack(\">I2\", ", PathGetName, ")", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_encode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_encode_loop([#meta{path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = u32, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),
    PathGetName = [word:to_lower_hump(Prefix), word:to_hump(Suffix), ["." || Get =/= []], word:to_lower_hump(Get)],

    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = string.pack(\">I4\", ", PathGetName, ")", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_encode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_encode_loop([#meta{path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = u64, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),
    PathGetName = [word:to_lower_hump(Prefix), word:to_hump(Suffix), ["." || Get =/= []], word:to_lower_hump(Get)],

    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = string.pack(\">I8\", ", PathGetName, ")", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_encode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_encode_loop([#meta{path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = i8, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),
    PathGetName = [word:to_lower_hump(Prefix), word:to_hump(Suffix), ["." || Get =/= []], word:to_lower_hump(Get)],

    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = string.pack(\">i1\", ", PathGetName, ")", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_encode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_encode_loop([#meta{path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = i16, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),
    PathGetName = [word:to_lower_hump(Prefix), word:to_hump(Suffix), ["." || Get =/= []], word:to_lower_hump(Get)],

    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = string.pack(\">i2\", ", PathGetName, ")", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_encode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_encode_loop([#meta{path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = i32, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),
    PathGetName = [word:to_lower_hump(Prefix), word:to_hump(Suffix), ["." || Get =/= []], word:to_lower_hump(Get)],

    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = string.pack(\">i4\", ", PathGetName, ")", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_encode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_encode_loop([#meta{path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = i64, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),
    PathGetName = [word:to_lower_hump(Prefix), word:to_hump(Suffix), ["." || Get =/= []], word:to_lower_hump(Get)],

    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = string.pack(\">i8\", ", PathGetName, ")", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_encode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_encode_loop([#meta{path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = f32, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),
    PathGetName = [word:to_lower_hump(Prefix), word:to_hump(Suffix), ["." || Get =/= []], word:to_lower_hump(Get)],

    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = string.pack(\">f\", ", PathGetName, ")", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_encode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_encode_loop([#meta{path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = f64, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),
    PathGetName = [word:to_lower_hump(Prefix), word:to_hump(Suffix), ["." || Get =/= []], word:to_lower_hump(Get)],

    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = string.pack(\">d\", ", PathGetName, ")", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_encode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_encode_loop([#meta{path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = str, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),
    PathGetName = [word:to_lower_hump(Prefix), word:to_hump(Suffix), ["." || Get =/= []], word:to_lower_hump(Get)],

    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = string.pack(\">s2\", ", PathGetName, ")", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_encode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_encode_loop([#meta{path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = bst, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),
    PathGetName = [word:to_lower_hump(Prefix), word:to_hump(Suffix), ["." || Get =/= []], word:to_lower_hump(Get)],

    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = string.pack(\">s2\", ", PathGetName, ")", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_encode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_encode_loop([#meta{path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = ast, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),
    PathGetName = [word:to_lower_hump(Prefix), word:to_hump(Suffix), ["." || Get =/= []], word:to_lower_hump(Get)],

    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = string.pack(\">s2\", ", PathGetName, ")", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_encode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_encode_loop([#meta{from = From, path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = tuple, explain = Explain} | T], Depth, Names, List) ->
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),
    PathGetName = [word:to_lower_hump(Prefix), word:to_hump(Suffix), ["." || Get =/= []], word:to_lower_hump(Get)],
    SubExplain = tuple_to_list(Explain),

    %% not root or list inner
    SubCode = [lists:concat([Padding, "local ", PathHumpName, " = ", PathGetName, ";"]) || From =/= root andalso From =/= list andalso From =/= ets],

    %% recursive
    {SubNames, SubCodes} = parse_encode_loop(SubExplain, Depth, Names, [SubCode | List]),

    %% flatten
    parse_encode_loop(T, Depth, lists:reverse(SubNames), lists:reverse(SubCodes));

parse_encode_loop([#meta{from = From, path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = record, explain = Explain} | T], Depth, Names, List) ->
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),
    PathGetName = [word:to_lower_hump(Prefix), word:to_hump(Suffix), ["." || Get =/= []], word:to_lower_hump(Get)],
    SubExplain = [Meta || Meta = #meta{} <- tuple_to_list(Explain)],

    %% not root or list inner
    SubCode = [lists:concat([Padding, "local ", PathHumpName, " = ", PathGetName, ";"]) || From =/= root andalso From =/= list andalso From =/= ets],

    %% recursive
    {SubNames, SubCodes} = parse_encode_loop(SubExplain, Depth, Names, [SubCode | List]),

    %% flatten
    parse_encode_loop(T, Depth, lists:reverse(SubNames), lists:reverse(SubCodes));

parse_encode_loop([#meta{from = From, path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = maps, explain = Explain} | T], Depth, Names, List) ->
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),
    PathGetName = [word:to_lower_hump(Prefix), word:to_hump(Suffix), ["." || Get =/= []], word:to_lower_hump(Get)],
    SubExplain = maps:values(Explain),

    %% not root or list inner
    SubCode = [lists:concat([Padding, "local ", PathHumpName, " = ", PathGetName, ";"]) || From =/= root andalso From =/= list andalso From =/= ets],

    %% recursive
    {SubNames, SubCodes} = parse_encode_loop(SubExplain, Depth, Names, [SubCode | List]),

    %% flatten
    parse_encode_loop(T, Depth, lists:reverse(SubNames), lists:reverse(SubCodes));

parse_encode_loop([#meta{from = From, path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = list, explain = Explain, comment = Comment, key = undefined} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),
    PathGetName = [word:to_lower_hump(Prefix), word:to_hump(Suffix), ["." || Get =/= []], word:to_lower_hump(Get)],

    %% recursive
    {_, SubCodes} = parse_encode_loop(Explain, Depth + 1, [], []),

    %% not root or list inner
    SubCode = [lists:concat([Padding, "local ", PathHumpName, " = ", PathGetName, ";"]) || From =/= root andalso From =/= list andalso From =/= ets],

    Code = [
        SubCode, "\n",
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = string.pack(\">I2\", #", PathHumpName, ")", "\n",
        Padding, "offset = offset + 1", "\n",
        Padding, "for ", PathHumpName, "Index = 1, #", PathHumpName, " do", "\n",
        Padding, "    ", "local ", PathHumpName, "Data = ", PathHumpName, "[", PathHumpName, "Index]", "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "end"
    ],
    parse_encode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_encode_loop([#meta{from = From, path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = list, explain = Explain, comment = Comment, key = _} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),
    PathGetName = [word:to_lower_hump(Prefix), word:to_hump(Suffix), ["." || Get =/= []], word:to_lower_hump(Get)],

    %% recursive
    {_, SubCodes} = parse_encode_loop(Explain, Depth + 1, [], []),

    %% not root or list inner
    SubCode = [lists:concat([Padding, "local ", PathHumpName, " = ", PathGetName, ";"]) || From =/= root andalso From =/= list andalso From =/= ets],

    Code = [
        SubCode, "\n",
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = string.pack(\">I2\", #", PathHumpName, ")", "\n",
        Padding, "offset = offset + 1", "\n",
        Padding, "for _, ", PathHumpName, "Data in pairs(", PathHumpName, ") do", "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "end"
    ],
    parse_encode_loop(T, Depth, [PathHumpName | Fields], [Code | List]).

%%%===================================================================
%%% decode
%%%===================================================================
%% code
parse_decode(Protocol, Meta) ->
    %% start with 3 tabs(4 space) padding
    Padding = lists:duplicate(1, "    "),

    {Fields, Codes} = parse_decode_loop([Meta], 2, [], []),

    %% codes format
    CodesDefault = lists:concat([
        Padding, "    ", "return ", "{protocol = ", Protocol, ", ", word:to_lower_hump(?ANONYMOUS_DATA_NAME), " = ", string:join(Fields, ", "), "}", "\n"
    ]),
    CodesBlock = lists:append(Codes, [CodesDefault]),

    %% format one protocol define
    Code = lists:concat([
        "if protocol == ", Protocol, " then", "\n",
        string:join(CodesBlock, "\n"),
        Padding, "else"
    ]),

    #file{function = Code}.

%% code
parse_decode_loop([], _, Fields, List) ->
    %% construct as a list
    {lists:reverse(Fields), lists:reverse(List)};

parse_decode_loop([#meta{type = zero} | T], Depth, Fields, List) ->

    parse_decode_loop(T, Depth, Fields, List);

parse_decode_loop([#meta{path = Path, type = binary, explain = Explain, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),

    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", PathHumpName, " = string.unpack(\"c", integer_to_list(Explain), "\", bytes, offset)", "\n",
        Padding, "offset = offset + ", integer_to_list(Explain)
    ],
    parse_decode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_decode_loop([#meta{path = Path, type = bool, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),

    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", PathHumpName, " = string.unpack(\">I1\", bytes, offset) ~= 0", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_decode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_decode_loop([#meta{path = Path, type = u8, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),

    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", PathHumpName, " = string.unpack(\">I1\", bytes, offset)", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_decode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_decode_loop([#meta{path = Path, type = u16, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),

    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", PathHumpName, " = string.unpack(\">I2\", bytes, offset)", "\n",
        Padding, "offset = offset + 2"
    ],
    parse_decode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_decode_loop([#meta{path = Path, type = u32, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),

    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", PathHumpName, " = string.unpack(\">I4\", bytes, offset)", "\n",
        Padding, "offset = offset + 4"
    ],
    parse_decode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_decode_loop([#meta{path = Path, type = u64, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),

    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", PathHumpName, " = string.unpack(\">I8\", bytes, offset)", "\n",
        Padding, "offset = offset + 8"
    ],
    parse_decode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_decode_loop([#meta{path = Path, type = i8, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),

    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", PathHumpName, " = string.unpack(\">i1\", bytes, offset)", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_decode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_decode_loop([#meta{path = Path, type = i16, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),

    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", PathHumpName, " = string.unpack(\">i2\", bytes, offset)", "\n",
        Padding, "offset = offset + 2"
    ],
    parse_decode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_decode_loop([#meta{path = Path, type = i32, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),

    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", PathHumpName, " = string.unpack(\">i4\", bytes, offset)", "\n",
        Padding, "offset = offset + 4"
    ],
    parse_decode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_decode_loop([#meta{path = Path, type = i64, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),

    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", PathHumpName, " = string.unpack(\">i8\", bytes, offset)", "\n",
        Padding, "offset = offset + 8"
    ],
    parse_decode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_decode_loop([#meta{path = Path, type = f32, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),

    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", PathHumpName, " = string.unpack(\">f\", bytes, offset)", "\n",
        Padding, "offset = offset + 4"
    ],
    parse_decode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_decode_loop([#meta{path = Path, type = f64, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),

    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", PathHumpName, " = string.unpack(\">d\", bytes, offset)", "\n",
        Padding, "offset = offset + 8"
    ],
    parse_decode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_decode_loop([#meta{path = Path, type = str, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),

    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", PathHumpName, " = string.unpack(\">s2\", bytes, offset)", "\n",
        Padding, "offset = offset + 2 + string.len(", PathHumpName, ")"
    ],
    parse_decode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_decode_loop([#meta{path = Path, type = bst, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),

    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", PathHumpName, " = string.unpack(\">s2\", bytes, offset)", "\n",
        Padding, "offset = offset + 2 + string.len(", PathHumpName, ")"
    ],
    parse_decode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_decode_loop([#meta{path = Path, type = ast, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),

    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", PathHumpName, " = string.unpack(\">s2\", bytes, offset)", "\n",
        Padding, "offset = offset + 2 + string.len(", PathHumpName, ")"
    ],
    parse_decode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_decode_loop([#meta{path = Path, type = tuple, explain = Explain, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),

    SubExplain = [Meta || Meta = #meta{type = SubType} <- tuple_to_list(Explain), SubType =/= zero],

    %% recursive
    {SubFields, SubCodes} = parse_decode_loop(SubExplain, Depth, [], []),

    Code = [
        Padding, "-- ", Comment, "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "-- object", "\n",
        Padding, "local ", PathHumpName, " = {", string:join([lists:concat([word:to_lower_hump(FieldName), " = ", PathName]) || {#meta{name = FieldName}, PathName} <- lists:zip(SubExplain, SubFields)], ", "), "}"
    ],

    %% flatten
    parse_decode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_decode_loop([#meta{path = Path, type = record, explain = Explain, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),

    SubExplain = [SubMeta || SubMeta = #meta{} <- tuple_to_list(Explain)],

    %% recursive
    {SubFields, SubCodes} = parse_decode_loop(SubExplain, Depth, [], []),

    Code = [
        Padding, "-- ", Comment, "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "-- object", "\n",
        Padding, "local ", PathHumpName, " = {", string:join([lists:concat([word:to_lower_hump(FieldName), " = ", PathName]) || {#meta{name = FieldName}, PathName} <- lists:zip(SubExplain, SubFields)], ", "), "}"
    ],

    %% flatten
    parse_decode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_decode_loop([#meta{path = Path, type = maps, explain = Explain, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),

    SubExplain = maps:values(Explain),

    %% recursive
    {SubFields, SubCodes} = parse_decode_loop(SubExplain, Depth, [], []),

    Code = [
        Padding, "-- ", Comment, "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "-- object", "\n",
        Padding, "local ", PathHumpName, " = {", string:join([lists:concat([word:to_lower_hump(FieldName), " = ", PathName]) || {#meta{name = FieldName}, PathName} <- lists:zip(SubExplain, SubFields)], ", "), "}"
    ],

    %% flatten
    parse_decode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_decode_loop([#meta{path = Path, type = list, explain = Explain, comment = Comment, key = undefined} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),

    %% recursive
    {SubFields, SubCodes} = parse_decode_loop(Explain, Depth + 1, [], []),
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", PathHumpName, " = {}", "\n",
        Padding, "local ", PathHumpName, "Length = string.unpack(\">I2\", bytes, offset)", "\n",
        Padding, "offset = offset + 2", "\n",
        Padding, "for ", PathHumpName, "Index = 1, ", PathHumpName, "Length do", "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "    ", PathHumpName, "[", PathHumpName, "Index] = ", string:join(SubFields, ", "), "\n",
        Padding, "end"
    ],
    parse_decode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_decode_loop([#meta{path = Path, type = list, explain = Explain, comment = Comment, key = Key} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),

    %% recursive
    {SubFields, SubCodes} = parse_decode_loop(Explain, Depth + 1, [], []),
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", PathHumpName, " = {}", "\n",
        Padding, "local ", PathHumpName, "Length = string.unpack(\">I2\", bytes, offset)", "\n",
        Padding, "offset = offset + 2", "\n",
        Padding, "for ", PathHumpName, "Index = 1, ", PathHumpName, "Length do", "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "    ", PathHumpName, "[", PathHumpName, "Data", word:to_hump(Key), "] = ", string:join(SubFields, ", "), "\n",
        Padding, "end"
    ],
    parse_decode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_decode_loop([#meta{path = Path, type = ets, explain = Explain, comment = Comment, key = []} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),

    %% recursive
    {SubFields, SubCodes} = parse_decode_loop(Explain, Depth + 1, [], []),
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", PathHumpName, " = {}", "\n",
        Padding, "local ", PathHumpName, "Length = string.unpack(\">I2\", bytes, offset)", "\n",
        Padding, "offset = offset + 2", "\n",
        Padding, "for ", PathHumpName, "Index = 1, ", PathHumpName, "Length do", "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "    ", PathHumpName, "[", PathHumpName, "Index] = ", string:join(SubFields, ", "), "\n",
        Padding, "end"
    ],
    parse_decode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_decode_loop([#meta{path = Path, type = ets, explain = Explain, comment = Comment, key = [Key]} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),

    %% recursive
    {SubFields, SubCodes} = parse_decode_loop(Explain, Depth + 1, [], []),
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", PathHumpName, " = {}", "\n",
        Padding, "local ", PathHumpName, "Length = string.unpack(\">I2\", bytes, offset)", "\n",
        Padding, "offset = offset + 2", "\n",
        Padding, "for ", PathHumpName, "Index = 1, ", PathHumpName, "Length do", "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "    ", PathHumpName, "[", PathHumpName, "Data", word:to_hump(Key), "] = ", string:join(SubFields, ", "), "\n",
        Padding, "end"
    ],
    parse_decode_loop(T, Depth, [PathHumpName | Fields], [Code | List]).