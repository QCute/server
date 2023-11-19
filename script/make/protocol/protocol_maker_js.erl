%%%-------------------------------------------------------------------
%%% @doc
%%% make protocol define to js io/metadata code
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_maker_js).
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
    ImportExport = [[Import, "\n", "\n", Export] || {#data{js = #set{code = #file{import = Import}}}, #data{js = #set{code = #file{export = Export}}}} <- lists:zip(EncodeList, DecodeList), Import =/= [] orelse Export =/= []],

    %% encode
    Encode = [Encode || #data{js = #set{code = #file{function = Encode}}} <- EncodeList, Encode =/= []],
    EncodeDefault = lists:concat([
        "    ", "    ", "    ", "default: throw(\"unknown protocol define: \" + protocol)", "\n"
    ]),
    EncodeCode = lists:append(Encode, [EncodeDefault]),

    %% decode
    Decode = [Decode || #data{js = #set{code = #file{function = Decode}}} <- DecodeList, Decode =/= []],
    DecodeDefault = lists:concat([
        "    ", "    ", "    ", "default: throw(\"unknown protocol define: \" + protocol)", "\n"
    ]),
    DecodeCode = lists:append(Decode, [DecodeDefault]),

    lists:concat([
        string:join(ImportExport, "\n\n"),
        "\n",
        "\n",
        "export default class ", Name, " {", "\n",
        "    ", "static encode(textEncoder, view, offset, protocol, data) {", "\n",
        "    ", "    ", "switch (protocol) {", "\n",
        string:join(EncodeCode, "\n"),
        "    ", "    ", "}", "\n",
        "    ", "}", "\n\n",
        "    ", "static decode(textDecoder, view, offset, protocol) {", "\n",
        "    ", "    ", "switch (protocol) {", "\n",
        string:join(DecodeCode, "\n"),
        "    ", "    ", "}", "\n",
        "    ", "}", "\n",
        "}"
    ]).

format_meta(_, List) ->
    JsMetaInner = string:join([lists:concat([
        "    ", "\"", Protocol, "\": {", "\n",
        "    ", "    ", "\"comment\": \"", Comment, "\",", "\n",
        "    ", "    ", "\"write\": ", Read, ",", "\n",
        "    ", "    ", "\"read\": ", Write, "\n",
        "    ", "}"
    ]) || {Protocol, Comment, #data{js = #set{meta = #file{extra = Read}}}, #data{js = #set{meta = #file{extra = Write}}}} <- List, Protocol =/= 0], ",\n"),
    lists:concat([
        "export", " ", "default", " " "{", "\n",
        JsMetaInner, "\n",
        "};"
    ]).

%%%===================================================================
%%% meta
%%%===================================================================
%% meta
parse_meta(_, Meta) ->
    %% start with 1 tabs(4 space) padding
    %% Padding = lists:duplicate(1, "    "),
    Code = parse_meta_loop([Meta], 2, []),
    %% format one protocol define
    %% lists:concat(["        \"", Protocol, "\" : [\n", MetaData, "\n        ]"]).

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
    Code = lists:flatten(io_lib:format("~s{\"name\": \"~s\", \"type\": \"~s\", \"comment\": \"~ts\", \"explain\": ~w}", [Padding, word:to_lower_hump(Name), binary, Comment, Length])),
    parse_meta_loop(T, Depth, [Code | List]);

parse_meta_loop([#meta{name = Name, type = Type, explain = undefined, comment = Comment} | T], Depth, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    Code = lists:flatten(io_lib:format("~s{\"name\": \"~s\", \"type\": \"~s\", \"comment\": \"~ts\", \"explain\": ~w}", [Padding, word:to_lower_hump(Name), Type, Comment, []])),
    parse_meta_loop(T, Depth, [Code | List]);

parse_meta_loop([#meta{name = Name, type = tuple, explain = Explain, comment = Comment} | T], Depth, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),

    %% recursive
    SubCodes = parse_meta_loop(tuple_to_list(Explain), Depth + 1, []),

    %% format one field
    Code = lists:flatten(io_lib:format("~s{\"name\": \"~s\", \"type\": \"~s\", \"comment\": \"~ts\", \"explain\": [\n~ts\n~s]}", [Padding, word:to_lower_hump(Name), map, Comment, SubCodes, Padding])),

    parse_meta_loop(T, Depth, [Code | List]);

parse_meta_loop([#meta{name = Name, type = record, explain = Explain, comment = Comment} | T], Depth, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),

    SubExplain = [Meta || Meta = #meta{} <- tuple_to_list(Explain)],

    %% recursive
    SubCodes = parse_meta_loop(SubExplain, Depth + 1, []),

    %% format one field
    Code = lists:flatten(io_lib:format("~s{\"name\": \"~s\", \"type\": \"~s\", \"comment\": \"~ts\", \"explain\": [\n~ts\n~s]}", [Padding, word:to_lower_hump(Name), map, Comment, SubCodes, Padding])),

    parse_meta_loop(T, Depth, [Code | List]);

parse_meta_loop([#meta{name = Name, type = maps, explain = Explain, comment = Comment} | T], Depth, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),

    %% recursive
    SubCodes = parse_meta_loop(maps:values(Explain), Depth + 1, []),

    %% format one field
    Code = lists:flatten(io_lib:format("~s{\"name\": \"~s\", \"type\": \"~s\", \"comment\": \"~ts\", \"explain\": [\n~ts\n~s]}", [Padding, word:to_lower_hump(Name), map, Comment, SubCodes, Padding])),

    parse_meta_loop(T, Depth, [Code | List]);

parse_meta_loop([#meta{name = Name, type = list, explain = Explain, comment = Comment, key = undefined} | T], Depth, List) ->
    %% recursive
    SubCodes = parse_meta_loop(Explain, Depth + 1, []),
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    Code = lists:flatten(io_lib:format("~s{\"name\": \"~s\", \"type\": \"~s\", \"comment\": \"~ts\", \"explain\": [\n~ts\n~s]}", [Padding, word:to_lower_hump(Name), list, Comment, SubCodes, Padding])),
    parse_meta_loop(T, Depth, [Code | List]);

parse_meta_loop([#meta{name = Name, type = list, explain = Explain, comment = Comment, key = Key} | T], Depth, List) ->
    %% recursive
    SubCodes = parse_meta_loop(Explain, Depth + 1, []),
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    Code = lists:flatten(io_lib:format("~s{\"name\": \"~s\", \"type\": \"~s\", \"comment\": \"~ts\", \"key\": \"~ts\", \"explain\": [\n~ts\n~s]}", [Padding, word:to_lower_hump(Name), list, Comment, word:to_lower_hump(Key), SubCodes, Padding])),
    parse_meta_loop(T, Depth, [Code | List]);

parse_meta_loop([#meta{name = Name, type = ets, explain = Explain, comment = Comment, key = undefined} | T], Depth, List) ->
    %% recursive
    SubCodes = parse_meta_loop(Explain, Depth + 1, []),
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    Code = lists:flatten(io_lib:format("~s{\"name\": \"~s\", \"type\": \"~s\", \"comment\": \"~ts\", \"explain\": [\n~ts\n~s]}", [Padding, word:to_lower_hump(Name), list, Comment, SubCodes, Padding])),
    parse_meta_loop(T, Depth, [Code | List]);

parse_meta_loop([#meta{name = Name, type = ets, explain = Explain, comment = Comment, key = Key} | T], Depth, List) ->
    %% recursive
    SubCodes = parse_meta_loop(Explain, Depth + 1, []),
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    Code = lists:flatten(io_lib:format("~s{\"name\": \"~s\", \"type\": \"~s\", \"comment\": \"~ts\", \"key\": \"~ts\", \"explain\": [\n~ts\n~s]}", [Padding, word:to_lower_hump(Name), list, Comment, word:to_lower_hump(Key), SubCodes, Padding])),
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
        "export class ", string:replace(word:to_hump(Name), "Protocol", ""), word:to_hump(Sub), "Request {", "\n",
        "    ", "/** @type {number} protocol **/" "\n",
        "    ", "protocol = ", Protocol, ";", "\n",
        "    ", "/**", "\n",
        "    ", " * ", "@type {", string:join(Codes, ""), "} ", "data", "\n",
        "    ", "**/", "\n",
        "    ", "data", ";", "\n",
        "}"
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
        "export class ", string:replace(word:to_hump(Name), "Protocol", ""), word:to_hump(Sub), "Response {", "\n",
        "    ", "/** @type {number} protocol **/" "\n",
        "    ", "protocol = ", Protocol, ";", "\n",
        "    ", "/**", "\n",
        "    ", " * ", "@type {", string:join(Codes, ""), "} ", "data", "\n",
        "    ", "**/", "\n",
        "    ", "data", ";", "\n",
        "}"
    ]),

    #file{export = Code}.


parse_class_loop([], _, Fields, List) ->
    %% construct as a list
    {lists:reverse(Fields), lists:reverse(List)};

parse_class_loop([#meta{type = zero} | T], Depth, Fields, List) ->

    parse_class_loop(T, Depth, Fields, List);

parse_class_loop([Meta = #meta{type = binary} | T], Depth, Fields, List) ->

    parse_class_loop(T, Depth, [Meta | Fields], ["Uint8Array" | List]);

parse_class_loop([Meta = #meta{type = bool, explain = undefined} | T], Depth, Fields, List) ->

    parse_class_loop(T, Depth, [Meta | Fields], ["boolean" | List]);

parse_class_loop([Meta = #meta{type = u8, explain = undefined} | T], Depth, Fields, List) ->

    parse_class_loop(T, Depth, [Meta | Fields], ["number" | List]);

parse_class_loop([Meta = #meta{type = u16, explain = undefined} | T], Depth, Fields, List) ->

    parse_class_loop(T, Depth, [Meta | Fields], ["number" | List]);

parse_class_loop([Meta = #meta{type = u32, explain = undefined} | T], Depth, Fields, List) ->

    parse_class_loop(T, Depth, [Meta | Fields], ["number" | List]);

parse_class_loop([Meta = #meta{type = u64, explain = undefined} | T], Depth, Fields, List) ->

    parse_class_loop(T, Depth, [Meta | Fields], ["BigInt" | List]);

parse_class_loop([Meta = #meta{type = i8, explain = undefined} | T], Depth, Fields, List) ->

    parse_class_loop(T, Depth, [Meta | Fields], ["number" | List]);

parse_class_loop([Meta = #meta{type = i16, explain = undefined} | T], Depth, Fields, List) ->

    parse_class_loop(T, Depth, [Meta | Fields], ["number" | List]);

parse_class_loop([Meta = #meta{type = i32, explain = undefined} | T], Depth, Fields, List) ->

    parse_class_loop(T, Depth, [Meta | Fields], ["number" | List]);

parse_class_loop([Meta = #meta{type = i64, explain = undefined} | T], Depth, Fields, List) ->

    parse_class_loop(T, Depth, [Meta | Fields], ["BigInt" | List]);

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
                Line = lists:flatten(lists:concat(["\n", "    ", " * ", Padding, word:to_lower_hump(SubName), ": ", SubCode, ";"])),
                LastLine = lists:last(string:tokens(Line, "\n")),
                SubPadding = lists:duplicate(max(0, 100 - lists:flatlength(LastLine)), " "),
                lists:concat([Line, SubPadding, "// ", SubComment])
            end
            ||
            {#meta{name = SubName, comment = SubComment}, SubCode} <- lists:zip(SubNames, SubCodes)
        ],
        "\n",
        "    ", " * ", IndentPadding, "}"
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
                Line = lists:flatten(lists:concat(["\n", "    ", " * ", Padding, word:to_lower_hump(SubName), ": ", SubCode, ";"])),
                LastLine = lists:last(string:tokens(Line, "\n")),
                SubPadding = lists:duplicate(max(0, 100 - lists:flatlength(LastLine)), " "),
                lists:concat([Line, SubPadding, "// ", SubComment])
            end
            ||
            {#meta{name = SubName, comment = SubComment}, SubCode} <- lists:zip(SubNames, SubCodes)
        ],
        "\n",
        "    ", " * ", IndentPadding, "}"
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
                Line = lists:flatten(lists:concat(["\n", "    ", " * ", Padding, word:to_lower_hump(SubName), ": ", SubCode, ";"])),
                LastLine = lists:last(string:tokens(Line, "\n")),
                SubPadding = lists:duplicate(max(0, 100 - lists:flatlength(LastLine)), " "),
                lists:concat([Line, SubPadding, "// ", SubComment])
            end
            ||
            {#meta{name = SubName, comment = SubComment}, SubCode} <- lists:zip(SubNames, SubCodes)
        ],
        "\n",
        "    ", " * ", IndentPadding, "}"
    ],

    %% flatten
    parse_class_loop(T, Depth, [Meta | Names], [Code | List]);

parse_class_loop([Meta = #meta{type = list, explain = Explain, key = undefined} | T], Depth, Fields, List) ->

    %% recursive
    {_, SubCodes} = parse_class_loop(Explain, Depth, [], []),

    Code = ["Array<", string:join(SubCodes, ""), ">"],

    parse_class_loop(T, Depth, [Meta | Fields], [Code | List]);

parse_class_loop([Meta = #meta{type = list, explain = Explain, key = Key} | T], Depth, Fields, List) ->

    %% recursive
    {_, SubCodes} = parse_class_loop(Explain, Depth, [], []),

    Code = ["{[", word:to_lower_hump(Key), ": boolean|number|string|BigInt]: ", string:join(SubCodes, ""), "}"],

    parse_class_loop(T, Depth, [Meta | Fields], [Code | List]);

parse_class_loop([Meta = #meta{type = ets, explain = Explain, key = []} | T], Depth, Fields, List) ->

    %% recursive
    {_, SubCodes} = parse_class_loop(Explain, Depth, [], []),

    Code = ["Array<", string:join(SubCodes, ""), ">"],

    parse_class_loop(T, Depth, [Meta | Fields], [Code | List]);

parse_class_loop([Meta = #meta{type = ets, explain = Explain, key = Key} | T], Depth, Fields, List) ->

    %% recursive
    {_, SubCodes} = parse_class_loop(Explain, Depth, [], []),

    Code = ["{[", word:to_lower_hump(Key), ": boolean|number|string|BigInt]: ", string:join(SubCodes, ""), "}"],

    parse_class_loop(T, Depth, [Meta | Fields], [Code | List]).

%%%===================================================================
%%% encode
%%%===================================================================
%% code
parse_encode(Protocol, Meta = #meta{}) ->
    %% start with 3 tabs(4 space) padding
    Padding = lists:duplicate(3, "    "),

    {_, Codes} = parse_encode_loop([Meta], 4, [], []),

    %% codes format
    CodesDefault = lists:concat([
        Padding, "    ", "return new DataView(view.buffer.slice(0, offset));", "\n"
    ]),
    CodesBlock = lists:append(Codes, [CodesDefault]),

    %% format one protocol define
    Code = lists:concat([
        Padding, "case ", Protocol, ": {", "\n",
        string:join(CodesBlock, "\n"),
        Padding, "}"
    ]),

    #file{function = Code}.

parse_encode_loop([], _, Fields, List) ->
    %% construct as a list
    {lists:reverse(Fields), lists:reverse(List)};

parse_encode_loop([#meta{path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = binary, explain = Length, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),
    PathGetName = [word:to_lower_hump(Prefix), word:to_hump(Suffix), ["." || Get =/= []], word:to_lower_hump(Get)],

    Code = [
        Padding, "// extend", "\n",
        Padding, "while (view.byteLength < offset + ", integer_to_list(Length), ") {", "\n",
        Padding, "    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));", "\n",
        Padding, "    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));", "\n",
        Padding, "    view = extendView;", "\n",
        Padding, "}", "\n",
        Padding, "// ", Comment, "\n",
        Padding, "const ", PathHumpName, " = ", PathGetName, ";", "\n",
        Padding, "(new Uint8Array(view.buffer, offset)).set(new Uint8Array(", PathHumpName, "));", "\n",
        Padding, "offset = offset + ", PathHumpName, ".byteLength;"
    ],
    parse_encode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_encode_loop([#meta{path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = bool, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),
    PathGetName = [word:to_lower_hump(Prefix), word:to_hump(Suffix), ["." || Get =/= []], word:to_lower_hump(Get)],

    Code = [
        Padding, "// extend", "\n",
        Padding, "while (view.byteLength < offset + 1) {", "\n",
        Padding, "    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));", "\n",
        Padding, "    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));", "\n",
        Padding, "    view = extendView;", "\n",
        Padding, "}", "\n",
        Padding, "// ", Comment, "\n",
        Padding, "view.setUint8(offset, ", PathGetName, " ? 1 : 0, false);", "\n",
        Padding, "offset = offset + 1;"
    ],
    parse_encode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_encode_loop([#meta{path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = u8, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),
    PathGetName = [word:to_lower_hump(Prefix), word:to_hump(Suffix), ["." || Get =/= []], word:to_lower_hump(Get)],

    Code = [
        Padding, "// extend", "\n",
        Padding, "while (view.byteLength < offset + 1) {", "\n",
        Padding, "    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));", "\n",
        Padding, "    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));", "\n",
        Padding, "    view = extendView;", "\n",
        Padding, "}", "\n",
        Padding, "// ", Comment, "\n",
        Padding, "view.setUint8(offset, ", PathGetName, ", false);", "\n",
        Padding, "offset = offset + 1;"
    ],
    parse_encode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_encode_loop([#meta{path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = u16, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),
    PathGetName = [word:to_lower_hump(Prefix), word:to_hump(Suffix), ["." || Get =/= []], word:to_lower_hump(Get)],

    Code = [
        Padding, "// extend", "\n",
        Padding, "while (view.byteLength < offset + 2) {", "\n",
        Padding, "    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));", "\n",
        Padding, "    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));", "\n",
        Padding, "    view = extendView;", "\n",
        Padding, "}", "\n",
        Padding, "// ", Comment, "\n",
        Padding, "view.setUint16(offset, ", PathGetName, ", false);", "\n",
        Padding, "offset = offset + 2;"
    ],
    parse_encode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_encode_loop([#meta{path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = u32, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),
    PathGetName = [word:to_lower_hump(Prefix), word:to_hump(Suffix), ["." || Get =/= []], word:to_lower_hump(Get)],

    Code = [
        Padding, "// extend", "\n",
        Padding, "while (view.byteLength < offset + 4) {", "\n",
        Padding, "    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));", "\n",
        Padding, "    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));", "\n",
        Padding, "    view = extendView;", "\n",
        Padding, "}", "\n",
        Padding, "// ", Comment, "\n",
        Padding, "view.setUint32(offset, ", PathGetName, ", false);", "\n",
        Padding, "offset = offset + 4;"
    ],
    parse_encode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_encode_loop([#meta{path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = u64, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),
    PathGetName = [word:to_lower_hump(Prefix), word:to_hump(Suffix), ["." || Get =/= []], word:to_lower_hump(Get)],

    Code = [
        Padding, "// extend", "\n",
        Padding, "while (view.byteLength < offset + 8) {", "\n",
        Padding, "    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));", "\n",
        Padding, "    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));", "\n",
        Padding, "    view = extendView;", "\n",
        Padding, "}", "\n",
        Padding, "// ", Comment, "\n",
        Padding, "view.setBigUint64(offset, ", PathGetName, ", false);", "\n",
        Padding, "offset = offset + 8;"
    ],
    parse_encode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_encode_loop([#meta{path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = i8, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),
    PathGetName = [word:to_lower_hump(Prefix), word:to_hump(Suffix), ["." || Get =/= []], word:to_lower_hump(Get)],

    Code = [
        Padding, "// extend", "\n",
        Padding, "while (view.byteLength < offset + 1) {", "\n",
        Padding, "    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));", "\n",
        Padding, "    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));", "\n",
        Padding, "    view = extendView;", "\n",
        Padding, "}", "\n",
        Padding, "// ", Comment, "\n",
        Padding, "view.setInt8(offset, ", PathGetName, ", false);", "\n",
        Padding, "offset = offset + 1;"
    ],
    parse_encode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_encode_loop([#meta{path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = i16, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),
    PathGetName = [word:to_lower_hump(Prefix), word:to_hump(Suffix), ["." || Get =/= []], word:to_lower_hump(Get)],

    Code = [
        Padding, "// extend", "\n",
        Padding, "while (view.byteLength < offset + 2) {", "\n",
        Padding, "    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));", "\n",
        Padding, "    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));", "\n",
        Padding, "    view = extendView;", "\n",
        Padding, "}", "\n",
        Padding, "// ", Comment, "\n",
        Padding, "view.setInt16(offset, ", PathGetName, ", false);", "\n",
        Padding, "offset = offset + 2;"
    ],
    parse_encode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_encode_loop([#meta{path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = i32, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),
    PathGetName = [word:to_lower_hump(Prefix), word:to_hump(Suffix), ["." || Get =/= []], word:to_lower_hump(Get)],

    Code = [
        Padding, "// extend", "\n",
        Padding, "while (view.byteLength < offset + 4) {", "\n",
        Padding, "    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));", "\n",
        Padding, "    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));", "\n",
        Padding, "    view = extendView;", "\n",
        Padding, "}", "\n",
        Padding, "// ", Comment, "\n",
        Padding, "view.setInt32(offset, ", PathGetName, ", false);", "\n",
        Padding, "offset = offset + 4;"
    ],
    parse_encode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_encode_loop([#meta{path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = i64, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),
    PathGetName = [word:to_lower_hump(Prefix), word:to_hump(Suffix), ["." || Get =/= []], word:to_lower_hump(Get)],

    Code = [
        Padding, "// extend", "\n",
        Padding, "while (view.byteLength < offset + 8) {", "\n",
        Padding, "    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));", "\n",
        Padding, "    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));", "\n",
        Padding, "    view = extendView;", "\n",
        Padding, "}", "\n",
        Padding, "// ", Comment, "\n",
        Padding, "view.setBigInt64(offset, ", PathGetName, ", false);", "\n",
        Padding, "offset = offset + 8;"
    ],
    parse_encode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_encode_loop([#meta{path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = f32, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),
    PathGetName = [word:to_lower_hump(Prefix), word:to_hump(Suffix), ["." || Get =/= []], word:to_lower_hump(Get)],

    Code = [
        Padding, "// extend", "\n",
        Padding, "while (view.byteLength < offset + 4) {", "\n",
        Padding, "    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));", "\n",
        Padding, "    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));", "\n",
        Padding, "    view = extendView;", "\n",
        Padding, "}", "\n",
        Padding, "// ", Comment, "\n",
        Padding, "view.setFloat32(offset, ", PathGetName, ", false);", "\n",
        Padding, "offset = offset + 4;"
    ],
    parse_encode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_encode_loop([#meta{path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = f64, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),
    PathGetName = [word:to_lower_hump(Prefix), word:to_hump(Suffix), ["." || Get =/= []], word:to_lower_hump(Get)],

    Code = [
        Padding, "// extend", "\n",
        Padding, "while (view.byteLength < offset + 8) {", "\n",
        Padding, "    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));", "\n",
        Padding, "    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));", "\n",
        Padding, "    view = extendView;", "\n",
        Padding, "}", "\n",
        Padding, "// ", Comment, "\n",
        Padding, "view.setFloat64(offset, ", PathGetName, ", false);", "\n",
        Padding, "offset = offset + 8;"
    ],
    parse_encode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_encode_loop([#meta{path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = str, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),
    PathGetName = [word:to_lower_hump(Prefix), word:to_hump(Suffix), ["." || Get =/= []], word:to_lower_hump(Get)],

    Code = [
        Padding, "// extend", "\n",
        Padding, "while (view.byteLength < offset + 2) {", "\n",
        Padding, "    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));", "\n",
        Padding, "    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));", "\n",
        Padding, "    view = extendView;", "\n",
        Padding, "}", "\n",
        Padding, "// ", Comment, "\n",
        Padding, "const ", PathHumpName, "Array = textEncoder.encode(", PathGetName, ");", "\n",
        Padding, "view.setUint16(offset, ", PathHumpName, "Array.length, false);", "\n",
        Padding, "offset = offset + 2;", "\n",
        Padding, "// extend", "\n",
        Padding, "while (view.byteLength < offset + ", PathHumpName, "Array.length) {", "\n",
        Padding, "    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));", "\n",
        Padding, "    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));", "\n",
        Padding, "    view = extendView;", "\n",
        Padding, "}", "\n",
        Padding, "(new Uint8Array(view.buffer, offset)).set(", PathHumpName, "Array);", "\n",
        Padding, "offset = offset + ", PathHumpName, "Array.length;"
    ],
    parse_encode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_encode_loop([#meta{path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = bst, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),
    PathGetName = [word:to_lower_hump(Prefix), word:to_hump(Suffix), ["." || Get =/= []], word:to_lower_hump(Get)],

    Code = [
        Padding, "// extend", "\n",
        Padding, "while (view.byteLength < offset + 2) {", "\n",
        Padding, "    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));", "\n",
        Padding, "    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));", "\n",
        Padding, "    view = extendView;", "\n",
        Padding, "}", "\n",
        Padding, "// ", Comment, "\n",
        Padding, "const ", PathHumpName, "Array = textEncoder.encode(", PathGetName, ");", "\n",
        Padding, "view.setUint16(offset, ", PathHumpName, "Array.length, false);", "\n",
        Padding, "offset = offset + 2;", "\n",
        Padding, "// extend", "\n",
        Padding, "while (view.byteLength < offset + ", PathHumpName, "Array.length) {", "\n",
        Padding, "    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));", "\n",
        Padding, "    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));", "\n",
        Padding, "    view = extendView;", "\n",
        Padding, "}", "\n",
        Padding, "(new Uint8Array(view.buffer, offset)).set(", PathHumpName, "Array);", "\n",
        Padding, "offset = offset + ", PathHumpName, "Array.length;"
    ],
    parse_encode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_encode_loop([#meta{path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = ast, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),
    PathGetName = [word:to_lower_hump(Prefix), word:to_hump(Suffix), ["." || Get =/= []], word:to_lower_hump(Get)],

    Code = [
        Padding, "// extend", "\n",
        Padding, "while (view.byteLength < offset + 2) {", "\n",
        Padding, "    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));", "\n",
        Padding, "    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));", "\n",
        Padding, "    view = extendView;", "\n",
        Padding, "}", "\n",
        Padding, "// ", Comment, "\n",
        Padding, "const ", PathHumpName, "Array = textEncoder.encode(", PathGetName, ");", "\n",
        Padding, "view.setUint16(offset, ", PathHumpName, "Array.length, false);", "\n",
        Padding, "offset = offset + 2;", "\n",
        Padding, "// extend", "\n",
        Padding, "while (view.byteLength < offset + ", PathHumpName, "Array.length) {", "\n",
        Padding, "    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));", "\n",
        Padding, "    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));", "\n",
        Padding, "    view = extendView;", "\n",
        Padding, "}", "\n",
        Padding, "(new Uint8Array(view.buffer, offset)).set(", PathHumpName, "Array);", "\n",
        Padding, "offset = offset + ", PathHumpName, "Array.length;"
    ],
    parse_encode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_encode_loop([#meta{from = From, path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = tuple, explain = Explain} | T], Depth, Names, List) ->
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),
    PathGetName = [word:to_lower_hump(Prefix), word:to_hump(Suffix), ["." || Get =/= []], word:to_lower_hump(Get)],
    SubExplain = tuple_to_list(Explain),

    %% not root or list inner
    SubCode = [lists:concat([Padding, "const ", PathHumpName, " = ", PathGetName, ";"]) || From =/= root andalso From =/= list andalso From =/= ets],

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
    SubCode = [lists:concat([Padding, "const ", PathHumpName, " = ", PathGetName, ";"]) || From =/= root andalso From =/= list andalso From =/= ets],

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
    SubCode = [lists:concat([Padding, "const ", PathHumpName, " = ", PathGetName, ";"]) || From =/= root andalso From =/= list andalso From =/= ets],

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
    SubCode = [lists:concat([Padding, "const ", PathHumpName, " = ", PathGetName, ";"]) || From =/= root andalso From =/= list andalso From =/= ets],

    Code = [
        SubCode, "\n",
        Padding, "// extend", "\n",
        Padding, "while (view.byteLength < offset + 2) {", "\n",
        Padding, "    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));", "\n",
        Padding, "    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));", "\n",
        Padding, "    view = extendView;", "\n",
        Padding, "}", "\n",
        Padding, "// ", Comment, "\n",
        Padding, "view.setUint16(offset, ", PathHumpName, ".length, false);", "\n",
        Padding, "offset = offset + 2;", "\n",
        Padding, "for (const ", PathHumpName, "Data of ", PathHumpName, ") {", "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "}"
    ],
    parse_encode_loop(T, Depth, [PathHumpName, lists:concat([PathHumpName, "Length"]) | Fields], [Code | List]);

parse_encode_loop([#meta{from = From, path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = list, explain = Explain, comment = Comment, key = _} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),
    PathGetName = [word:to_lower_hump(Prefix), word:to_hump(Suffix), ["." || Get =/= []], word:to_lower_hump(Get)],

    %% recursive
    {_, SubCodes} = parse_encode_loop(Explain, Depth + 1, [], []),

    %% not root or list inner
    SubCode = [lists:concat([Padding, "const ", PathHumpName, " = ", PathGetName, ";"]) || From =/= root andalso From =/= list andalso From =/= ets],

    Code = [
        SubCode, "\n",
        Padding, "// extend", "\n",
        Padding, "while (view.byteLength < offset + 2) {", "\n",
        Padding, "    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));", "\n",
        Padding, "    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));", "\n",
        Padding, "    view = extendView;", "\n",
        Padding, "}", "\n",
        Padding, "// ", Comment, "\n",
        Padding, "view.setUint16(offset, Object.keys(", PathHumpName, ").length, false);", "\n",
        Padding, "offset = offset + 2;", "\n",
        Padding, "for (const ", PathHumpName, "Key in ", PathHumpName, ") {", "\n",
        Padding, "    ", "const ", PathHumpName, "Data = ", PathHumpName, "[", PathHumpName, "Key", "];", "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "}"
    ],
    parse_encode_loop(T, Depth, [PathHumpName, lists:concat([PathHumpName, "Length"]) | Fields], [Code | List]).

%%%===================================================================
%%% decode
%%%===================================================================
%% code
parse_decode(Protocol, Meta) ->
    %% start with 3 tabs(4 space) padding
    Padding = lists:duplicate(3, "    "),

    {Fields, Codes} = parse_decode_loop([Meta], 4, [], []),

    %% codes format
    CodesDefault = lists:concat([
        Padding, "    ", "return ", "{", "\"protocol\": ", Protocol, ", \"data\": ", string:join(Fields, ", "), "}", ";", "\n"
    ]),
    CodesBlock = lists:append(Codes, [CodesDefault]),

    %% format one protocol define
    Code = lists:concat([
        Padding, "case ", Protocol, ": {", "\n",
        string:join(CodesBlock, "\n"),
        Padding, "}"
    ]),

    #file{function = Code}.

%% code
parse_decode_loop([], _, Fields, List) ->
    %% construct as a list
    {lists:reverse(Fields), lists:reverse(List)};

parse_decode_loop([#meta{name = _, type = zero} | T], Depth, Fields, List) ->

    parse_decode_loop(T, Depth, Fields, List);

parse_decode_loop([#meta{path = Path, type = binary, explain = Length, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "const ", PathHumpName, " = view.buffer.slice(offset, offset + ", integer_to_list(Length), ");", "\n",
        Padding, "offset = offset + ", integer_to_list(Length), ";"
    ],
    parse_decode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_decode_loop([#meta{path = Path, type = bool, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "const ", PathHumpName, " = view.getUint8(offset, false) !== 0;", "\n",
        Padding, "offset = offset + 1;"
    ],
    parse_decode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_decode_loop([#meta{path = Path, type = u8, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "const ", PathHumpName, " = view.getUint8(offset, false);", "\n",
        Padding, "offset = offset + 1;"
    ],
    parse_decode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_decode_loop([#meta{path = Path, type = u16, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "const ", PathHumpName, " = view.getUint16(offset, false);", "\n",
        Padding, "offset = offset + 2;"
    ],
    parse_decode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_decode_loop([#meta{path = Path, type = u32, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "const ", PathHumpName, " = view.getUint32(offset, false);", "\n",
        Padding, "offset = offset + 4;"
    ],
    parse_decode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_decode_loop([#meta{path = Path, type = u64, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "const ", PathHumpName, " = view.getBigUint64(offset, false);", "\n",
        Padding, "offset = offset + 8;"
    ],
    parse_decode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_decode_loop([#meta{path = Path, type = i8, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "const ", PathHumpName, " = view.getInt8(offset, false);", "\n",
        Padding, "offset = offset + 1;"
    ],
    parse_decode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_decode_loop([#meta{path = Path, type = i16, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "const ", PathHumpName, " = view.getInt16(offset, false);", "\n",
        Padding, "offset = offset + 2;"
    ],
    parse_decode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_decode_loop([#meta{path = Path, type = i32, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "const ", PathHumpName, " = view.getInt32(offset, false);", "\n",
        Padding, "offset = offset + 4;"
    ],
    parse_decode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_decode_loop([#meta{path = Path, type = i64, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "const ", PathHumpName, " = view.getBigInt64(offset, false);", "\n",
        Padding, "offset = offset + 8;"
    ],
    parse_decode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_decode_loop([#meta{path = Path, type = f32, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "const ", PathHumpName, " = view.getFloat32(offset, false);", "\n",
        Padding, "offset = offset + 4;"
    ],
    parse_decode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_decode_loop([#meta{path = Path, type = f64, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "const ", PathHumpName, " = view.getFloat64(offset, false);", "\n",
        Padding, "offset = offset + 8;"
    ],
    parse_decode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_decode_loop([#meta{path = Path, type = str, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "const ", PathHumpName, "Length = view.getUint16(offset, false);", "\n",
        Padding, "offset = offset + 2;", "\n",
        Padding, "const ", PathHumpName, "Array = new Uint8Array(view.buffer.slice(offset, offset + ", PathHumpName, "Length));", "\n",
        Padding, "const ", PathHumpName, " = textDecoder.decode(", PathHumpName, "Array);", "\n",
        Padding, "offset = offset + ", PathHumpName, "Length;"
    ],
    parse_decode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_decode_loop([#meta{path = Path, type = bst, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "const ", PathHumpName, "Length = view.getUint16(offset, false);", "\n",
        Padding, "offset = offset + 2;", "\n",
        Padding, "const ", PathHumpName, "Array = new Uint8Array(view.buffer.slice(offset, offset + ", PathHumpName, "Length));", "\n",
        Padding, "const ", PathHumpName, " = textDecoder.decode(", PathHumpName, "Array);", "\n",
        Padding, "offset = offset + ", PathHumpName, "Length;"
    ],
    parse_decode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_decode_loop([#meta{path = Path, type = ast, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "const ", PathHumpName, "Length = view.getUint16(offset, false);", "\n",
        Padding, "offset = offset + 2;", "\n",
        Padding, "const ", PathHumpName, "Array = new Uint8Array(view.buffer.slice(offset, offset + ", PathHumpName, "Length));", "\n",
        Padding, "const ", PathHumpName, " = textDecoder.decode(", PathHumpName, "Array);", "\n",
        Padding, "offset = offset + ", PathHumpName, "Length;"
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
        Padding, "// ", Comment, "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "// object", "\n",
        Padding, "const ", PathHumpName, " = {", string:join([lists:concat(["\"", word:to_lower_hump(FieldName), "\"", ": ", PathName]) || {#meta{name = FieldName}, PathName} <- lists:zip(SubExplain, SubFields)], ", "), "};"
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
        Padding, "// ", Comment, "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "// object", "\n",
        Padding, "const ", PathHumpName, " = {", string:join([lists:concat(["\"", word:to_lower_hump(FieldName), "\"", ": ", PathName]) || {#meta{name = FieldName}, PathName} <- lists:zip(SubExplain, SubFields)], ", "), "};"
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
        Padding, "// ", Comment, "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "// object", "\n",
        Padding, "const ", PathHumpName, " = {", string:join([lists:concat(["\"", word:to_lower_hump(FieldName), "\"", ": ", PathName]) || {#meta{name = FieldName}, PathName} <- lists:zip(SubExplain, SubFields)], ", "), "};"
    ],

    %% flatten
    parse_decode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_decode_loop([#meta{path = Path, type = list, explain = Explain, comment = Comment, key = undefined} | T], Depth, Fields, List)  ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),

    %% recursive
    {SubFields, SubCodes} = parse_decode_loop(Explain, Depth + 1, [], []),
    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "const ", PathHumpName, " = [];", "\n",
        Padding, "let ", PathHumpName, "Length = view.getUint16(offset, false);", "\n",
        Padding, "offset = offset + 2;", "\n",
        Padding, "while (--", PathHumpName, "Length >= 0) {", "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "    // add", "\n",
        Padding, "    ", PathHumpName, ".push(", string:join(SubFields, ", "), ");", "\n",
        Padding, "}"
    ],
    parse_decode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_decode_loop([#meta{path = Path, type = list, explain = Explain, comment = Comment, key = Key} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),

    %% recursive
    {SubFields, SubCodes} = parse_decode_loop(Explain, Depth + 1, [], []),
    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "const ", PathHumpName, " = {};", "\n",
        Padding, "let ", PathHumpName, "Length = view.getUint16(offset, false);", "\n",
        Padding, "offset = offset + 2;", "\n",
        Padding, "while (--", PathHumpName, "Length >= 0) {", "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "    // add", "\n",
        Padding, "    ", PathHumpName, "[", PathHumpName, "Data", word:to_hump(Key), "] = ", string:join(SubFields, ", "), ";", "\n",
        Padding, "}"
    ],
    parse_decode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_decode_loop([#meta{path = Path, type = ets, explain = Explain, comment = Comment, key = []} | T], Depth, Fields, List)  ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),

    %% recursive
    {SubFields, SubCodes} = parse_decode_loop(Explain, Depth + 1, [], []),
    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "const ", PathHumpName, " = [];", "\n",
        Padding, "let ", PathHumpName, "Length = view.getUint16(offset, false);", "\n",
        Padding, "offset = offset + 2;", "\n",
        Padding, "while (--", PathHumpName, "Length >= 0) {", "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "    // add", "\n",
        Padding, "    ", PathHumpName, ".push(", string:join(SubFields, ", "), ");", "\n",
        Padding, "}"
    ],
    parse_decode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_decode_loop([#meta{path = Path, type = ets, explain = Explain, comment = Comment, key = [Key]} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),

    %% recursive
    {SubFields, SubCodes} = parse_decode_loop(Explain, Depth + 1, [], []),

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "const ", PathHumpName, " = {};", "\n",
        Padding, "let ", PathHumpName, "Length = view.getUint16(offset, false);", "\n",
        Padding, "offset = offset + 2;", "\n",
        Padding, "while (--", PathHumpName, "Length >= 0) {", "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "    // add", "\n",
        Padding, "    ", PathHumpName, "[", PathHumpName, "Data", word:to_hump(Key), "] = ", string:join(SubFields, ", "), ";", "\n",
        Padding, "}"
    ],
    parse_decode_loop(T, Depth, [PathHumpName | Fields], [Code | List]).