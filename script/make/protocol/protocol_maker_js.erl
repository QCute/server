%%%-------------------------------------------------------------------
%%% @doc
%%% make protocol define to js io/metadata code
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_maker_js).
-export([format_code/3, format_meta/2]).
-export([parse_meta_js/2]).
-export([parse_encode_js/2, parse_decode_js/2]).
-include("../../../include/serialize.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
format_code(JsName, EncodeList, DecodeList) ->

    %% encode
    JsEncode = [Encode || #data{js = #set{code = #file{function = Encode}}} <- EncodeList, Encode =/= []],
    JsEncodeDefault = lists:concat([
        "    ", "    ", "    ", "default: throw(\"unknown protocol define: \" + protocol)", "\n"
    ]),
    Encode = lists:append(JsEncode, [JsEncodeDefault]),

    %% decode
    JsDecode = [Decode || #data{js = #set{code = #file{function = Decode}}} <- DecodeList, Decode =/= []],
    JsDecodeDefault = lists:concat([
        "    ", "    ", "    ", "default: throw(\"unknown protocol define: \" + protocol)", "\n"
    ]),
    Decode = lists:append(JsDecode, [JsDecodeDefault]),

    lists:concat([
        "export default class ", JsName, " {", "\n",
        "    ", "static encode(textEncoder, view, offset, protocol, data) {", "\n",
        "    ", "    ", "switch (protocol) {", "\n",
        string:join(Encode, "\n"),
        "    ", "    ", "}", "\n",
        "    ", "}", "\n\n",
        "    ", "static decode(textDecoder, view, offset, protocol) {", "\n",
        "    ", "    ", "switch (protocol) {", "\n",
        string:join(Decode, "\n"),
        "    ", "    ", "}", "\n",
        "    ", "}", "\n",
        "}"
    ]).

format_meta(_JsName, List) ->
    JsMetaInner = string:join([lists:concat([
        "    ", "\"", Protocol, "\" : {", "\n",
        "    ", "    ", "\"comment\" : \"", Comment, "\",", "\n",
        "    ", "    ", "\"write\" : ", Read, ",", "\n",
        "    ", "    ", "\"read\" : ", Write, "\n",
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
%% js meta
parse_meta_js(_, Meta) ->
    %% start with 3 tabs(4 space) padding
    Padding = lists:duplicate(2, "    "),
    MetaData = parse_meta_js_loop([Meta], 3, []),
    %% format one protocol define
    %% lists:concat(["        \"", Protocol, "\" : [\n", MetaData, "\n        ]"]).
    Code = lists:concat([
        "[", "\n",
        MetaData, "\n",
        Padding, "]"
    ]),

    #file{extra = Code}.

parse_meta_js_loop([], _, List) ->
    %% construct as a list
    string:join(lists:reverse(List), ",\n");

parse_meta_js_loop([#meta{name = _, type = zero} | T], Depth, List) ->
    parse_meta_js_loop(T, Depth, List);

parse_meta_js_loop([#meta{name = Name, type = binary, explain = Length, comment = Comment} | T], Depth, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    Code = lists:flatten(io_lib:format("~s{\"name\": \"~s\", \"type\": \"~s\", \"comment\": \"~ts\", \"explain\": ~w}", [Padding, word:to_lower_hump(Name), binary, Comment, Length])),
    parse_meta_js_loop(T, Depth, [Code | List]);

parse_meta_js_loop([#meta{name = Name, type = Type, explain = undefined, comment = Comment} | T], Depth, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    Code = lists:flatten(io_lib:format("~s{\"name\": \"~s\", \"type\": \"~s\", \"comment\": \"~ts\", \"explain\": ~w}", [Padding, word:to_lower_hump(Name), Type, Comment, []])),
    parse_meta_js_loop(T, Depth, [Code | List]);

parse_meta_js_loop([#meta{name = Name, type = Type = tuple, explain = Explain, comment = Comment} | T], Depth, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),

    %% recursive
    SubCodes = parse_meta_js_loop(tuple_to_list(Explain), Depth + 1, []),

    %% format one field
    Code = lists:flatten(io_lib:format("~s{\"name\": \"~s\", \"type\": \"~s\", \"comment\": \"~ts\", \"explain\": [\n~ts\n~s]}", [Padding, word:to_lower_hump(Name), Type, Comment, SubCodes, Padding])),

    parse_meta_js_loop(T, Depth, [Code | List]);

parse_meta_js_loop([#meta{name = Name, type = Type = record, explain = Explain, comment = Comment} | T], Depth, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),

    SubExplain = [Meta || Meta = #meta{} <- tuple_to_list(Explain)],

    %% recursive
    SubCodes = parse_meta_js_loop(SubExplain, Depth + 1, []),

    %% format one field
    Code = lists:flatten(io_lib:format("~s{\"name\": \"~s\", \"type\": \"~s\", \"comment\": \"~ts\", \"explain\": [\n~ts\n~s]}", [Padding, word:to_lower_hump(Name), Type, Comment, SubCodes, Padding])),

    parse_meta_js_loop(T, Depth, [Code | List]);

parse_meta_js_loop([#meta{name = Name, type = Type = maps, explain = Explain, comment = Comment} | T], Depth, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),

    %% recursive
    SubCodes = parse_meta_js_loop(maps:values(Explain), Depth + 1, []),

    %% format one field
    Code = lists:flatten(io_lib:format("~s{\"name\": \"~s\", \"type\": \"~s\", \"comment\": \"~ts\", \"explain\": [\n~ts\n~s]}", [Padding, word:to_lower_hump(Name), Type, Comment, SubCodes, Padding])),

    parse_meta_js_loop(T, Depth, [Code | List]);

parse_meta_js_loop([#meta{name = Name, type = list, explain = Explain, comment = Comment, key = undefined} | T], Depth, List) ->
    %% recursive
    SubCodes = parse_meta_js_loop(Explain, Depth + 1, []),
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    Code = lists:flatten(io_lib:format("~s{\"name\": \"~s\", \"type\": \"~s\", \"comment\": \"~ts\", \"explain\": \n~ts\n~s}", [Padding, word:to_lower_hump(Name), list, Comment, SubCodes, Padding])),
    parse_meta_js_loop(T, Depth, [Code | List]);

parse_meta_js_loop([#meta{name = Name, type = list, explain = Explain, comment = Comment, key = Key} | T], Depth, List) ->
    %% recursive
    SubCodes = parse_meta_js_loop(Explain, Depth + 1, []),
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    Code = lists:flatten(io_lib:format("~s{\"name\": \"~s\", \"type\": \"~s\", \"comment\": \"~ts\", \"key\": \"~ts\", \"explain\": \n~ts\n~s}", [Padding, word:to_lower_hump(Name), map, Comment, word:to_lower_hump(Key), SubCodes, Padding])),
    parse_meta_js_loop(T, Depth, [Code | List]);

parse_meta_js_loop([#meta{name = Name, type = ets, explain = Explain, comment = Comment, key = undefined} | T], Depth, List) ->
    %% recursive
    SubCodes = parse_meta_js_loop(Explain, Depth + 1, []),
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    Code = lists:flatten(io_lib:format("~s{\"name\": \"~s\", \"type\": \"~s\", \"comment\": \"~ts\", \"explain\": \n~ts\n~s}", [Padding, word:to_lower_hump(Name), list, Comment, SubCodes, Padding])),
    parse_meta_js_loop(T, Depth, [Code | List]);

parse_meta_js_loop([#meta{name = Name, type = ets, explain = Explain, comment = Comment, key = Key} | T], Depth, List) ->
    %% recursive
    SubCodes = parse_meta_js_loop(Explain, Depth + 1, []),
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    Code = lists:flatten(io_lib:format("~s{\"name\": \"~s\", \"type\": \"~s\", \"comment\": \"~ts\", \"key\": \"~ts\", \"explain\": \n~ts\n~s}", [Padding, word:to_lower_hump(Name), map, Comment, word:to_lower_hump(Key), SubCodes, Padding])),
    parse_meta_js_loop(T, Depth, [Code | List]).

%%%===================================================================
%%% encode
%%%===================================================================
%% js code
parse_encode_js(Protocol, Meta = #meta{name = Name, type = Type}) ->
    %% start with 3 tabs(4 space) padding
    Padding = lists:duplicate(3, "    "),

    {_, Codes} = parse_encode_js_loop([Meta], 4, Type, Name, [], []),

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

parse_encode_js_loop([], _, _, _, Fields, List) ->
    %% construct as a list
    %% {string:join(lists:reverse(Fields), ", "), string:join(lists:reverse(List), "\n")};
    {lists:reverse(Fields), lists:reverse(List)};

parse_encode_js_loop([#meta{name = Name, type = binary, explain = Length, comment = Comment} | T], Depth, Parent, Scope, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Target = [["[\"", HumpName, "\"]"] || Parent == tuple orelse Parent == record orelse Parent == maps],
    Code = [
        Padding, "// extend", "\n",
        Padding, "while (view.byteLength < offset + ", integer_to_list(Length), ") {", "\n",
        Padding, "    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));", "\n",
        Padding, "    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));", "\n",
        Padding, "    view = extendView;", "\n",
        Padding, "}", "\n",
        Padding, "// ", Comment, "\n",
        Padding, "const ", HumpName, " = ", Scope, Target, ";", "\n",
        Padding, "(new Uint8Array(view.buffer, offset)).set(new Uint8Array(", HumpName, "));", "\n",
        Padding, "offset = offset + ", HumpName, ".byteLength;"
    ],
    parse_encode_js_loop(T, Depth, Parent, Scope, [HumpName | Fields], [Code | List]);

parse_encode_js_loop([#meta{name = Name, type = bool, explain = undefined, comment = Comment} | T], Depth, Parent, Scope, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Target = [["[\"", HumpName, "\"]"] || Parent == tuple orelse Parent == record orelse Parent == maps],
    Code = [
        Padding, "// extend", "\n",
        Padding, "while (view.byteLength < offset + 1) {", "\n",
        Padding, "    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));", "\n",
        Padding, "    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));", "\n",
        Padding, "    view = extendView;", "\n",
        Padding, "}", "\n",
        Padding, "// ", Comment, "\n",
        Padding, "view.setUint8(offset, ", Scope, Target, " ? 1 : 0, false);", "\n",
        Padding, "offset = offset + 1;"
    ],
    parse_encode_js_loop(T, Depth, Parent, Scope, [HumpName | Fields], [Code | List]);

parse_encode_js_loop([#meta{name = Name, type = u8, explain = undefined, comment = Comment} | T], Depth, Parent, Scope, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Target = [["[\"", HumpName, "\"]"] || Parent == tuple orelse Parent == record orelse Parent == maps],
    Code = [
        Padding, "// extend", "\n",
        Padding, "while (view.byteLength < offset + 1) {", "\n",
        Padding, "    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));", "\n",
        Padding, "    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));", "\n",
        Padding, "    view = extendView;", "\n",
        Padding, "}", "\n",
        Padding, "// ", Comment, "\n",
        Padding, "view.setUint8(offset, ", Scope, Target, ", false);", "\n",
        Padding, "offset = offset + 1;"
    ],
    parse_encode_js_loop(T, Depth, Parent, Scope, [HumpName | Fields], [Code | List]);

parse_encode_js_loop([#meta{name = Name, type = u16, explain = undefined, comment = Comment} | T], Depth, Parent, Scope, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Target = [["[\"", HumpName, "\"]"] || Parent == tuple orelse Parent == record orelse Parent == maps],
    Code = [
        Padding, "// extend", "\n",
        Padding, "while (view.byteLength < offset + 2) {", "\n",
        Padding, "    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));", "\n",
        Padding, "    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));", "\n",
        Padding, "    view = extendView;", "\n",
        Padding, "}", "\n",
        Padding, "// ", Comment, "\n",
        Padding, "view.setUint16(offset, ", Scope, Target, ", false);", "\n",
        Padding, "offset = offset + 2;"
    ],
    parse_encode_js_loop(T, Depth, Parent, Scope, [HumpName | Fields], [Code | List]);

parse_encode_js_loop([#meta{name = Name, type = u32, explain = undefined, comment = Comment} | T], Depth, Parent, Scope, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Target = [["[\"", HumpName, "\"]"] || Parent == tuple orelse Parent == record orelse Parent == maps],
    Code = [
        Padding, "// extend", "\n",
        Padding, "while (view.byteLength < offset + 4) {", "\n",
        Padding, "    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));", "\n",
        Padding, "    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));", "\n",
        Padding, "    view = extendView;", "\n",
        Padding, "}", "\n",
        Padding, "// ", Comment, "\n",
        Padding, "view.setUint32(offset, ", Scope, Target, ", false);", "\n",
        Padding, "offset = offset + 4;"
    ],
    parse_encode_js_loop(T, Depth, Parent, Scope, [HumpName | Fields], [Code | List]);

parse_encode_js_loop([#meta{name = Name, type = u64, explain = undefined, comment = Comment} | T], Depth, Parent, Scope, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Target = [["[\"", HumpName, "\"]"] || Parent == tuple orelse Parent == record orelse Parent == maps],
    Code = [
        Padding, "// extend", "\n",
        Padding, "while (view.byteLength < offset + 8) {", "\n",
        Padding, "    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));", "\n",
        Padding, "    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));", "\n",
        Padding, "    view = extendView;", "\n",
        Padding, "}", "\n",
        Padding, "// ", Comment, "\n",
        Padding, "view.setBigUint64(offset, ", Scope, Target, ", false);", "\n",
        Padding, "offset = offset + 8;"
    ],
    parse_encode_js_loop(T, Depth, Parent, Scope, [HumpName | Fields], [Code | List]);

parse_encode_js_loop([#meta{name = Name, type = i8, explain = undefined, comment = Comment} | T], Depth, Parent, Scope, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Target = [["[\"", HumpName, "\"]"] || Parent == tuple orelse Parent == record orelse Parent == maps],
    Code = [
        Padding, "// extend", "\n",
        Padding, "while (view.byteLength < offset + 1) {", "\n",
        Padding, "    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));", "\n",
        Padding, "    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));", "\n",
        Padding, "    view = extendView;", "\n",
        Padding, "}", "\n",
        Padding, "// ", Comment, "\n",
        Padding, "view.setInt8(offset, ", Scope, Target, ", false);", "\n",
        Padding, "offset = offset + 1;"
    ],
    parse_encode_js_loop(T, Depth, Parent, Scope, [HumpName | Fields], [Code | List]);

parse_encode_js_loop([#meta{name = Name, type = i16, explain = undefined, comment = Comment} | T], Depth, Parent, Scope, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Target = [["[\"", HumpName, "\"]"] || Parent == tuple orelse Parent == record orelse Parent == maps],
    Code = [
        Padding, "// extend", "\n",
        Padding, "while (view.byteLength < offset + 2) {", "\n",
        Padding, "    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));", "\n",
        Padding, "    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));", "\n",
        Padding, "    view = extendView;", "\n",
        Padding, "}", "\n",
        Padding, "// ", Comment, "\n",
        Padding, "view.setInt16(offset, ", Scope, Target, ", false);", "\n",
        Padding, "offset = offset + 2;"
    ],
    parse_encode_js_loop(T, Depth, Parent, Scope, [HumpName | Fields], [Code | List]);

parse_encode_js_loop([#meta{name = Name, type = i32, explain = undefined, comment = Comment} | T], Depth, Parent, Scope, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Target = [["[\"", HumpName, "\"]"] || Parent == tuple orelse Parent == record orelse Parent == maps],
    Code = [
        Padding, "// extend", "\n",
        Padding, "while (view.byteLength < offset + 4) {", "\n",
        Padding, "    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));", "\n",
        Padding, "    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));", "\n",
        Padding, "    view = extendView;", "\n",
        Padding, "}", "\n",
        Padding, "// ", Comment, "\n",
        Padding, "view.setInt32(offset, ", Scope, Target, ", false);", "\n",
        Padding, "offset = offset + 4;"
    ],
    parse_encode_js_loop(T, Depth, Parent, Scope, [HumpName | Fields], [Code | List]);

parse_encode_js_loop([#meta{name = Name, type = i64, explain = undefined, comment = Comment} | T], Depth, Parent, Scope, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Target = [["[\"", HumpName, "\"]"] || Parent == tuple orelse Parent == record orelse Parent == maps],
    Code = [
        Padding, "// extend", "\n",
        Padding, "while (view.byteLength < offset + 8) {", "\n",
        Padding, "    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));", "\n",
        Padding, "    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));", "\n",
        Padding, "    view = extendView;", "\n",
        Padding, "}", "\n",
        Padding, "// ", Comment, "\n",
        Padding, "view.setBigInt64(offset, ", Scope, Target, ", false);", "\n",
        Padding, "offset = offset + 8;"
    ],
    parse_encode_js_loop(T, Depth, Parent, Scope, [HumpName | Fields], [Code | List]);

parse_encode_js_loop([#meta{name = Name, type = f32, explain = undefined, comment = Comment} | T], Depth, Parent, Scope, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Target = [["[\"", HumpName, "\"]"] || Parent == tuple orelse Parent == record orelse Parent == maps],
    Code = [
        Padding, "// extend", "\n",
        Padding, "while (view.byteLength < offset + 4) {", "\n",
        Padding, "    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));", "\n",
        Padding, "    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));", "\n",
        Padding, "    view = extendView;", "\n",
        Padding, "}", "\n",
        Padding, "// ", Comment, "\n",
        Padding, "view.setFloat32(offset, ", Scope, Target, ", false);", "\n",
        Padding, "offset = offset + 4;"
    ],
    parse_encode_js_loop(T, Depth, Parent, Scope, [HumpName | Fields], [Code | List]);

parse_encode_js_loop([#meta{name = Name, type = f64, explain = undefined, comment = Comment} | T], Depth, Parent, Scope, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Target = [["[\"", HumpName, "\"]"] || Parent == tuple orelse Parent == record orelse Parent == maps],
    Code = [
        Padding, "// extend", "\n",
        Padding, "while (view.byteLength < offset + 8) {", "\n",
        Padding, "    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));", "\n",
        Padding, "    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));", "\n",
        Padding, "    view = extendView;", "\n",
        Padding, "}", "\n",
        Padding, "// ", Comment, "\n",
        Padding, "view.setFloat64(offset, ", Scope, Target, ", false);", "\n",
        Padding, "offset = offset + 8;"
    ],
    parse_encode_js_loop(T, Depth, Parent, Scope, [HumpName | Fields], [Code | List]);

parse_encode_js_loop([#meta{name = Name, type = str, explain = undefined, comment = Comment} | T], Depth, Parent, Scope, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Target = [["[\"", HumpName, "\"]"] || Parent == tuple orelse Parent == record orelse Parent == maps],
    Code = [
        Padding, "// extend", "\n",
        Padding, "while (view.byteLength < offset + 2) {", "\n",
        Padding, "    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));", "\n",
        Padding, "    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));", "\n",
        Padding, "    view = extendView;", "\n",
        Padding, "}", "\n",
        Padding, "// ", Comment, "\n",
        Padding, "const ", HumpName, "Array = textEncoder.encode(", Scope, Target, ");", "\n",
        Padding, "view.setUint16(offset, ", HumpName, "Array.length, false);", "\n",
        Padding, "offset = offset + 2;", "\n",
        Padding, "// extend", "\n",
        Padding, "while (view.byteLength < offset + ", HumpName, "Array.length) {", "\n",
        Padding, "    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));", "\n",
        Padding, "    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));", "\n",
        Padding, "    view = extendView;", "\n",
        Padding, "}", "\n",
        Padding, "(new Uint8Array(view.buffer, offset)).set(", HumpName, "Array);", "\n",
        Padding, "offset = offset + ", HumpName, "Array.length;"
    ],
    parse_encode_js_loop(T, Depth, Parent, Scope, [HumpName | Fields], [Code | List]);

parse_encode_js_loop([#meta{name = Name, type = bst, explain = undefined, comment = Comment} | T], Depth, Parent, Scope, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Target = [["[\"", HumpName, "\"]"] || Parent == tuple orelse Parent == record orelse Parent == maps],
    Code = [
        Padding, "// extend", "\n",
        Padding, "while (view.byteLength < offset + 2) {", "\n",
        Padding, "    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));", "\n",
        Padding, "    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));", "\n",
        Padding, "    view = extendView;", "\n",
        Padding, "}", "\n",
        Padding, "// ", Comment, "\n",
        Padding, "const ", HumpName, "Array = textEncoder.encode(", Scope, Target, ");", "\n",
        Padding, "view.setUint16(offset, ", HumpName, "Array.length, false);", "\n",
        Padding, "offset = offset + 2;", "\n",
        Padding, "// extend", "\n",
        Padding, "while (view.byteLength < offset + ", HumpName, "Array.length) {", "\n",
        Padding, "    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));", "\n",
        Padding, "    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));", "\n",
        Padding, "    view = extendView;", "\n",
        Padding, "}", "\n",
        Padding, "(new Uint8Array(view.buffer, offset)).set(", HumpName, "Array);", "\n",
        Padding, "offset = offset + ", HumpName, "Array.length;"
    ],
    parse_encode_js_loop(T, Depth, Parent, Scope, [HumpName | Fields], [Code | List]);

parse_encode_js_loop([#meta{name = Name, type = rst, explain = undefined, comment = Comment} | T], Depth, Parent, Scope, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Target = [["[\"", HumpName, "\"]"] || Parent == tuple orelse Parent == record orelse Parent == maps],
    Code = [
        Padding, "// extend", "\n",
        Padding, "while (view.byteLength < offset + 2) {", "\n",
        Padding, "    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));", "\n",
        Padding, "    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));", "\n",
        Padding, "    view = extendView;", "\n",
        Padding, "}", "\n",
        Padding, "// ", Comment, "\n",
        Padding, "const ", HumpName, "Array = textEncoder.encode(", Scope, Target, ");", "\n",
        Padding, "view.setUint16(offset, ", HumpName, "Array.length, false);", "\n",
        Padding, "offset = offset + 2;", "\n",
        Padding, "// extend", "\n",
        Padding, "while (view.byteLength < offset + ", HumpName, "Array.length) {", "\n",
        Padding, "    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));", "\n",
        Padding, "    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));", "\n",
        Padding, "    view = extendView;", "\n",
        Padding, "}", "\n",
        Padding, "(new Uint8Array(view.buffer, offset)).set(", HumpName, "Array);", "\n",
        Padding, "offset = offset + ", HumpName, "Array.length;"
    ],
    parse_encode_js_loop(T, Depth, Parent, Scope, [HumpName | Fields], [Code | List]);

parse_encode_js_loop([#meta{name = Name, type = tuple, explain = Explain} | T], Depth, Parent, Scope, Names, List) ->

    HumpName = word:to_lower_hump(Name),
    Target = [["[\"", HumpName, "\"]"] || Parent == tuple orelse Parent == record orelse Parent == maps],

    %% recursive
    {SubNames, SubCodes} = parse_encode_js_loop(tuple_to_list(Explain), Depth, tuple, lists:concat([Scope, Target]), Names, List),

    %% flatten
    parse_encode_js_loop(T, Depth, Parent, Scope, lists:reverse(SubNames), lists:reverse(SubCodes));

parse_encode_js_loop([#meta{name = Name, type = record, explain = Explain} | T], Depth, Parent, Scope, Names, List) ->

    HumpName = word:to_lower_hump(Name),
    Target = [["[\"", HumpName, "\"]"] || Parent == tuple orelse Parent == record orelse Parent == maps],

    SubExplain = [Meta || Meta = #meta{} <- tuple_to_list(Explain)],

    %% recursive
    {SubNames, SubCodes} = parse_encode_js_loop(SubExplain, Depth, record, lists:concat([Scope, Target]), Names, List),

    %% flatten
    parse_encode_js_loop(T, Depth, Parent, Scope, lists:reverse(SubNames), lists:reverse(SubCodes));

parse_encode_js_loop([#meta{name = Name, type = maps, explain = Explain} | T], Depth, Parent, Scope, Names, List) ->

    HumpName = word:to_lower_hump(Name),
    Target = [["[\"", HumpName, "\"]"] || Parent == tuple orelse Parent == record orelse Parent == maps],

    %% recursive
    {SubNames, SubCodes} = parse_encode_js_loop(maps:values(Explain), Depth, maps, lists:concat([Scope, Target]), Names, List),

    %% flatten
    parse_encode_js_loop(T, Depth, Parent, Scope, lists:reverse(SubNames), lists:reverse(SubCodes));

parse_encode_js_loop([#meta{name = Name, type = list, explain = Explain, comment = Comment, key = undefined} | T], Depth, Parent, Scope, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Target = [["[\"", HumpName, "\"]"] || Parent == tuple orelse Parent == record orelse Parent == maps],
    SubExplain = [Meta#meta{name = maps:get(SubName, #{[] => lists:concat([HumpName, "Item"])}, SubName)} || Meta = #meta{name = SubName} <- Explain],
    %% recursive
    {_, SubCodes} = parse_encode_js_loop(SubExplain, Depth + 1, list, lists:concat([HumpName, "DataItem"]), [], []),
    Code = [
        Padding, "// extend", "\n",
        Padding, "while (view.byteLength < offset + 2) {", "\n",
        Padding, "    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));", "\n",
        Padding, "    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));", "\n",
        Padding, "    view = extendView;", "\n",
        Padding, "}", "\n",
        Padding, "// ", Comment, "\n",
        Padding, "const ", HumpName, "Data = ", Scope, Target, ";", "\n",
        Padding, "view.setUint16(offset, ", HumpName, "Data.length, false);", "\n",
        Padding, "offset = offset + 2;", "\n",
        Padding, "for (const ", HumpName, "DataItem of ", HumpName, "Data) {", "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "}"
    ],
    parse_encode_js_loop(T, Depth, Parent, Scope, [HumpName, lists:concat([HumpName, "Length"]) | Fields], [Code | List]);

parse_encode_js_loop([#meta{name = Name, type = list, explain = Explain, comment = Comment, key = _} | T], Depth, Parent, Scope, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Target = [["[\"", HumpName, "\"]"] || Parent == tuple orelse Parent == record orelse Parent == maps],
    SubExplain = [Meta#meta{name = maps:get(SubName, #{[] => lists:concat([HumpName, "Item"])}, SubName)} || Meta = #meta{name = SubName} <- Explain],
    %% recursive
    {_, SubCodes} = parse_encode_js_loop(SubExplain, Depth + 1, list, lists:concat([HumpName, "DataItem"]), [], []),
    Code = [
        Padding, "// extend", "\n",
        Padding, "while (view.byteLength < offset + 2) {", "\n",
        Padding, "    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));", "\n",
        Padding, "    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));", "\n",
        Padding, "    view = extendView;", "\n",
        Padding, "}", "\n",
        Padding, "// ", Comment, "\n",
        Padding, "const ", HumpName, "Data = ", Scope, Target, ";", "\n",
        Padding, "view.setUint16(offset, Object.keys(", HumpName, "Data).length, false);", "\n",
        Padding, "offset = offset + 2;", "\n",
        Padding, "for (const ", HumpName, "DataKey in ", HumpName, "Data) {", "\n",
        Padding, "    ", "const ", HumpName, "DataItem = ", HumpName, "Data", "[", HumpName, "DataKey", "];", "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "}"
    ],
    parse_encode_js_loop(T, Depth, Parent, Scope, [HumpName, lists:concat([HumpName, "Length"]) | Fields], [Code | List]).

%%%===================================================================
%%% decode
%%%===================================================================
%% js code
parse_decode_js(Protocol, Meta) ->
    %% start with 3 tabs(4 space) padding
    Padding = lists:duplicate(3, "    "),

    {Fields, Codes} = parse_decode_js_loop([Meta], 4, [], []),

    %% codes format
    CodesDefault = lists:concat([
        Padding, "    ", "return ", string:join(Fields, ", "), ";", "\n"
    ]),
    CodesBlock = lists:append(Codes, [CodesDefault]),

    %% format one protocol define
    Code = lists:concat([
        Padding, "case ", Protocol, ": {", "\n",
        string:join(CodesBlock, "\n"),
        Padding, "}"
    ]),

    #file{function = Code}.

%% js code
parse_decode_js_loop([], _, Fields, List) ->
    %% construct as a list
    %% {string:join(lists:reverse(Fields), ", "), string:join(lists:reverse(List), "\n")};
    {lists:reverse(Fields), lists:reverse(List)};

parse_decode_js_loop([#meta{name = _, type = zero} | T], Depth, Fields, List) ->

    parse_decode_js_loop(T, Depth, Fields, List);

parse_decode_js_loop([#meta{name = Name, type = binary, explain = Length, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "const ", HumpName, " = view.buffer.slice(offset, offset + ", integer_to_list(Length), ");", "\n",
        Padding, "offset = offset + ", integer_to_list(Length), ";"
    ],
    parse_decode_js_loop(T, Depth, [HumpName | Fields], [Code | List]);

parse_decode_js_loop([#meta{name = Name, type = bool, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "const ", HumpName, " = view.getUint8(offset, false) !== 0;", "\n",
        Padding, "offset = offset + 1;"
    ],
    parse_decode_js_loop(T, Depth, [HumpName | Fields], [Code | List]);

parse_decode_js_loop([#meta{name = Name, type = u8, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "const ", HumpName, " = view.getUint8(offset, false);", "\n",
        Padding, "offset = offset + 1;"
    ],
    parse_decode_js_loop(T, Depth, [HumpName | Fields], [Code | List]);

parse_decode_js_loop([#meta{name = Name, type = u16, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "const ", HumpName, " = view.getUint16(offset, false);", "\n",
        Padding, "offset = offset + 2;"
    ],
    parse_decode_js_loop(T, Depth, [HumpName | Fields], [Code | List]);

parse_decode_js_loop([#meta{name = Name, type = u32, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "const ", HumpName, " = view.getUint32(offset, false);", "\n",
        Padding, "offset = offset + 4;"
    ],
    parse_decode_js_loop(T, Depth, [HumpName | Fields], [Code | List]);

parse_decode_js_loop([#meta{name = Name, type = u64, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "const ", HumpName, " = view.getBigUint64(offset, false);", "\n",
        Padding, "offset = offset + 8;"
    ],
    parse_decode_js_loop(T, Depth, [HumpName | Fields], [Code | List]);

parse_decode_js_loop([#meta{name = Name, type = i8, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "const ", HumpName, " = view.getInt8(offset, false);", "\n",
        Padding, "offset = offset + 1;"
    ],
    parse_decode_js_loop(T, Depth, [HumpName | Fields], [Code | List]);

parse_decode_js_loop([#meta{name = Name, type = i16, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "const ", HumpName, " = view.getInt16(offset, false);", "\n",
        Padding, "offset = offset + 2;"
    ],
    parse_decode_js_loop(T, Depth, [HumpName | Fields], [Code | List]);

parse_decode_js_loop([#meta{name = Name, type = i32, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "const ", HumpName, " = view.getInt32(offset, false);", "\n",
        Padding, "offset = offset + 4;"
    ],
    parse_decode_js_loop(T, Depth, [HumpName | Fields], [Code | List]);

parse_decode_js_loop([#meta{name = Name, type = i64, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "const ", HumpName, " = view.getBigInt64(offset, false);", "\n",
        Padding, "offset = offset + 8;"
    ],
    parse_decode_js_loop(T, Depth, [HumpName | Fields], [Code | List]);

parse_decode_js_loop([#meta{name = Name, type = f32, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "const ", HumpName, " = view.getFloat32(offset, false);", "\n",
        Padding, "offset = offset + 4;"
    ],
    parse_decode_js_loop(T, Depth, [HumpName | Fields], [Code | List]);

parse_decode_js_loop([#meta{name = Name, type = f64, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "const ", HumpName, " = view.getFloat64(offset, false);", "\n",
        Padding, "offset = offset + 8;"
    ],
    parse_decode_js_loop(T, Depth, [HumpName | Fields], [Code | List]);

parse_decode_js_loop([#meta{name = Name, type = str, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "const ", HumpName, "Length = view.getUint16(offset, false);", "\n",
        Padding, "offset = offset + 2;", "\n",
        Padding, "const ", HumpName, "Array = new Uint8Array(view.buffer.slice(offset, offset + ", HumpName, "Length));", "\n",
        Padding, "const ", HumpName, " = textDecoder.decode(", HumpName, "Array);", "\n",
        Padding, "offset = offset + ", HumpName, "Length;"
    ],
    parse_decode_js_loop(T, Depth, [HumpName | Fields], [Code | List]);

parse_decode_js_loop([#meta{name = Name, type = bst, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "const ", HumpName, "Length = view.getUint16(offset, false);", "\n",
        Padding, "offset = offset + 2;", "\n",
        Padding, "const ", HumpName, "Array = new Uint8Array(view.buffer.slice(offset, offset + ", HumpName, "Length));", "\n",
        Padding, "const ", HumpName, " = textDecoder.decode(", HumpName, "Array);", "\n",
        Padding, "offset = offset + ", HumpName, "Length;"
    ],
    parse_decode_js_loop(T, Depth, [HumpName | Fields], [Code | List]);

parse_decode_js_loop([#meta{name = Name, type = rst, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "const ", HumpName, "Length = view.getUint16(offset, false);", "\n",
        Padding, "offset = offset + 2;", "\n",
        Padding, "const ", HumpName, "Array = new Uint8Array(view.buffer.slice(offset, offset + ", HumpName, "Length));", "\n",
        Padding, "const ", HumpName, " = textDecoder.decode(", HumpName, "Array);", "\n",
        Padding, "offset = offset + ", HumpName, "Length;"
    ],
    parse_decode_js_loop(T, Depth, [HumpName | Fields], [Code | List]);

parse_decode_js_loop([#meta{name = Name, type = tuple, explain = Explain, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),

    %% recursive
    {SubFields, SubCodes} = parse_decode_js_loop(tuple_to_list(Explain), Depth, [], []),

    Code = [
        Padding, "// ", Comment, "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "// object", "\n",
        Padding, "const ", HumpName, " = {", string:join(SubFields, ", "), "};"
    ],

    %% flatten
    parse_decode_js_loop(T, Depth, [HumpName | Fields], [Code | List]);

parse_decode_js_loop([#meta{name = Name, type = record, explain = Explain, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),

    SubExplain = [Meta || Meta = #meta{} <- tuple_to_list(Explain)],

    %% recursive
    {SubFields, SubCodes} = parse_decode_js_loop(SubExplain, Depth, [], []),

    Code = [
        Padding, "// ", Comment, "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "// object", "\n",
        Padding, "const ", HumpName, " = {", string:join(SubFields, ", "), "};"
    ],

    %% flatten
    parse_decode_js_loop(T, Depth, [HumpName | Fields], [Code | List]);

parse_decode_js_loop([#meta{name = Name, type = maps, explain = Explain, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),

    %% recursive
    {SubFields, SubCodes} = parse_decode_js_loop(maps:values(Explain), Depth, [], []),

    Code = [
        Padding, "// ", Comment, "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "// object", "\n",
        Padding, "const ", HumpName, " = {", string:join(SubFields, ", "), "};"
    ],

    %% flatten
    parse_decode_js_loop(T, Depth, [HumpName | Fields], [Code | List]);

parse_decode_js_loop([#meta{name = Name, type = list, explain = Explain, comment = Comment, key = undefined} | T], Depth, Fields, List)  ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    SubExplain = [Meta#meta{name = maps:get(SubName, #{[] => lists:concat([HumpName, "Item"])}, SubName)} || Meta = #meta{name = SubName} <- Explain],
    %% recursive
    {SubFields, SubCodes} = parse_decode_js_loop(SubExplain, Depth + 1, [], []),
    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "const ", HumpName, " = [];", "\n",
        Padding, "let ", HumpName, "Length = view.getUint16(offset, false);", "\n",
        Padding, "offset = offset + 2;", "\n",
        Padding, "while (--", HumpName, "Length >= 0) {", "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "    // add", "\n",
        Padding, "    ", HumpName, ".push(", string:join(SubFields, ", "), ");", "\n",
        Padding, "}"
    ],
    parse_decode_js_loop(T, Depth, [HumpName | Fields], [Code | List]);

parse_decode_js_loop([#meta{name = Name, type = list, explain = Explain, comment = Comment, key = Key} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    SubExplain = [Meta#meta{name = maps:get(SubName, #{[] => lists:concat([HumpName, "Item"])}, SubName)} || Meta = #meta{name = SubName} <- Explain],
    %% recursive
    {SubFields, SubCodes} = parse_decode_js_loop(SubExplain, Depth + 1, [], []),
    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "const ", HumpName, " = {};", "\n",
        Padding, "let ", HumpName, "Length = view.getUint16(offset, false);", "\n",
        Padding, "offset = offset + 2;", "\n",
        Padding, "while (--", HumpName, "Length >= 0) {", "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "    // add", "\n",
        Padding, "    ", HumpName, "[", word:to_lower_hump(Key), "] = ", string:join(SubFields, ", "), ";", "\n",
        Padding, "}"
    ],
    parse_decode_js_loop(T, Depth, [HumpName | Fields], [Code | List]);

parse_decode_js_loop([#meta{name = Name, type = ets, explain = Explain, comment = Comment, key = []} | T], Depth, Fields, List)  ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    SubExplain = [Meta#meta{name = maps:get(SubName, #{[] => lists:concat([HumpName, "Item"])}, SubName)} || Meta = #meta{name = SubName} <- Explain],
    %% recursive
    {SubFields, SubCodes} = parse_decode_js_loop(SubExplain, Depth + 1, [], []),
    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "const ", HumpName, " = [];", "\n",
        Padding, "let ", HumpName, "Length = view.getUint16(offset, false);", "\n",
        Padding, "offset = offset + 2;", "\n",
        Padding, "while (--", HumpName, "Length >= 0) {", "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "    // add", "\n",
        Padding, "    ", HumpName, ".push(", string:join(SubFields, ", "), ");", "\n",
        Padding, "}"
    ],
    parse_decode_js_loop(T, Depth, [HumpName | Fields], [Code | List]);

parse_decode_js_loop([#meta{name = Name, type = ets, explain = Explain, comment = Comment, key = [Key]} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    SubExplain = [Meta#meta{name = maps:get(SubName, #{[] => lists:concat([HumpName, "Item"])}, SubName)} || Meta = #meta{name = SubName} <- Explain],
    %% recursive
    {SubFields, SubCodes} = parse_decode_js_loop(SubExplain, Depth + 1, [], []),
    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "const ", HumpName, " = {};", "\n",
        Padding, "let ", HumpName, "Length = view.getUint16(offset, false);", "\n",
        Padding, "offset = offset + 2;", "\n",
        Padding, "while (--", HumpName, "Length >= 0) {", "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "    // add", "\n",
        Padding, "    ", HumpName, "[", word:to_lower_hump(Key), "] = ", string:join(SubFields, ", "), ";", "\n",
        Padding, "}"
    ],
    parse_decode_js_loop(T, Depth, [HumpName | Fields], [Code | List]).
