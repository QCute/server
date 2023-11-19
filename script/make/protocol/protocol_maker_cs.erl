%%%-------------------------------------------------------------------
%%% @doc
%%% make protocol define to cs io/metadata code
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_maker_cs).
-export([format_code/3, format_meta/2]).
-export([parse_meta/2]).
-export([parse_encode_class/4, parse_decode_class/4]).
-export([parse_encode/4, parse_decode/4]).
-include("../../../include/serialize.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
format_code(Name, EncodeList, DecodeList) ->
    %% export
    ImportExport = [[Import, "\n", "\n", Export] || {#data{cs = #set{code = #file{import = Import}}}, #data{cs = #set{code = #file{export = Export}}}} <- lists:zip(EncodeList, DecodeList), Import =/= [] orelse Export =/= []],

    %% encode
    Encode = [Encode || #data{cs = #set{code = #file{function = Encode}}} <- EncodeList, Encode =/= []],
    EncodeDefault = lists:concat([
        "    ", "    ", "    ", "default:throw new System.ArgumentException(System.String.Format(\"unknown protocol define: {0}\", protocol));", "\n"
    ]),
    EncodeCode = lists:append(Encode, [EncodeDefault]),

    %% decode
    Decode = [Decode || #data{cs = #set{code = #file{function = Decode}}} <- DecodeList, Decode =/= []],
    DecodeDefault = lists:concat([
        "    ", "    ", "    ", "default:throw new System.ArgumentException(System.String.Format(\"unknown protocol define: {0}\", protocol));", "\n"
    ]),
    DecodeCode = lists:append(Decode, [DecodeDefault]),

    lists:concat([
        string:join(ImportExport, "\n\n"),
        "\n",
        "\n",
        "public static class ", Name, "\n"
        "{", "\n",
        "    ", "public static void Encode(System.Text.Encoding encoding, System.IO.BinaryWriter writer, System.UInt16 protocol, System.Object dataRaw) ", "\n",
        "    ", "{", "\n",
        "    ", "    ", "switch (protocol) ", "\n",
        "    ", "    ", "{", "\n",
        string:join(EncodeCode, "\n"),
        "    ", "    ", "}", "\n",
        "    ", "}", "\n\n",
        "    ", "public static System.Object Decode(System.Text.Encoding encoding, System.IO.BinaryReader reader, System.UInt16 protocol) ", "\n",
        "    ", "{", "\n",
        "    ", "    ", "switch (protocol) ", "\n",
        "    ", "    ", "{", "\n",
        string:join(DecodeCode, "\n"),
        "    ", "    ", "}", "\n",
        "    ", "}", "\n",
        "}"
    ]).

format_meta(Name, List) ->
    CsMetaInner = string:join([lists:concat([
        "    ", "    ", "    ", "{\"", Protocol, "\", new Map() {\n",
        "    ", "    ", "    ", "    ", "{\"comment\", \"", Comment, "\"},", "\n",
        "    ", "    ", "    ", "    ", "{\"write\", ", Read, "},", "\n",
        "    ", "    ", "    ", "    ", "{\"read\", ", Write, "}", "\n",
        "    ", "    ", "    ", "}}"
    ]) || {Protocol, Comment, #data{cs = #set{meta = #file{extra = Read}}}, #data{cs = #set{meta = #file{extra = Write}}}} <- List, Protocol =/= 0], ",\n"),
    lists:concat([
        "using List = System.Collections.Generic.List<System.Object>;", "\n",
        "using Map = System.Collections.Generic.Dictionary<System.String, System.Object>;", "\n",
        "\n",
        "public static class ", Name, "\n", "{", "\n",
        "    ", "public static Map GetMeta()", "\n",
        "    ", "{", "\n",
        "    ", "    ", "return new Map()", "\n",
        "    ", "    ", "{\n",
        CsMetaInner, "\n",
        "    ", "    ", "};", "\n",
        "    ", "}", "\n",
        "}"
    ]).

%%%===================================================================
%%% meta
%%%===================================================================
%% meta
parse_meta(_, Meta) ->
    %% start with 1 tabs(4 space) padding
    %% Padding = lists:duplicate(3, "    "),
    Code = parse_meta_loop([Meta], 4, []),
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
    Code = lists:flatten(io_lib:format("~snew Map() { {\"name\", \"~s\"}, {\"type\", \"~s\"}, {\"comment\", \"~ts\"}, {\"explain\", ~w} }", [Padding, word:to_lower_hump(Name), binary, Comment, Length])),
    parse_meta_loop(T, Depth, [Code | List]);

parse_meta_loop([#meta{name = Name, type = Type, explain = undefined, comment = Comment} | T], Depth, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    Code = lists:flatten(io_lib:format("~snew Map() { {\"name\", \"~s\"}, {\"type\", \"~s\"}, {\"comment\", \"~ts\"}, {\"explain\", new List()} }", [Padding, word:to_lower_hump(Name), Type, Comment])),
    parse_meta_loop(T, Depth, [Code | List]);

parse_meta_loop([#meta{name = Name, type = tuple, explain = Explain, comment = Comment} | T], Depth, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),

    %% recursive
    SubCodes = parse_meta_loop(tuple_to_list(Explain), Depth + 1, []),

    %% format one field
    Code = lists:flatten(io_lib:format("~snew Map() { {\"name\", \"~s\"}, {\"type\", \"~s\"}, {\"comment\", \"~ts\"}, {\"explain\", new List() {\n~ts\n~s}}}", [Padding, word:to_lower_hump(Name), map, Comment, SubCodes, Padding])),

    parse_meta_loop(T, Depth, [Code | List]);

parse_meta_loop([#meta{name = Name, type = record, explain = Explain, comment = Comment} | T], Depth, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),

    SubExplain = [SubMeta || SubMeta = #meta{} <- tuple_to_list(Explain)],

    %% recursive
    SubCodes = parse_meta_loop(SubExplain, Depth + 1, []),

    %% format one field
    Code = lists:flatten(io_lib:format("~snew Map() { {\"name\", \"~s\"}, {\"type\", \"~s\"}, {\"comment\", \"~ts\"}, {\"explain\", new List() {\n~ts\n~s}}}", [Padding, word:to_lower_hump(Name), map, Comment, SubCodes, Padding])),

    parse_meta_loop(T, Depth, [Code | List]);

parse_meta_loop([#meta{name = Name, type = maps, explain = Explain, comment = Comment} | T], Depth, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),

    %% recursive
    SubCodes = parse_meta_loop(maps:values(Explain), Depth + 1, []),

    %% format one field
    Code = lists:flatten(io_lib:format("~snew Map() { {\"name\", \"~s\"}, {\"type\", \"~s\"}, {\"comment\", \"~ts\"}, {\"explain\", new List() {\n~ts\n~s}}}", [Padding, word:to_lower_hump(Name), map, Comment, SubCodes, Padding])),

    parse_meta_loop(T, Depth, [Code | List]);

parse_meta_loop([#meta{name = Name, type = list, explain = Explain, comment = Comment, key = undefined} | T], Depth, List) ->
    %% recursive
    SubCodes = parse_meta_loop(Explain, Depth + 1, []),
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    Code = lists:flatten(io_lib:format("~snew Map() { {\"name\", \"~s\"}, {\"type\", \"~s\"}, {\"comment\", \"~ts\"}, {\"explain\", new List() {\n~ts\n~s}}}", [Padding, word:to_lower_hump(Name), list, Comment, SubCodes, Padding])),
    parse_meta_loop(T, Depth, [Code | List]);

parse_meta_loop([#meta{name = Name, type = list, explain = Explain, comment = Comment, key = Key} | T], Depth, List) ->
    %% recursive
    SubCodes = parse_meta_loop(Explain, Depth + 1, []),
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    Code = lists:flatten(io_lib:format("~snew Map() { {\"name\", \"~s\"}, {\"type\", \"~s\"}, {\"comment\", \"~ts\"}, {\"key\", \"~ts\"}, {\"explain\", new List() {\n~ts\n~s}}}", [Padding, word:to_lower_hump(Name), list, Comment, word:to_lower_hump(Key), SubCodes, Padding])),
    parse_meta_loop(T, Depth, [Code | List]);

parse_meta_loop([#meta{name = Name, type = ets, explain = Explain, comment = Comment, key = undefined} | T], Depth, List) ->
    %% recursive
    SubCodes = parse_meta_loop(Explain, Depth + 1, []),
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    Code = lists:flatten(io_lib:format("~snew Map() { {\"name\", \"~s\"}, {\"type\", \"~s\"}, {\"comment\", \"~ts\"}, {\"explain\", new List() {\n~ts\n~s}}}", [Padding, word:to_lower_hump(Name), list, Comment, SubCodes, Padding])),
    parse_meta_loop(T, Depth, [Code | List]);

parse_meta_loop([#meta{name = Name, type = ets, explain = Explain, comment = Comment, key = Key} | T], Depth, List) ->
    %% recursive
    SubCodes = parse_meta_loop(Explain, Depth + 1, []),
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    Code = lists:flatten(io_lib:format("~snew Map() { {\"name\", \"~s\"}, {\"type\", \"~s\"}, {\"comment\", \"~ts\"}, {\"key\", \"~ts\"}, {\"explain\", new List() {\n~ts\n~s}}}", [Padding, word:to_lower_hump(Name), list, Comment, word:to_lower_hump(Key), SubCodes, Padding])),
    parse_meta_loop(T, Depth, [Code | List]).


%%%===================================================================
%%% encode class
%%%===================================================================
%% code
parse_encode_class(_, #meta{}, undefined, _) ->
    #file{};
parse_encode_class(Protocol, Meta, #handler{module = Module, function = Function, alias = Alias}, Name) ->

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
        "public class ", string:replace(word:to_hump(Name), "Protocol", ""), word:to_hump(Sub), "Request", "\n",
        "{", "\n",
        "    ", "public System.UInt16 protocol = ", Protocol, ";", "\n",
        "    ", "public ", string:join(Codes, ""), " ", "data", ";", "\n",
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
        "public class ", string:replace(word:to_hump(Name), "Protocol", ""), word:to_hump(Sub), "Response", "\n",
        "{", "\n",
        "    ", "public System.UInt16 protocol = ", Protocol, ";", "\n",
        "    ", "public ", string:join(Codes, ""), " ", "data", ";", "\n",
        "}"
    ]),

    #file{export = Code}.


parse_class_loop([], _, Fields, List) ->
    %% construct as a list
    {lists:reverse(Fields), lists:reverse(List)};

parse_class_loop([#meta{type = zero} | T], Depth, Fields, List) ->

    parse_class_loop(T, Depth, Fields, List);

parse_class_loop([Meta = #meta{type = binary} | T], Depth, Fields, List) ->

    parse_class_loop(T, Depth, [Meta | Fields], ["System.Byte[]" | List]);

parse_class_loop([Meta = #meta{type = bool, explain = undefined} | T], Depth, Fields, List) ->

    parse_class_loop(T, Depth, [Meta | Fields], ["System.Boolean" | List]);

parse_class_loop([Meta = #meta{type = u8, explain = undefined} | T], Depth, Fields, List) ->

    parse_class_loop(T, Depth, [Meta | Fields], ["System.Byte" | List]);

parse_class_loop([Meta = #meta{type = u16, explain = undefined} | T], Depth, Fields, List) ->

    parse_class_loop(T, Depth, [Meta | Fields], ["System.UInt16" | List]);

parse_class_loop([Meta = #meta{type = u32, explain = undefined} | T], Depth, Fields, List) ->

    parse_class_loop(T, Depth, [Meta | Fields], ["System.UInt32" | List]);

parse_class_loop([Meta = #meta{type = u64, explain = undefined} | T], Depth, Fields, List) ->

    parse_class_loop(T, Depth, [Meta | Fields], ["System.UInt64" | List]);

parse_class_loop([Meta = #meta{type = i8, explain = undefined} | T], Depth, Fields, List) ->

    parse_class_loop(T, Depth, [Meta | Fields], ["System.SByte" | List]);

parse_class_loop([Meta = #meta{type = i16, explain = undefined} | T], Depth, Fields, List) ->

    parse_class_loop(T, Depth, [Meta | Fields], ["System.Int16" | List]);

parse_class_loop([Meta = #meta{type = i32, explain = undefined} | T], Depth, Fields, List) ->

    parse_class_loop(T, Depth, [Meta | Fields], ["System.Int32" | List]);

parse_class_loop([Meta = #meta{type = i64, explain = undefined} | T], Depth, Fields, List) ->

    parse_class_loop(T, Depth, [Meta | Fields], ["System.Int64" | List]);

parse_class_loop([Meta = #meta{type = f32, explain = undefined} | T], Depth, Fields, List) ->

    parse_class_loop(T, Depth, [Meta | Fields], ["System.Single" | List]);

parse_class_loop([Meta = #meta{type = f64, explain = undefined} | T], Depth, Fields, List) ->

    parse_class_loop(T, Depth, [Meta | Fields], ["System.Double" | List]);

parse_class_loop([Meta = #meta{type = str, explain = undefined} | T], Depth, Fields, List) ->

    parse_class_loop(T, Depth, [Meta | Fields], ["System.String" | List]);

parse_class_loop([Meta = #meta{type = bst, explain = undefined} | T], Depth, Fields, List) ->

    parse_class_loop(T, Depth, [Meta | Fields], ["System.String" | List]);

parse_class_loop([Meta = #meta{type = ast, explain = undefined} | T], Depth, Fields, List) ->

    parse_class_loop(T, Depth, [Meta | Fields], ["System.String" | List]);

parse_class_loop([Meta = #meta{type = tuple, explain = Explain} | T], Depth, Names, List) ->
    %% alignment padding
    IndentPadding = lists:duplicate(Depth - 1, "    "),
    Padding = lists:duplicate(Depth, "    "),

    SubExplain = tuple_to_list(Explain),

    %% recursive
    {SubNames, SubCodes} = parse_class_loop(SubExplain, Depth + 1, [], []),

    NameEmpty = [#meta{name = "_"} || length(SubNames) == 1],
    CodeEmpty = ["Empty" || length(SubCodes) == 1],
    SubCode = fun
        Loop([#meta{name = SubName, comment = SubComment} | X], [SubCode | Y], Acc, Position) ->
            %% trim last comma
            Comma = ["," || X =/= [] andalso Y =/= []],
            %% the whole data
            Line = lists:flatten(lists:concat(["\n", "    ", Padding, SubCode, " ", word:to_lower_hump(SubName), Comma])),
            %% get the last line
            LastLine = lists:last(string:tokens(Line, "\n")),
            %% calc padding
            SubPadding = lists:duplicate(max(0, 80 - lists:flatlength(LastLine)), " "),
            Data = lists:concat([Line, SubPadding, "// ", SubComment]),
            Loop(X, Y, [Data | Acc], Position + 1);
        Loop([], [], Acc, _) ->
            Acc
    end(SubNames ++ NameEmpty, SubCodes ++ CodeEmpty, [], 1),

    case SubCode of
        [] ->
            Code = [
                "Empty"
            ];
        _ ->
            Code = [
                "(",
                lists:reverse(SubCode),
                "\n",
                "    ", IndentPadding, ")"
            ]
    end,

    %% flatten
    parse_class_loop(T, Depth, [Meta | Names], [Code | List]);

parse_class_loop([Meta = #meta{type = record, explain = Explain} | T], Depth, Names, List) ->
    %% alignment padding
    IndentPadding = lists:duplicate(Depth - 1, "    "),
    Padding = lists:duplicate(Depth, "    "),

    SubExplain = [SubMeta || SubMeta = #meta{} <- tuple_to_list(Explain)],

    %% recursive
    {SubNames, SubCodes} = parse_class_loop(SubExplain, Depth + 1, [], []),

    NameEmpty = [#meta{name = "_"} || length(SubNames) == 1],
    CodeEmpty = ["Empty" || length(SubCodes) == 1],
    SubCode = fun
        Loop([#meta{name = SubName, comment = SubComment} | X], [SubCode | Y], Acc, Position) ->
            %% trim last comma
            Comma = ["," || X =/= [] andalso Y =/= []],
            %% the whole data
            Line = lists:flatten(lists:concat(["\n", "    ", Padding, SubCode, " ", word:to_lower_hump(SubName), Comma])),
            %% get the last line
            LastLine = lists:last(string:tokens(Line, "\n")),
            %% calc padding
            SubPadding = lists:duplicate(max(0, 80 - lists:flatlength(LastLine)), " "),
            Data = lists:concat([Line, SubPadding, "// ", SubComment]),
            Loop(X, Y, [Data | Acc], Position + 1);
        Loop([], [], Acc, _) ->
            Acc
    end(SubNames ++ NameEmpty, SubCodes ++ CodeEmpty, [], 1),

    case SubCode of
        [] ->
            Code = [
                "Empty"
            ];
        _ ->
            Code = [
                "(",
                lists:reverse(SubCode),
                "\n",
                "    ", IndentPadding, ")"
            ]
    end,

    %% flatten
    parse_class_loop(T, Depth, [Meta | Names], [Code | List]);

parse_class_loop([Meta = #meta{type = maps, explain = Explain} | T], Depth, Names, List) ->
    %% alignment padding
    IndentPadding = lists:duplicate(Depth - 1, "    "),
    Padding = lists:duplicate(Depth, "    "),

    SubExplain = maps:values(Explain),

    %% recursive
    {SubNames, SubCodes} = parse_class_loop(SubExplain, Depth + 1, [], []),

    NameEmpty = [#meta{name = "_"} || length(SubNames) == 1],
    CodeEmpty = ["Empty" || length(SubCodes) == 1],
    SubCode = fun
        Loop([#meta{name = SubName, comment = SubComment} | X], [SubCode | Y], Acc, Position) ->
            %% trim last comma
            Comma = ["," || X =/= [] andalso Y =/= []],
            %% the whole data
            Line = lists:flatten(lists:concat(["\n", "    ", Padding, SubCode, " ", word:to_lower_hump(SubName), Comma])),
            %% get the last line
            LastLine = lists:last(string:tokens(Line, "\n")),
            %% calc padding
            SubPadding = lists:duplicate(max(0, 80 - lists:flatlength(LastLine)), " "),
            Data = lists:concat([Line, SubPadding, "// ", SubComment]),
            Loop(X, Y, [Data | Acc], Position + 1);
        Loop([], [], Acc, _) ->
            Acc
    end(SubNames ++ NameEmpty, SubCodes ++ CodeEmpty, [], 1),

    case SubCode of
        [] ->
            Code = [
                "Empty"
            ];
        _ ->
            Code = [
                "(",
                lists:reverse(SubCode),
                "\n",
                "    ", IndentPadding, ")"
            ]
    end,

    %% flatten
    parse_class_loop(T, Depth, [Meta | Names], [Code | List]);

parse_class_loop([Meta = #meta{type = list, explain = Explain, key = undefined} | T], Depth, Fields, List) ->
    %% alignment padding
    %% IndentPadding = lists:duplicate(Depth - 1, "    "),
    %% Padding = lists:duplicate(Depth, "    "),

    %% recursive
    {_, SubCodes} = parse_class_loop(Explain, Depth, [], []),

    Code = [
        "System.Collections.Generic.List<", string:join(SubCodes, ""), ">"
    ],

    parse_class_loop(T, Depth, [Meta | Fields], [Code | List]);

parse_class_loop([Meta = #meta{type = list, explain = Explain, key = _} | T], Depth, Fields, List) ->
    %% alignment padding
    %% IndentPadding = lists:duplicate(Depth - 1, "    "),
    %% Padding = lists:duplicate(Depth, "    "),

    %% recursive
    {_, SubCodes} = parse_class_loop(Explain, Depth, [], []),

    Code = [
        "System.Collections.Generic.Dictionary<System.Object, ", string:join(SubCodes, ""), ">"
    ],

    parse_class_loop(T, Depth, [Meta | Fields], [Code | List]);

parse_class_loop([Meta = #meta{type = ets, explain = Explain, key = []} | T], Depth, Fields, List) ->
    %% alignment padding
    %% IndentPadding = lists:duplicate(Depth - 1, "    "),
    %% Padding = lists:duplicate(Depth, "    "),

    %% recursive
    {_, SubCodes} = parse_class_loop(Explain, Depth, [], []),

    Code = [
        "System.Collections.Generic.List<", string:join(SubCodes, ""), ">"
    ],

    parse_class_loop(T, Depth, [Meta | Fields], [Code | List]);

parse_class_loop([Meta = #meta{type = ets, explain = Explain, key = _} | T], Depth, Fields, List) ->
    %% alignment padding
    %% IndentPadding = lists:duplicate(Depth - 1, "    "),
    %% Padding = lists:duplicate(Depth, "    "),

    %% recursive
    {_, SubCodes} = parse_class_loop(Explain, Depth, [], []),

    Code = [
        "System.Collections.Generic.Dictionary<System.Object, ", string:join(SubCodes, ""), ">"
    ],

    parse_class_loop(T, Depth, [Meta | Fields], [Code | List]).

%%%===================================================================
%%% encode
%%%===================================================================
%% code
parse_encode(Protocol, Meta = #meta{}, #handler{}, _) ->
    %% start with 3 tabs(4 space) padding
    Padding = lists:duplicate(3, "    "),

    {_, SubTypes, Codes} = parse_encode_loop([Meta], 4, [], [], []),

    %% codes format
    CodesDefault = lists:concat([
        Padding, "    ", "return;", "\n"
    ]),
    CodesBlock = lists:append(Codes, [CodesDefault]),

    %% format one protocol define
    Code = lists:concat([
        Padding, "case ", Protocol, ":", "\n",
        Padding, "{", "\n",
        [[Padding, "    ", "#pragma warning disable", "\n"] || lists:flatlength(Codes) =:= 0],
        Padding, "    ", "var", " ", "data = (", string:join(SubTypes, ""), ")dataRaw", ";", "\n",
        [[Padding, "    ", "#pragma warning restore", "\n"] || lists:flatlength(Codes) =:= 0],
        string:join(CodesBlock, "\n"),
        Padding, "}"
    ]),

    #file{function = Code}.

parse_encode_loop([], _, Fields, Type, List) ->
    %% construct as a list
    {lists:reverse(Fields), lists:reverse(Type), lists:reverse(List)};

parse_encode_loop([#meta{path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = binary, explain = _, comment = Comment} | T], Depth, Fields, Type, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),
    PathGetName = [word:to_lower_hump(Prefix), word:to_hump(Suffix), ["." || Get =/= []], word:to_lower_hump(Get)],

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "writer.Write((System.Byte[])", PathGetName, ");"
    ],
    parse_encode_loop(T, Depth, [PathHumpName | Fields], ["System.Byte[]" | Type], [Code | List]);

parse_encode_loop([#meta{path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = bool, explain = undefined, comment = Comment} | T], Depth, Fields, Type, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),
    PathGetName = [word:to_lower_hump(Prefix), word:to_hump(Suffix), ["." || Get =/= []], word:to_lower_hump(Get)],

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "writer.Write((System.Byte)((System.Boolean)", PathGetName, " ? 1 : 0));"
    ],
    parse_encode_loop(T, Depth, [PathHumpName | Fields], ["System.Boolean" | Type], [Code | List]);

parse_encode_loop([#meta{path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = u8, explain = undefined, comment = Comment} | T], Depth, Fields, Type, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),
    PathGetName = [word:to_lower_hump(Prefix), word:to_hump(Suffix), ["." || Get =/= []], word:to_lower_hump(Get)],

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "writer.Write((System.Byte)", PathGetName, ");"
    ],
    parse_encode_loop(T, Depth, [PathHumpName | Fields], ["System.Byte" | Type], [Code | List]);

parse_encode_loop([#meta{path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = u16, explain = undefined, comment = Comment} | T], Depth, Fields, Type, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),
    PathGetName = [word:to_lower_hump(Prefix), word:to_hump(Suffix), ["." || Get =/= []], word:to_lower_hump(Get)],

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)(System.UInt16)", PathGetName, "));"
    ],
    parse_encode_loop(T, Depth, [PathHumpName | Fields], ["System.UInt16" | Type], [Code | List]);

parse_encode_loop([#meta{path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = u32, explain = undefined, comment = Comment} | T], Depth, Fields, Type, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),
    PathGetName = [word:to_lower_hump(Prefix), word:to_hump(Suffix), ["." || Get =/= []], word:to_lower_hump(Get)],

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int32)(System.UInt32)", PathGetName, "));"
    ],
    parse_encode_loop(T, Depth, [PathHumpName | Fields], ["System.UInt32" | Type], [Code | List]);

parse_encode_loop([#meta{path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = u64, explain = undefined, comment = Comment} | T], Depth, Fields, Type, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),
    PathGetName = [word:to_lower_hump(Prefix), word:to_hump(Suffix), ["." || Get =/= []], word:to_lower_hump(Get)],

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)(System.UInt64)", PathGetName, "));"
    ],
    parse_encode_loop(T, Depth, [PathHumpName | Fields], ["System.UInt64" | Type], [Code | List]);

parse_encode_loop([#meta{path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = i8, explain = undefined, comment = Comment} | T], Depth, Fields, Type, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),
    PathGetName = [word:to_lower_hump(Prefix), word:to_hump(Suffix), ["." || Get =/= []], word:to_lower_hump(Get)],

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "writer.Write((System.SByte)", PathGetName, ");"
    ],
    parse_encode_loop(T, Depth, [PathHumpName | Fields], ["System.SByte" | Type], [Code | List]);

parse_encode_loop([#meta{path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = i16, explain = undefined, comment = Comment} | T], Depth, Fields, Type, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),
    PathGetName = [word:to_lower_hump(Prefix), word:to_hump(Suffix), ["." || Get =/= []], word:to_lower_hump(Get)],

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)", PathGetName, "));"
    ],
    parse_encode_loop(T, Depth, [PathHumpName | Fields], ["System.Int16" | Type], [Code | List]);

parse_encode_loop([#meta{path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = i32, explain = undefined, comment = Comment} | T], Depth, Fields, Type, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),
    PathGetName = [word:to_lower_hump(Prefix), word:to_hump(Suffix), ["." || Get =/= []], word:to_lower_hump(Get)],

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int32)", PathGetName, "));"
    ],
    parse_encode_loop(T, Depth, [PathHumpName | Fields], ["System.Int32" | Type], [Code | List]);

parse_encode_loop([#meta{path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = i64, explain = undefined, comment = Comment} | T], Depth, Fields, Type, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),
    PathGetName = [word:to_lower_hump(Prefix), word:to_hump(Suffix), ["." || Get =/= []], word:to_lower_hump(Get)],

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)", PathGetName, "));"
    ],
    parse_encode_loop(T, Depth, [PathHumpName | Fields], ["System.Int64" | Type], [Code | List]);

parse_encode_loop([#meta{path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = f32, explain = undefined, comment = Comment} | T], Depth, Fields, Type, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),
    PathGetName = [word:to_lower_hump(Prefix), word:to_hump(Suffix), ["." || Get =/= []], word:to_lower_hump(Get)],

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", PathHumpName, "Bytes = System.BitConverter.GetBytes((System.Single)", PathGetName, ");", "\n",
        Padding, "if (System.BitConverter.IsLittleEndian) System.Array.Reverse(", PathHumpName, "Bytes);", "\n",
        Padding, "writer.Write(", PathHumpName, "Bytes);"
    ],
    parse_encode_loop(T, Depth, [PathHumpName | Fields], ["System.Single" | Type], [Code | List]);

parse_encode_loop([#meta{path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = f64, explain = undefined, comment = Comment} | T], Depth, Fields, Type, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),
    PathGetName = [word:to_lower_hump(Prefix), word:to_hump(Suffix), ["." || Get =/= []], word:to_lower_hump(Get)],

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", PathHumpName, "Bytes = System.BitConverter.GetBytes((System.Double)", PathGetName, ");", "\n",
        Padding, "if (System.BitConverter.IsLittleEndian) System.Array.Reverse(", PathHumpName, "Bytes);", "\n",
        Padding, "writer.Write(", PathHumpName, "Bytes);"
    ],
    parse_encode_loop(T, Depth, [PathHumpName | Fields], ["System.Double" | Type], [Code | List]);

parse_encode_loop([#meta{path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = str, explain = undefined, comment = Comment} | T], Depth, Fields, Type, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),
    PathGetName = [word:to_lower_hump(Prefix), word:to_hump(Suffix), ["." || Get =/= []], word:to_lower_hump(Get)],

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", PathHumpName, "Bytes = encoding.GetBytes((System.String)", PathGetName, ");", "\n",
        Padding, "writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)", PathHumpName, "Bytes.Length));", "\n",
        Padding, "writer.Write(", PathHumpName, "Bytes);"
    ],
    parse_encode_loop(T, Depth, [PathHumpName | Fields], ["System.String" | Type], [Code | List]);

parse_encode_loop([#meta{path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = bst, explain = undefined, comment = Comment} | T], Depth, Fields, Type, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),
    PathGetName = [word:to_lower_hump(Prefix), word:to_hump(Suffix), ["." || Get =/= []], word:to_lower_hump(Get)],

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", PathHumpName, "Bytes = encoding.GetBytes((System.String)", PathGetName, ");", "\n",
        Padding, "writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)", PathHumpName, "Bytes.Length));", "\n",
        Padding, "writer.Write(", PathHumpName, "Bytes);"
    ],
    parse_encode_loop(T, Depth, [PathHumpName | Fields], ["System.String" | Type], [Code | List]);

parse_encode_loop([#meta{path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = ast, explain = undefined, comment = Comment} | T], Depth, Fields, Type, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),
    PathGetName = [word:to_lower_hump(Prefix), word:to_hump(Suffix), ["." || Get =/= []], word:to_lower_hump(Get)],

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", PathHumpName, "Bytes = encoding.GetBytes((System.String)", PathGetName, ");", "\n",
        Padding, "writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)", PathHumpName, "Bytes.Length));", "\n",
        Padding, "writer.Write(", PathHumpName, "Bytes);"
    ],
    parse_encode_loop(T, Depth, [PathHumpName | Fields], ["System.String" | Type], [Code | List]);

parse_encode_loop([#meta{from = From, path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = tuple, explain = Explain} | T], Depth, Names, Type, List) ->
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),
    PathGetName = [word:to_lower_hump(Prefix), word:to_hump(Suffix), ["." || Get =/= []], word:to_lower_hump(Get)],
    SubExplain = tuple_to_list(Explain),

    %% not root or list inner
    SubCode = [lists:concat([Padding, "var ", PathHumpName, " = ", PathGetName, ";"]) || From =/= root andalso From =/= list andalso From =/= ets],

    %% recursive
    {SubNames, SubTypes, SubCodes} = parse_encode_loop(SubExplain, Depth, Names, [], [SubCode | List]),

    ExplainEmpty = [#meta{name = "_"} || length(SubExplain) == 1],
    TypeEmpty = ["Empty" || length(SubTypes) == 1],

    TypeSet = [[TypeName, " ", word:to_lower_hump(FieldName)] || {#meta{name = FieldName}, TypeName} <- lists:zip(SubExplain ++ ExplainEmpty, SubTypes ++ TypeEmpty)],

    case TypeSet of
        [] ->
            TypeName = "Empty";
        _ ->
            TypeName = lists:concat(["(", string:join(TypeSet, ", "), ")"])
    end,

    %% flatten
    parse_encode_loop(T, Depth, lists:reverse(SubNames), [TypeName | Type], lists:reverse(SubCodes));

parse_encode_loop([#meta{from = From, path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = record, explain = Explain} | T], Depth, Names, Type, List) ->
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),
    PathGetName = [word:to_lower_hump(Prefix), word:to_hump(Suffix), ["." || Get =/= []], word:to_lower_hump(Get)],
    SubExplain = [Meta || Meta = #meta{} <- tuple_to_list(Explain)],

    %% not root or list inner
    SubCode = [lists:concat([Padding, "var ", PathHumpName, " = ", PathGetName, ";"]) || From =/= root andalso From =/= list andalso From =/= ets],

    %% recursive
    {SubNames, SubTypes, SubCodes} = parse_encode_loop(SubExplain, Depth, Names, [], [SubCode | List]),

    ExplainEmpty = [#meta{name = "_"} || length(SubExplain) == 1],
    TypeEmpty = ["Empty" || length(SubTypes) == 1],

    TypeSet = [[TypeName, " ", word:to_lower_hump(FieldName)] || {#meta{name = FieldName}, TypeName} <- lists:zip(SubExplain ++ ExplainEmpty, SubTypes ++ TypeEmpty)],

    case TypeSet of
        [] ->
            TypeName = "Empty";
        _ ->
            TypeName = lists:concat(["(", string:join(TypeSet, ", "), ")"])
    end,

    %% flatten
    parse_encode_loop(T, Depth, lists:reverse(SubNames), [TypeName | Type], lists:reverse(SubCodes));

parse_encode_loop([#meta{from = From, path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = maps, explain = Explain} | T], Depth, Names, Type, List) ->
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),
    PathGetName = [word:to_lower_hump(Prefix), word:to_hump(Suffix), ["." || Get =/= []], word:to_lower_hump(Get)],
    SubExplain = maps:values(Explain),

    %% not root or list inner
    SubCode = [lists:concat([Padding, "var ", PathHumpName, " = ", PathGetName, ";"]) || From =/= root andalso From =/= list andalso From =/= ets],

    %% recursive
    {SubNames, SubTypes, SubCodes} = parse_encode_loop(SubExplain, Depth, Names, [], [SubCode | List]),

    ExplainEmpty = [#meta{name = "_"} || length(SubExplain) == 1],
    TypeEmpty = ["Empty" || length(SubTypes) == 1],

    TypeSet = [[TypeName, " ", word:to_lower_hump(FieldName)] || {#meta{name = FieldName}, TypeName} <- lists:zip(SubExplain ++ ExplainEmpty, SubTypes ++ TypeEmpty)],

    case TypeSet of
        [] ->
            TypeName = "Empty";
        _ ->
            TypeName = lists:concat(["(", string:join(TypeSet, ", "), ")"])
    end,

    %% flatten
    parse_encode_loop(T, Depth, lists:reverse(SubNames), [TypeName | Type], lists:reverse(SubCodes));

parse_encode_loop([#meta{from = From, path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = list, explain = Explain, comment = Comment, key = undefined} | T], Depth, Fields, Type, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),
    PathGetName = [word:to_lower_hump(Prefix), word:to_hump(Suffix), ["." || Get =/= []], word:to_lower_hump(Get)],

    %% recursive
    {_, SubTypes, SubCodes} = parse_encode_loop(Explain, Depth + 1, [], [], []),

    %% not root or list inner
    SubCode = [lists:concat([Padding, "var ", PathHumpName, " = ", PathGetName, ";"]) || From =/= root andalso From =/= list andalso From =/= ets],

    TypeName = lists:concat(["System.Collections.Generic.List<", string:join(SubTypes, ""), ">"]),

    Code = [
        SubCode, "\n",
        Padding, "// ", Comment, "\n",
        Padding, "writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)", PathHumpName, ".Count));", "\n",
        Padding, "foreach(var ", PathHumpName, "Data in ", PathHumpName, ")", "\n",
        Padding, "{", "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "}"
    ],
    parse_encode_loop(T, Depth, [PathHumpName, lists:concat([PathHumpName, "Length"]) | Fields], [TypeName | Type], [Code | List]);

parse_encode_loop([#meta{from = From, path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = list, explain = Explain, comment = Comment, key = _} | T], Depth, Fields, Type, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),
    PathGetName = [word:to_lower_hump(Prefix), word:to_hump(Suffix), ["." || Get =/= []], word:to_lower_hump(Get)],

    %% recursive
    {_, SubTypes, SubCodes} = parse_encode_loop(Explain, Depth + 1, [], [], []),

    %% not root or list inner
    SubCode = [lists:concat([Padding, "var ", PathHumpName, " = ", PathGetName, ";"]) || From =/= root andalso From =/= list andalso From =/= ets],

    TypeName = lists:concat(["System.Collections.Generic.Dictionary<System.Object, ", string:join(SubTypes, ""), ">"]),

    Code = [
        SubCode, "\n",
        Padding, "// ", Comment, "\n",
        Padding, "writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)", PathHumpName, ".Count));", "\n",
        Padding, "foreach(var ", PathHumpName, "Data in ", PathHumpName, ".Values)", "\n",
        Padding, "{", "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "}"
    ],
    parse_encode_loop(T, Depth, [PathHumpName, lists:concat([PathHumpName, "Length"]) | Fields], [TypeName | Type], [Code | List]).

%%%===================================================================
%%% decode
%%%===================================================================
%% code
parse_decode(Protocol, Meta, #handler{}, _) ->
    %% start with 3 tabs(4 space) padding
    Padding = lists:duplicate(3, "    "),

    {Fields, _, Codes} = parse_decode_loop([Meta], 4, [], [], []),

    %% codes format
    CodesDefault = lists:concat([
        Padding, "    ", "return ", "(", "protocol: ", Protocol, ", " "data: ", string:join(Fields, ", "), ")", ";", "\n"
    ]),
    CodesBlock = lists:append(Codes, [CodesDefault]),

    %% format one protocol define
    Code = lists:concat([
        Padding, "case ", Protocol, ":", "\n",
        Padding, "{", "\n",
        string:join(CodesBlock, "\n"),
        Padding, "}"
    ]),

    #file{function = Code}.

parse_decode_loop([], _, Fields, Type, List) ->
    %% construct as a list
    {lists:reverse(Fields), lists:reverse(Type), lists:reverse(List)};

parse_decode_loop([#meta{name = _, type = zero} | T], Depth, Fields, Type, List) ->

    parse_decode_loop(T, Depth, Fields, Type, List);

parse_decode_loop([#meta{path = Path, type = binary, explain = Length, comment = Comment} | T], Depth, Fields, Type, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", PathHumpName, " = reader.ReadBytes(", integer_to_list(Length), ");"
    ],
    parse_decode_loop(T, Depth, [PathHumpName | Fields], ["System.Byte[]" | Type], [Code | List]);

parse_decode_loop([#meta{path = Path, type = bool, explain = undefined, comment = Comment} | T], Depth, Fields, Type, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", PathHumpName, " = reader.ReadByte() != 0;"
    ],
    parse_decode_loop(T, Depth, [PathHumpName | Fields], ["System.Boolean" | Type], [Code | List]);

parse_decode_loop([#meta{path = Path, type = u8, explain = undefined, comment = Comment} | T], Depth, Fields, Type, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", PathHumpName, " = reader.ReadByte();"
    ],
    parse_decode_loop(T, Depth, [PathHumpName | Fields], ["System.Byte" | Type], [Code | List]);

parse_decode_loop([#meta{path = Path, type = u16, explain = undefined, comment = Comment} | T], Depth, Fields, Type, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", PathHumpName, " = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());"
    ],
    parse_decode_loop(T, Depth, [PathHumpName | Fields], ["System.UInt16" | Type], [Code | List]);

parse_decode_loop([#meta{path = Path, type = u32, explain = undefined, comment = Comment} | T], Depth, Fields, Type, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", PathHumpName, " = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());"
    ],
    parse_decode_loop(T, Depth, [PathHumpName | Fields], ["System.UInt32" | Type], [Code | List]);

parse_decode_loop([#meta{path = Path, type = u64, explain = undefined, comment = Comment} | T], Depth, Fields, Type, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", PathHumpName, " = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());"
    ],
    parse_decode_loop(T, Depth, [PathHumpName | Fields], ["System.UInt64" | Type], [Code | List]);

parse_decode_loop([#meta{path = Path, type = i8, explain = undefined, comment = Comment} | T], Depth, Fields, Type, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", PathHumpName, " = reader.ReadSByte();"
    ],
    parse_decode_loop(T, Depth, [PathHumpName | Fields], ["System.SByte" | Type], [Code | List]);

parse_decode_loop([#meta{path = Path, type = i16, explain = undefined, comment = Comment} | T], Depth, Fields, Type, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", PathHumpName, " = System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());"
    ],
    parse_decode_loop(T, Depth, [PathHumpName | Fields], ["System.Int16" | Type], [Code | List]);

parse_decode_loop([#meta{path = Path, type = i32, explain = undefined, comment = Comment} | T], Depth, Fields, Type, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", PathHumpName, " = System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());"
    ],
    parse_decode_loop(T, Depth, [PathHumpName | Fields], ["System.Int32" | Type], [Code | List]);

parse_decode_loop([#meta{path = Path, type = i64, explain = undefined, comment = Comment} | T], Depth, Fields, Type, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", PathHumpName, " = System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());"
    ],
    parse_decode_loop(T, Depth, [PathHumpName | Fields], ["System.Int64" | Type], [Code | List]);

parse_decode_loop([#meta{path = Path, type = f32, explain = undefined, comment = Comment} | T], Depth, Fields, Type, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", PathHumpName, " = System.BitConverter.ToSingle(System.BitConverter.GetBytes(System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32())), 0);"
    ],
    parse_decode_loop(T, Depth, [PathHumpName | Fields], ["System.Single" | Type], [Code | List]);

parse_decode_loop([#meta{path = Path, type = f64, explain = undefined, comment = Comment} | T], Depth, Fields, Type, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", PathHumpName, " = System.BitConverter.ToDouble(System.BitConverter.GetBytes(System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64())), 0);"
    ],
    parse_decode_loop(T, Depth, [PathHumpName | Fields], ["System.Double" | Type], [Code | List]);

parse_decode_loop([#meta{path = Path, type = str, explain = undefined, comment = Comment} | T], Depth, Fields, Type, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", PathHumpName, "Length = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());", "\n",
        Padding, "var ", PathHumpName, " = encoding.GetString(reader.ReadBytes(", PathHumpName, "Length));"
    ],
    parse_decode_loop(T, Depth, [PathHumpName | Fields], ["System.String" | Type], [Code | List]);

parse_decode_loop([#meta{path = Path, type = bst, explain = undefined, comment = Comment} | T], Depth, Fields, Type, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", PathHumpName, "Length = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());", "\n",
        Padding, "var ", PathHumpName, " = encoding.GetString(reader.ReadBytes(", PathHumpName, "Length));"
    ],
    parse_decode_loop(T, Depth, [PathHumpName | Fields], ["System.String" | Type], [Code | List]);

parse_decode_loop([#meta{path = Path, type = ast, explain = undefined, comment = Comment} | T], Depth, Fields, Type, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", PathHumpName, "Length = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());", "\n",
        Padding, "var ", PathHumpName, " = encoding.GetString(reader.ReadBytes(", PathHumpName, "Length));"
    ],
    parse_decode_loop(T, Depth, [PathHumpName | Fields], ["System.String" | Type], [Code | List]);

parse_decode_loop([#meta{path = Path, type = tuple, explain = Explain, comment = Comment} | T], Depth, Fields, Type, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),

    SubExplain = [Meta || Meta = #meta{type = SubTypes} <- tuple_to_list(Explain), SubTypes =/= zero],

    %% recursive
    {SubFields, SubTypes, SubCodes} = parse_decode_loop(SubExplain, Depth, [], [], []),

    ExplainEmpty = [#meta{name = "_"} || length(SubExplain) == 1],
    TypeEmpty = ["Empty" || length(SubTypes) == 1],
    FieldEmpty = ["new Empty()" || length(SubTypes) == 1],

    TypeSet = [[TypeName, " ", word:to_lower_hump(FieldName)] || {#meta{name = FieldName}, TypeName} <- lists:zip(SubExplain ++ ExplainEmpty, SubTypes ++ TypeEmpty)],

    case TypeSet of
        [] ->
            TypeName = "Empty";
        _ ->
            TypeName = lists:concat(["(", string:join(TypeSet, ", "), ")"])
    end,

    ValueSet = [lists:concat([word:to_lower_hump(FieldName), ": ", PathName]) || {#meta{name = FieldName}, PathName} <- lists:zip(SubExplain ++ ExplainEmpty, SubFields ++ FieldEmpty)],

    case ValueSet of
        [] ->
            Result = "new Empty()";
        _ ->
            Result = lists:concat(["(" ,string:join(ValueSet, ", "), ")"])
    end,

    Code = [
        Padding, "// ", Comment, "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "// object", "\n",
        Padding, "var ", PathHumpName, " = ", Result, ";"
    ],

    %% flatten
    parse_decode_loop(T, Depth, [PathHumpName | Fields], [TypeName | Type], [Code | List]);

parse_decode_loop([#meta{path = Path, type = record, explain = Explain, comment = Comment} | T], Depth, Fields, Type, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),

    SubExplain = [Meta || Meta = #meta{} <- tuple_to_list(Explain)],

    %% recursive
    {SubFields, SubTypes, SubCodes} = parse_decode_loop(SubExplain, Depth, [], [], []),

    ExplainEmpty = [#meta{name = "_"} || length(SubExplain) == 1],
    TypeEmpty = ["Empty" || length(SubTypes) == 1],
    FieldEmpty = ["new Empty()" || length(SubTypes) == 1],

    TypeSet = [[TypeName, " ", word:to_lower_hump(FieldName)] || {#meta{name = FieldName}, TypeName} <- lists:zip(SubExplain ++ ExplainEmpty, SubTypes ++ TypeEmpty)],

    case TypeSet of
        [] ->
            TypeName = "Empty";
        _ ->
            TypeName = lists:concat(["(", string:join(TypeSet, ", "), ")"])
    end,

    ValueSet = [lists:concat([word:to_lower_hump(FieldName), ": ", PathName]) || {#meta{name = FieldName}, PathName} <- lists:zip(SubExplain ++ ExplainEmpty, SubFields ++ FieldEmpty)],

    case ValueSet of
        [] ->
            Result = "new Empty()";
        _ ->
            Result = lists:concat(["(" ,string:join(ValueSet, ", "), ")"])
    end,

    Code = [
        Padding, "// ", Comment, "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "// object", "\n",
        Padding, "var ", PathHumpName, " = ", Result, ";"
    ],

    %% flatten
    parse_decode_loop(T, Depth, [PathHumpName | Fields], [TypeName | Type], [Code | List]);

parse_decode_loop([#meta{path = Path, type = maps, explain = Explain, comment = Comment} | T], Depth, Fields, Type, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),

    SubExplain = maps:values(Explain),

    %% recursive
    {SubFields, SubTypes, SubCodes} = parse_decode_loop(SubExplain, Depth, [], [], []),

    ExplainEmpty = [#meta{name = "_"} || length(SubExplain) == 1],
    TypeEmpty = ["Empty" || length(SubTypes) == 1],
    FieldEmpty = ["new Empty()" || length(SubTypes) == 1],

    TypeSet = [[TypeName, " ", word:to_lower_hump(FieldName)] || {#meta{name = FieldName}, TypeName} <- lists:zip(SubExplain ++ ExplainEmpty, SubTypes ++ TypeEmpty)],

    case TypeSet of
        [] ->
            TypeName = "Empty";
        _ ->
            TypeName = lists:concat(["(", string:join(TypeSet, ", "), ")"])
    end,

    ValueSet = [lists:concat([word:to_lower_hump(FieldName), ": ", PathName]) || {#meta{name = FieldName}, PathName} <- lists:zip(SubExplain ++ ExplainEmpty, SubFields ++ FieldEmpty)],

    case ValueSet of
        [] ->
            Result = "new Empty()";
        _ ->
            Result = lists:concat(["(" ,string:join(ValueSet, ", "), ")"])
    end,

    Code = [
        Padding, "// ", Comment, "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "// object", "\n",
        Padding, "var ", PathHumpName, " = ", Result, ";"
    ],

    %% flatten
    parse_decode_loop(T, Depth, [PathHumpName | Fields], [TypeName | Type], [Code | List]);

parse_decode_loop([#meta{path = Path, type = list, explain = Explain, comment = Comment, key = undefined} | T], Depth, Fields, Type, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),

    %% recursive
    {SubFields, SubTypes, SubCodes} = parse_decode_loop(Explain, Depth + 1, [], [], []),

    TypeName = lists:concat(["System.Collections.Generic.List<", string:join(SubTypes, ""), ">"]),

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", PathHumpName, "Length = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());", "\n",
        Padding, "var ", PathHumpName, " = new ", TypeName, "(", PathHumpName, "Length);", "\n",
        Padding, "while (", PathHumpName, "Length-- > 0)", "\n",
        Padding, "{", "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "    // add", "\n",
        Padding, "    ", PathHumpName, ".Add(", string:join(SubFields, ", "), ");", "\n",
        Padding, "}"
    ],
    parse_decode_loop(T, Depth, [PathHumpName | Fields], [TypeName | Type], [Code | List]);

parse_decode_loop([#meta{path = Path, type = list, explain = Explain, comment = Comment, key = Key} | T], Depth, Fields, Type, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),

    %% recursive
    {SubFields, SubTypes, SubCodes} = parse_decode_loop(Explain, Depth + 1, [], [], []),

    TypeName = lists:concat(["System.Collections.Generic.Dictionary<System.Object, ", string:join(SubTypes, ""), ">"]),

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", PathHumpName, "Length = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());", "\n",
        Padding, "var ", PathHumpName, " = new ", TypeName, "(", PathHumpName, "Length);", "\n",
        Padding, "while (", PathHumpName, "Length-- > 0)", "\n",
        Padding, "{", "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "    // add", "\n",
        Padding, "    ", PathHumpName, "[", PathHumpName, "Data", word:to_hump(Key), "] = ", string:join(SubFields, ", "), ";", "\n",
        Padding, "}"
    ],
    parse_decode_loop(T, Depth, [PathHumpName | Fields], [TypeName | Type], [Code | List]);

parse_decode_loop([#meta{path = Path, type = ets, explain = Explain, comment = Comment, key = []} | T], Depth, Fields, Type, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),

    %% recursive
    {SubFields, SubTypes, SubCodes} = parse_decode_loop(Explain, Depth + 1, [], [], []),

    TypeName = lists:concat(["System.Collections.Generic.List<", string:join(SubTypes, ""), ">"]),

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", PathHumpName, "Length = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());", "\n",
        Padding, "var ", PathHumpName, " = new ", TypeName, "(", PathHumpName, "Length);", "\n",
        Padding, "while (", PathHumpName, "Length-- > 0)", "\n",
        Padding, "{", "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "    // add", "\n",
        Padding, "    ", PathHumpName, ".Add(", string:join(SubFields, ", "), ");", "\n",
        Padding, "}"
    ],
    parse_decode_loop(T, Depth, [PathHumpName | Fields], [TypeName | Type], [Code | List]);

parse_decode_loop([#meta{path = Path, type = ets, explain = Explain, comment = Comment, key = [Key]} | T], Depth, Fields, Type, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = word:to_lower_hump(Path),

    %% recursive
    {SubFields, SubTypes, SubCodes} = parse_decode_loop(Explain, Depth + 1, [], [], []),

    TypeName = lists:concat(["System.Collections.Generic.Dictionary<System.Object, ", string:join(SubTypes, ""), ">"]),

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", PathHumpName, "Length = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());", "\n",
        Padding, "var ", PathHumpName, " = new ", TypeName, "(", PathHumpName, "Length);", "\n",
        Padding, "while (", PathHumpName, "Length-- > 0)", "\n",
        Padding, "{", "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "    // add", "\n",
        Padding, "    ", PathHumpName, "[", PathHumpName, "Data", word:to_hump(Key), "] = ", string:join(SubFields, ", "), ";", "\n",
        Padding, "}"
    ],
    parse_decode_loop(T, Depth, [PathHumpName | Fields], Type, [Code | List]).