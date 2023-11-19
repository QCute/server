%%%-------------------------------------------------------------------
%%% @doc
%%% make protocol define to cs io/metadata code
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_maker_cs).
-export([format_code/3, format_meta/2]).
-export([parse_meta/2]).
-export([parse_encode/2, parse_decode/2]).
-include("../../../include/serialize.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
format_code(CsName, EncodeList, DecodeList) ->
    %% encode
    CsEncode = [Encode || #data{cs = #set{code = #file{function = Encode}}} <- EncodeList, Encode =/= []],
    CsEncodeDefault = lists:concat([
        "    ", "    ", "    ", "default:throw new System.ArgumentException(System.String.Format(\"unknown protocol define: {0}\", protocol));", "\n"
    ]),
    Encode = lists:append(CsEncode, [CsEncodeDefault]),

    %% decode
    CsDecode = [Decode || #data{cs = #set{code = #file{function = Decode}}} <- DecodeList, Decode =/= []],
    CsDecodeDefault = lists:concat([
        "    ", "    ", "    ", "default:throw new System.ArgumentException(System.String.Format(\"unknown protocol define: {0}\", protocol));", "\n"
    ]),
    Decode = lists:append(CsDecode, [CsDecodeDefault]),

    lists:concat([
        "public static class ", CsName, "\n"
        "{", "\n",
        "    ", "public static void Encode(System.Text.Encoding encoding, System.IO.BinaryWriter writer, System.UInt16 protocol, dynamic data) ", "\n",
        "    ", "{", "\n",
        "    ", "    ", "switch (protocol) ", "\n",
        "    ", "    ", "{", "\n",
        string:join(Encode, "\n"),
        "    ", "    ", "}", "\n",
        "    ", "}", "\n\n",
        "    ", "public static System.Object Decode(System.Text.Encoding encoding, System.IO.BinaryReader reader, System.UInt16 protocol) ", "\n",
        "    ", "{", "\n",
        "    ", "    ", "switch (protocol) ", "\n",
        "    ", "    ", "{", "\n",
        string:join(Decode, "\n"),
        "    ", "    ", "}", "\n",
        "    ", "}", "\n",
        "}"
    ]).

format_meta(CsName, List) ->
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
        "public static class ", CsName, "\n", "{", "\n",
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

    SubExplain = [Meta || Meta = #meta{} <- tuple_to_list(Explain)],

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
%%% encode
%%%===================================================================
%% code
parse_encode(Protocol, Meta = #meta{}) ->
    %% start with 3 tabs(4 space) padding
    Padding = lists:duplicate(3, "    "),

    {_, Codes} = parse_encode_loop([Meta], 4, [], []),

    %% codes format
    CodesDefault = lists:concat([
        Padding, "    ", "return;", "\n"
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

parse_encode_loop([], _, Fields, List) ->
    %% construct as a list
    {lists:reverse(Fields), lists:reverse(List)};

parse_encode_loop([#meta{path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = binary, explain = _, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = [word:to_lower_hump(hd(Path)), [word:to_hump(P) || P <- tl(Path)]],
    PathGetName = [word:to_lower_hump(hd(Prefix)), [word:to_hump(H) || H <- tl(Prefix)], [word:to_hump(H) || H <- Suffix], [["[", "\"", word:to_lower_hump(G), "\"", "]"] || G <- Get]],

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "writer.Write((System.Byte[])", PathGetName, ");"
    ],
    parse_encode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_encode_loop([#meta{path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = bool, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = [word:to_lower_hump(hd(Path)), [word:to_hump(P) || P <- tl(Path)]],
    PathGetName = [word:to_lower_hump(hd(Prefix)), [word:to_hump(H) || H <- tl(Prefix)], [word:to_hump(H) || H <- Suffix], [["[", "\"", word:to_lower_hump(G), "\"", "]"] || G <- Get]],

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "writer.Write((System.Byte)((System.Boolean)", PathGetName, " ? 1 : 0));"
    ],
    parse_encode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_encode_loop([#meta{path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = u8, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = [word:to_lower_hump(hd(Path)), [word:to_hump(P) || P <- tl(Path)]],
    PathGetName = [word:to_lower_hump(hd(Prefix)), [word:to_hump(H) || H <- tl(Prefix)], [word:to_hump(H) || H <- Suffix], [["[", "\"", word:to_lower_hump(G), "\"", "]"] || G <- Get]],

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "writer.Write((System.Byte)", PathGetName, ");"
    ],
    parse_encode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_encode_loop([#meta{path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = u16, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = [word:to_lower_hump(hd(Path)), [word:to_hump(P) || P <- tl(Path)]],
    PathGetName = [word:to_lower_hump(hd(Prefix)), [word:to_hump(H) || H <- tl(Prefix)], [word:to_hump(H) || H <- Suffix], [["[", "\"", word:to_lower_hump(G), "\"", "]"] || G <- Get]],

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)(System.UInt16)", PathGetName, "));"
    ],
    parse_encode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_encode_loop([#meta{path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = u32, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = [word:to_lower_hump(hd(Path)), [word:to_hump(P) || P <- tl(Path)]],
    PathGetName = [word:to_lower_hump(hd(Prefix)), [word:to_hump(H) || H <- tl(Prefix)], [word:to_hump(H) || H <- Suffix], [["[", "\"", word:to_lower_hump(G), "\"", "]"] || G <- Get]],

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int32)(System.UInt32)", PathGetName, "));"
    ],
    parse_encode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_encode_loop([#meta{path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = u64, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = [word:to_lower_hump(hd(Path)), [word:to_hump(P) || P <- tl(Path)]],
    PathGetName = [word:to_lower_hump(hd(Prefix)), [word:to_hump(H) || H <- tl(Prefix)], [word:to_hump(H) || H <- Suffix], [["[", "\"", word:to_lower_hump(G), "\"", "]"] || G <- Get]],

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)(System.UInt64)", PathGetName, "));"
    ],
    parse_encode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_encode_loop([#meta{path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = i8, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = [word:to_lower_hump(hd(Path)), [word:to_hump(P) || P <- tl(Path)]],
    PathGetName = [word:to_lower_hump(hd(Prefix)), [word:to_hump(H) || H <- tl(Prefix)], [word:to_hump(H) || H <- Suffix], [["[", "\"", word:to_lower_hump(G), "\"", "]"] || G <- Get]],

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "writer.Write((System.SByte)", PathGetName, ");"
    ],
    parse_encode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_encode_loop([#meta{path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = i16, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = [word:to_lower_hump(hd(Path)), [word:to_hump(P) || P <- tl(Path)]],
    PathGetName = [word:to_lower_hump(hd(Prefix)), [word:to_hump(H) || H <- tl(Prefix)], [word:to_hump(H) || H <- Suffix], [["[", "\"", word:to_lower_hump(G), "\"", "]"] || G <- Get]],

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)", PathGetName, "));"
    ],
    parse_encode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_encode_loop([#meta{path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = i32, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = [word:to_lower_hump(hd(Path)), [word:to_hump(P) || P <- tl(Path)]],
    PathGetName = [word:to_lower_hump(hd(Prefix)), [word:to_hump(H) || H <- tl(Prefix)], [word:to_hump(H) || H <- Suffix], [["[", "\"", word:to_lower_hump(G), "\"", "]"] || G <- Get]],

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int32)", PathGetName, "));"
    ],
    parse_encode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_encode_loop([#meta{path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = i64, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = [word:to_lower_hump(hd(Path)), [word:to_hump(P) || P <- tl(Path)]],
    PathGetName = [word:to_lower_hump(hd(Prefix)), [word:to_hump(H) || H <- tl(Prefix)], [word:to_hump(H) || H <- Suffix], [["[", "\"", word:to_lower_hump(G), "\"", "]"] || G <- Get]],

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)", PathGetName, "));"
    ],
    parse_encode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_encode_loop([#meta{path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = f32, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = [word:to_lower_hump(hd(Path)), [word:to_hump(P) || P <- tl(Path)]],
    PathGetName = [word:to_lower_hump(hd(Prefix)), [word:to_hump(H) || H <- tl(Prefix)], [word:to_hump(H) || H <- Suffix], [["[", "\"", word:to_lower_hump(G), "\"", "]"] || G <- Get]],

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", PathHumpName, "Bytes = System.BitConverter.GetBytes((System.Single)", PathGetName, ");", "\n",
        Padding, "if (System.BitConverter.IsLittleEndian) System.Array.Reverse(", PathHumpName, "Bytes);", "\n",
        Padding, "writer.Write(", PathHumpName, "Bytes);"
    ],
    parse_encode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_encode_loop([#meta{path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = f64, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = [word:to_lower_hump(hd(Path)), [word:to_hump(P) || P <- tl(Path)]],
    PathGetName = [word:to_lower_hump(hd(Prefix)), [word:to_hump(H) || H <- tl(Prefix)], [word:to_hump(H) || H <- Suffix], [["[", "\"", word:to_lower_hump(G), "\"", "]"] || G <- Get]],

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", PathHumpName, "Bytes = System.BitConverter.GetBytes((System.Double)", PathGetName, ");", "\n",
        Padding, "if (System.BitConverter.IsLittleEndian) System.Array.Reverse(", PathHumpName, "Bytes);", "\n",
        Padding, "writer.Write(", PathHumpName, "Bytes);"
    ],
    parse_encode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_encode_loop([#meta{path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = str, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = [word:to_lower_hump(hd(Path)), [word:to_hump(P) || P <- tl(Path)]],
    PathGetName = [word:to_lower_hump(hd(Prefix)), [word:to_hump(H) || H <- tl(Prefix)], [word:to_hump(H) || H <- Suffix], [["[", "\"", word:to_lower_hump(G), "\"", "]"] || G <- Get]],

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", PathHumpName, "Bytes = encoding.GetBytes((System.String)", PathGetName, ");", "\n",
        Padding, "writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)", PathHumpName, "Bytes.Length));", "\n",
        Padding, "writer.Write(", PathHumpName, "Bytes);"
    ],
    parse_encode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_encode_loop([#meta{path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = bst, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = [word:to_lower_hump(hd(Path)), [word:to_hump(P) || P <- tl(Path)]],
    PathGetName = [word:to_lower_hump(hd(Prefix)), [word:to_hump(H) || H <- tl(Prefix)], [word:to_hump(H) || H <- Suffix], [["[", "\"", word:to_lower_hump(G), "\"", "]"] || G <- Get]],

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", PathHumpName, "Bytes = encoding.GetBytes((System.String)", PathGetName, ");", "\n",
        Padding, "writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)", PathHumpName, "Bytes.Length));", "\n",
        Padding, "writer.Write(", PathHumpName, "Bytes);"
    ],
    parse_encode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_encode_loop([#meta{path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = ast, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = [word:to_lower_hump(hd(Path)), [word:to_hump(P) || P <- tl(Path)]],
    PathGetName = [word:to_lower_hump(hd(Prefix)), [word:to_hump(H) || H <- tl(Prefix)], [word:to_hump(H) || H <- Suffix], [["[", "\"", word:to_lower_hump(G), "\"", "]"] || G <- Get]],

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", PathHumpName, "Bytes = encoding.GetBytes((System.String)", "(", "(System.Collections.Generic.Dictionary<System.String, System.Object>)", PathGetName, ")", ");", "\n",
        Padding, "writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)", PathHumpName, "Bytes.Length));", "\n",
        Padding, "writer.Write(", PathHumpName, "Bytes);"
    ],
    parse_encode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_encode_loop([#meta{alias = Alias, path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = tuple, explain = Explain} | T], Depth, Names, List) ->
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = [word:to_lower_hump(hd(Path)), [word:to_hump(P) || P <- tl(Path)]],
    PathGetName = [word:to_lower_hump(hd(Prefix)), [word:to_hump(H) || H <- tl(Prefix)], [word:to_hump(H) || H <- Suffix], [["[", "\"", word:to_lower_hump(G), "\"", "]"] || G <- Get]],
    SubExplain = tuple_to_list(Explain),

    %% not root or list inner
    SubCode = [lists:concat([Padding, "var ", PathHumpName, " = ", PathGetName, ";"]) || Alias =/= '$$'],

    %% recursive
    {SubNames, SubCodes} = parse_encode_loop(SubExplain, Depth, Names, [SubCode | List]),

    %% flatten
    parse_encode_loop(T, Depth, lists:reverse(SubNames), lists:reverse(SubCodes));

parse_encode_loop([#meta{alias = Alias, path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = record, explain = Explain} | T], Depth, Names, List) ->
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = [word:to_lower_hump(hd(Path)), [word:to_hump(P) || P <- tl(Path)]],
    PathGetName = [word:to_lower_hump(hd(Prefix)), [word:to_hump(H) || H <- tl(Prefix)], [word:to_hump(H) || H <- Suffix], [["[", "\"", word:to_lower_hump(G), "\"", "]"] || G <- Get]],
    SubExplain = [Meta || Meta = #meta{} <- tuple_to_list(Explain)],

    %% not root or list inner
    SubCode = [lists:concat([Padding, "var ", PathHumpName, " = ", PathGetName, ";"]) || Alias =/= '$$'],

    %% recursive
    {SubNames, SubCodes} = parse_encode_loop(SubExplain, Depth, Names, [SubCode | List]),

    %% flatten
    parse_encode_loop(T, Depth, lists:reverse(SubNames), lists:reverse(SubCodes));

parse_encode_loop([#meta{alias = Alias, path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = maps, explain = Explain} | T], Depth, Names, List) ->
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = [word:to_lower_hump(hd(Path)), [word:to_hump(P) || P <- tl(Path)]],
    PathGetName = [word:to_lower_hump(hd(Prefix)), [word:to_hump(H) || H <- tl(Prefix)], [word:to_hump(H) || H <- Suffix], [["[", "\"", word:to_lower_hump(G), "\"", "]"] || G <- Get]],
    SubExplain = maps:values(Explain),

    %% not root or list inner
    SubCode = [lists:concat([Padding, "var ", PathHumpName, " = ", PathGetName, ";"]) || Alias =/= '$$'],

    %% recursive
    {SubNames, SubCodes} = parse_encode_loop(SubExplain, Depth, Names, [SubCode | List]),

    %% flatten
    parse_encode_loop(T, Depth, lists:reverse(SubNames), lists:reverse(SubCodes));

parse_encode_loop([#meta{alias = Alias, path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = list, explain = Explain, comment = Comment, key = undefined} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = [word:to_lower_hump(hd(Path)), [word:to_hump(P) || P <- tl(Path)]],
    PathGetName = [word:to_lower_hump(hd(Prefix)), [word:to_hump(H) || H <- tl(Prefix)], [word:to_hump(H) || H <- Suffix], [["[", "\"", word:to_lower_hump(G), "\"", "]"] || G <- Get]],

    %% recursive
    {_, SubCodes} = parse_encode_loop(Explain, Depth + 1, [], []),

    %% not root or list inner
    SubCode = [lists:concat([Padding, "var ", PathHumpName, " = ", PathGetName, ";"]) || Alias =/= '$$'],

    Code = [
        SubCode, "\n",
        Padding, "// ", Comment, "\n",
        Padding, "writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)", PathHumpName, ".Count));", "\n",
        Padding, "foreach(var ", PathHumpName, "Data in ", PathHumpName, ")", "\n",
        Padding, "{", "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "}"
    ],
    parse_encode_loop(T, Depth, [PathHumpName, lists:concat([PathHumpName, "Length"]) | Fields], [Code | List]);

parse_encode_loop([#meta{alias = Alias, path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = list, explain = Explain, comment = Comment, key = _} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = [word:to_lower_hump(hd(Path)), [word:to_hump(P) || P <- tl(Path)]],
    PathGetName = [word:to_lower_hump(hd(Prefix)), [word:to_hump(H) || H <- tl(Prefix)], [word:to_hump(H) || H <- Suffix], [["[", "\"", word:to_lower_hump(G), "\"", "]"] || G <- Get]],

    %% recursive
    {_, SubCodes} = parse_encode_loop(Explain, Depth + 1, [], []),

    %% not root or list inner
    SubCode = [lists:concat([Padding, "var ", PathHumpName, " = ", PathGetName, ";"]) || Alias =/= '$$'],

    Code = [
        SubCode, "\n",
        Padding, "// ", Comment, "\n",
        Padding, "writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)", PathHumpName, ".Count));", "\n",
        Padding, "foreach(var ", PathHumpName, "Data in ", PathHumpName, ")", "\n",
        Padding, "{", "\n",
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
        Padding, "    ", "return ", string:join(Fields, ", "), ";", "\n"
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

parse_decode_loop([], _, Fields, List) ->
    %% construct as a list
    {lists:reverse(Fields), lists:reverse(List)};

parse_decode_loop([#meta{name = _, type = zero} | T], Depth, Fields, List) ->

    parse_decode_loop(T, Depth, Fields, List);

parse_decode_loop([#meta{path = Path, type = binary, explain = Length, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = [word:to_lower_hump(hd(Path)), [word:to_hump(P) || P <- tl(Path)]],

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", PathHumpName, " = reader.ReadBytes(", integer_to_list(Length), ");"
    ],
    parse_decode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_decode_loop([#meta{path = Path, type = bool, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = [word:to_lower_hump(hd(Path)), [word:to_hump(P) || P <- tl(Path)]],

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", PathHumpName, " = reader.ReadByte() != 0;"
    ],
    parse_decode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_decode_loop([#meta{path = Path, type = u8, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = [word:to_lower_hump(hd(Path)), [word:to_hump(P) || P <- tl(Path)]],

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", PathHumpName, " = reader.ReadByte();"
    ],
    parse_decode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_decode_loop([#meta{path = Path, type = u16, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = [word:to_lower_hump(hd(Path)), [word:to_hump(P) || P <- tl(Path)]],

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", PathHumpName, " = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());"
    ],
    parse_decode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_decode_loop([#meta{path = Path, type = u32, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = [word:to_lower_hump(hd(Path)), [word:to_hump(P) || P <- tl(Path)]],

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", PathHumpName, " = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());"
    ],
    parse_decode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_decode_loop([#meta{path = Path, type = u64, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = [word:to_lower_hump(hd(Path)), [word:to_hump(P) || P <- tl(Path)]],

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", PathHumpName, " = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());"
    ],
    parse_decode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_decode_loop([#meta{path = Path, type = i8, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = [word:to_lower_hump(hd(Path)), [word:to_hump(P) || P <- tl(Path)]],

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", PathHumpName, " = reader.ReadSByte();"
    ],
    parse_decode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_decode_loop([#meta{path = Path, type = i16, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = [word:to_lower_hump(hd(Path)), [word:to_hump(P) || P <- tl(Path)]],

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", PathHumpName, " = System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());"
    ],
    parse_decode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_decode_loop([#meta{path = Path, type = i32, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = [word:to_lower_hump(hd(Path)), [word:to_hump(P) || P <- tl(Path)]],

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", PathHumpName, " = System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());"
    ],
    parse_decode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_decode_loop([#meta{path = Path, type = i64, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = [word:to_lower_hump(hd(Path)), [word:to_hump(P) || P <- tl(Path)]],

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", PathHumpName, " = System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());"
    ],
    parse_decode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_decode_loop([#meta{path = Path, type = f32, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = [word:to_lower_hump(hd(Path)), [word:to_hump(P) || P <- tl(Path)]],

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", PathHumpName, " = System.BitConverter.ToSingle(System.BitConverter.GetBytes(System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32())), 0);"
    ],
    parse_decode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_decode_loop([#meta{path = Path, type = f64, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = [word:to_lower_hump(hd(Path)), [word:to_hump(P) || P <- tl(Path)]],

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", PathHumpName, " = System.BitConverter.ToDouble(System.BitConverter.GetBytes(System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64())), 0);"
    ],
    parse_decode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_decode_loop([#meta{path = Path, type = str, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = [word:to_lower_hump(hd(Path)), [word:to_hump(P) || P <- tl(Path)]],

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", PathHumpName, "Length = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());", "\n",
        Padding, "var ", PathHumpName, " = encoding.GetString(reader.ReadBytes(", PathHumpName, "Length));"
    ],
    parse_decode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_decode_loop([#meta{path = Path, type = bst, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = [word:to_lower_hump(hd(Path)), [word:to_hump(P) || P <- tl(Path)]],

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", PathHumpName, "Length = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());", "\n",
        Padding, "var ", PathHumpName, " = encoding.GetString(reader.ReadBytes(", PathHumpName, "Length));"
    ],
    parse_decode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_decode_loop([#meta{path = Path, type = ast, explain = undefined, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = [word:to_lower_hump(hd(Path)), [word:to_hump(P) || P <- tl(Path)]],

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", PathHumpName, "Length = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());", "\n",
        Padding, "var ", PathHumpName, " = encoding.GetString(reader.ReadBytes(", PathHumpName, "Length));"
    ],
    parse_decode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_decode_loop([#meta{path = Path, type = tuple, explain = Explain, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = [word:to_lower_hump(hd(Path)), [word:to_hump(P) || P <- tl(Path)]],

    SubExplain = [Meta || Meta = #meta{type = SubType} <- tuple_to_list(Explain), SubType =/= zero],

    %% recursive
    {SubFields, SubCodes} = parse_decode_loop(SubExplain, Depth, [], []),

    Code = [
        Padding, "// ", Comment, "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "// object", "\n",
        Padding, "var ", PathHumpName, " = new System.Collections.Generic.Dictionary<System.String, System.Object>() {", string:join([lists:concat(["{\"", word:to_lower_hump(FieldName), "\", ", PathName, "}"]) || {#meta{name = FieldName}, PathName} <- lists:zip(SubExplain, SubFields)], ", "), "};"
    ],

    %% flatten
    parse_decode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_decode_loop([#meta{path = Path, type = record, explain = Explain, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = [word:to_lower_hump(hd(Path)), [word:to_hump(P) || P <- tl(Path)]],

    SubExplain = [Meta || Meta = #meta{} <- tuple_to_list(Explain)],

    %% recursive
    {SubFields, SubCodes} = parse_decode_loop(SubExplain, Depth, [], []),

    Code = [
        Padding, "// ", Comment, "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "// object", "\n",
        Padding, "var ", PathHumpName, " = new System.Collections.Generic.Dictionary<System.String, System.Object>() {", string:join([lists:concat(["{\"", word:to_lower_hump(FieldName), "\", ", PathName, "}"]) || {#meta{name = FieldName}, PathName} <- lists:zip(SubExplain, SubFields)], ", "), "};"
    ],

    %% flatten
    parse_decode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_decode_loop([#meta{path = Path, type = maps, explain = Explain, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = [word:to_lower_hump(hd(Path)), [word:to_hump(P) || P <- tl(Path)]],

    SubExplain = maps:values(Explain),

    %% recursive
    {SubFields, SubCodes} = parse_decode_loop(SubExplain, Depth, [], []),

    Code = [
        Padding, "// ", Comment, "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "// object", "\n",
        Padding, "var ", PathHumpName, " = new System.Collections.Generic.Dictionary<System.String, System.Object>() {", string:join([lists:concat(["{\"", word:to_lower_hump(FieldName), "\", ", PathName, "}"]) || {#meta{name = FieldName}, PathName} <- lists:zip(SubExplain, SubFields)], ", "), "};"
    ],

    %% flatten
    parse_decode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_decode_loop([#meta{path = Path, type = list, explain = Explain, comment = Comment, key = undefined} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = [word:to_lower_hump(hd(Path)), [word:to_hump(P) || P <- tl(Path)]],

    %% recursive
    {SubFields, SubCodes} = parse_decode_loop(Explain, Depth + 1, [], []),
    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", PathHumpName, "Length = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());", "\n",
        Padding, "var ", PathHumpName, " = new System.Collections.Generic.List<System.Object>(", PathHumpName, "Length);", "\n",
        Padding, "while (", PathHumpName, "Length-- > 0)", "\n",
        Padding, "{", "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "    // add", "\n",
        Padding, "    ", PathHumpName, ".Add(", string:join(SubFields, ", "), ");", "\n",
        Padding, "}"
    ],
    parse_decode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_decode_loop([#meta{path = Path, type = list, explain = Explain, comment = Comment, key = Key} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = [word:to_lower_hump(hd(Path)), [word:to_hump(P) || P <- tl(Path)]],

    %% recursive
    {SubFields, SubCodes} = parse_decode_loop(Explain, Depth + 1, [], []),
    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", PathHumpName, "Length = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());", "\n",
        Padding, "var ", PathHumpName, " = new System.Collections.Generic.Dictionary<System.Object, System.Collections.Generic.Dictionary<System.String, System.Object>>(", PathHumpName, "Length);", "\n",
        Padding, "while (", PathHumpName, "Length-- > 0)", "\n",
        Padding, "{", "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "    // add", "\n",
        Padding, "    ", PathHumpName, "[", PathHumpName, "Data", word:to_hump(Key), "] = ", string:join(SubFields, ", "), ";", "\n",
        Padding, "}"
    ],
    parse_decode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_decode_loop([#meta{path = Path, type = ets, explain = Explain, comment = Comment, key = []} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = [word:to_lower_hump(hd(Path)), [word:to_hump(P) || P <- tl(Path)]],

    %% recursive
    {SubFields, SubCodes} = parse_decode_loop(Explain, Depth + 1, [], []),
    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", PathHumpName, "Length = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());", "\n",
        Padding, "var ", PathHumpName, " = new System.Collections.Generic.List<System.Object>(", PathHumpName, "Length);", "\n",
        Padding, "while (", PathHumpName, "Length-- > 0)", "\n",
        Padding, "{", "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "    // add", "\n",
        Padding, "    ", PathHumpName, ".Add(", string:join(SubFields, ", "), ");", "\n",
        Padding, "}"
    ],
    parse_decode_loop(T, Depth, [PathHumpName | Fields], [Code | List]);

parse_decode_loop([#meta{path = Path, type = ets, explain = Explain, comment = Comment, key = [Key]} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    PathHumpName = [word:to_lower_hump(hd(Path)), [word:to_hump(P) || P <- tl(Path)]],

    %% recursive
    {SubFields, SubCodes} = parse_decode_loop(Explain, Depth + 1, [], []),
    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", PathHumpName, "Length = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());", "\n",
        Padding, "var ", PathHumpName, " = new System.Collections.Generic.Dictionary<System.Object, System.Collections.Generic.Dictionary<System.String, System.Object>>(", PathHumpName, "Length);", "\n",
        Padding, "while (", PathHumpName, "Length-- > 0)", "\n",
        Padding, "{", "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "    // add", "\n",
        Padding, "    ", PathHumpName, "[", PathHumpName, "Data", word:to_hump(Key), "] = ", string:join(SubFields, ", "), ";", "\n",
        Padding, "}"
    ],
    parse_decode_loop(T, Depth, [PathHumpName | Fields], [Code | List]).
