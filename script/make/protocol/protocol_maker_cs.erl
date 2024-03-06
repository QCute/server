%%%-------------------------------------------------------------------
%%% @doc
%%% make protocol define to cs io/metadata code
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_maker_cs).
-export([format_code/3, format_meta/2]).
-export([parse_meta_cs/2]).
-export([parse_encode_cs/2, parse_decode_cs/2]).
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
        "    ", "public static void Encode(System.Text.Encoding encoding, System.IO.BinaryWriter writer, System.UInt16 protocol, System.Object data) ", "\n",
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
%% cs meta
parse_meta_cs(_, Meta) ->
    %% start with 1 tabs(4 space) padding
    %% Padding = lists:duplicate(3, "    "),
    Code = parse_meta_cs_loop([Meta], 4, []),
    %% format one protocol define
    %% lists:concat(["        \"", Protocol, "\" : [\n", MetaData, "\n        ]"]).

    #file{extra = string:trim(Code)}.

parse_meta_cs_loop([], _, List) ->
    %% construct as a list
    string:join(lists:reverse(List), ",\n");

parse_meta_cs_loop([#meta{name = _, type = zero} | T], Depth, List) ->
    parse_meta_cs_loop(T, Depth, List);

parse_meta_cs_loop([#meta{name = Name, type = binary, explain = Length, comment = Comment} | T], Depth, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    Code = lists:flatten(io_lib:format("~snew Map() { {\"name\", \"~s\"}, {\"type\", \"~s\"}, {\"comment\", \"~ts\"}, {\"explain\", ~w} }", [Padding, word:to_lower_hump(Name), binary, Comment, Length])),
    parse_meta_cs_loop(T, Depth, [Code | List]);

parse_meta_cs_loop([#meta{name = Name, type = Type, explain = undefined, comment = Comment} | T], Depth, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    Code = lists:flatten(io_lib:format("~snew Map() { {\"name\", \"~s\"}, {\"type\", \"~s\"}, {\"comment\", \"~ts\"}, {\"explain\", new List()} }", [Padding, word:to_lower_hump(Name), Type, Comment])),
    parse_meta_cs_loop(T, Depth, [Code | List]);

parse_meta_cs_loop([#meta{name = Name, type = tuple, explain = Explain, comment = Comment} | T], Depth, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),

    %% recursive
    SubCodes = parse_meta_cs_loop(tuple_to_list(Explain), Depth + 1, []),

    %% format one field
    Code = lists:flatten(io_lib:format("~snew Map() { {\"name\", \"~s\"}, {\"type\", \"~s\"}, {\"comment\", \"~ts\"}, {\"explain\", new List() {\n~ts\n~s}}}", [Padding, word:to_lower_hump(Name), map, Comment, SubCodes, Padding])),

    parse_meta_cs_loop(T, Depth, [Code | List]);

parse_meta_cs_loop([#meta{name = Name, type = record, explain = Explain, comment = Comment} | T], Depth, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),

    SubExplain = [Meta || Meta = #meta{} <- tuple_to_list(Explain)],

    %% recursive
    SubCodes = parse_meta_cs_loop(SubExplain, Depth + 1, []),

    %% format one field
    Code = lists:flatten(io_lib:format("~snew Map() { {\"name\", \"~s\"}, {\"type\", \"~s\"}, {\"comment\", \"~ts\"}, {\"explain\", new List() {\n~ts\n~s}}}", [Padding, word:to_lower_hump(Name), map, Comment, SubCodes, Padding])),

    parse_meta_cs_loop(T, Depth, [Code | List]);

parse_meta_cs_loop([#meta{name = Name, type = maps, explain = Explain, comment = Comment} | T], Depth, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),

    %% recursive
    SubCodes = parse_meta_cs_loop(maps:values(Explain), Depth + 1, []),

    %% format one field
    Code = lists:flatten(io_lib:format("~snew Map() { {\"name\", \"~s\"}, {\"type\", \"~s\"}, {\"comment\", \"~ts\"}, {\"explain\", new List() {\n~ts\n~s}}}", [Padding, word:to_lower_hump(Name), map, Comment, SubCodes, Padding])),

    parse_meta_cs_loop(T, Depth, [Code | List]);

parse_meta_cs_loop([#meta{name = Name, type = list, explain = Explain, comment = Comment, key = undefined} | T], Depth, List) ->
    %% recursive
    SubCodes = parse_meta_cs_loop(Explain, Depth + 1, []),
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    Code = lists:flatten(io_lib:format("~snew Map() { {\"name\", \"~s\"}, {\"type\", \"~s\"}, {\"comment\", \"~ts\"}, {\"explain\", new List() {\n~ts\n~s}}}", [Padding, word:to_lower_hump(Name), list, Comment, SubCodes, Padding])),
    parse_meta_cs_loop(T, Depth, [Code | List]);

parse_meta_cs_loop([#meta{name = Name, type = list, explain = Explain, comment = Comment, key = Key} | T], Depth, List) ->
    %% recursive
    SubCodes = parse_meta_cs_loop(Explain, Depth + 1, []),
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    Code = lists:flatten(io_lib:format("~snew Map() { {\"name\", \"~s\"}, {\"type\", \"~s\"}, {\"comment\", \"~ts\"}, {\"key\", \"~ts\"}, {\"explain\", new List() {\n~ts\n~s}}}", [Padding, word:to_lower_hump(Name), list, Comment, word:to_lower_hump(Key), SubCodes, Padding])),
    parse_meta_cs_loop(T, Depth, [Code | List]);

parse_meta_cs_loop([#meta{name = Name, type = ets, explain = Explain, comment = Comment, key = undefined} | T], Depth, List) ->
    %% recursive
    SubCodes = parse_meta_cs_loop(Explain, Depth + 1, []),
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    Code = lists:flatten(io_lib:format("~snew Map() { {\"name\", \"~s\"}, {\"type\", \"~s\"}, {\"comment\", \"~ts\"}, {\"explain\", new List() {\n~ts\n~s}}}", [Padding, word:to_lower_hump(Name), list, Comment, SubCodes, Padding])),
    parse_meta_cs_loop(T, Depth, [Code | List]);

parse_meta_cs_loop([#meta{name = Name, type = ets, explain = Explain, comment = Comment, key = Key} | T], Depth, List) ->
    %% recursive
    SubCodes = parse_meta_cs_loop(Explain, Depth + 1, []),
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    Code = lists:flatten(io_lib:format("~snew Map() { {\"name\", \"~s\"}, {\"type\", \"~s\"}, {\"comment\", \"~ts\"}, {\"key\", \"~ts\"}, {\"explain\", new List() {\n~ts\n~s}}}", [Padding, word:to_lower_hump(Name), list, Comment, word:to_lower_hump(Key), SubCodes, Padding])),
    parse_meta_cs_loop(T, Depth, [Code | List]).

%%%===================================================================
%%% encode
%%%===================================================================
%% cs code
parse_encode_cs(Protocol, Meta = #meta{name = Name, type = Type}) ->
    %% start with 3 tabs(4 space) padding
    Padding = lists:duplicate(3, "    "),

    {_, Codes} = parse_encode_cs_loop([Meta], 4, Type, [], atom_to_list(Name), [], []),

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

parse_encode_cs_loop([], _, _, _, _, Fields, List) ->
    %% construct as a list
    {lists:reverse(Fields), lists:reverse(List)};

parse_encode_cs_loop([#meta{name = Name, type = binary, explain = _, comment = Comment} | T], Depth, Parent, Ancestor, Scope, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),
    LastGetNames = lists:sublist(NewAncestor, 1),
    PathGetName = [lists:concat(["[\"", word:to_lower_hump(PathName), "\"]"]) || PathName <- LastGetNames, Ancestor =/= [] andalso Parent =/= list andalso Parent =/= ets],

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "writer.Write((System.Byte[])", Scope, PathGetName, ");"
    ],
    parse_encode_cs_loop(T, Depth, Parent, Ancestor, Scope, [PathHumpName | Fields], [Code | List]);

parse_encode_cs_loop([#meta{name = Name, type = bool, explain = undefined, comment = Comment} | T], Depth, Parent, Ancestor, Scope, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),
    LastGetNames = lists:sublist(NewAncestor, 1),
    PathGetName = [lists:concat(["[\"", word:to_lower_hump(PathName), "\"]"]) || PathName <- LastGetNames, Ancestor =/= [] andalso Parent =/= list andalso Parent =/= ets],

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "writer.Write((System.Byte)((System.Boolean)", Scope, PathGetName, " ? 1 : 0));"
    ],
    parse_encode_cs_loop(T, Depth, Parent, Ancestor, Scope, [PathHumpName | Fields], [Code | List]);

parse_encode_cs_loop([#meta{name = Name, type = u8, explain = undefined, comment = Comment} | T], Depth, Parent, Ancestor, Scope, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),
    LastGetNames = lists:sublist(NewAncestor, 1),
    PathGetName = [lists:concat(["[\"", word:to_lower_hump(PathName), "\"]"]) || PathName <- LastGetNames, Ancestor =/= [] andalso Parent =/= list andalso Parent =/= ets],

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "writer.Write((System.Byte)", Scope, PathGetName, ");"
    ],
    parse_encode_cs_loop(T, Depth, Parent, Ancestor, Scope, [PathHumpName | Fields], [Code | List]);

parse_encode_cs_loop([#meta{name = Name, type = u16, explain = undefined, comment = Comment} | T], Depth, Parent, Ancestor, Scope, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),
    LastGetNames = lists:sublist(NewAncestor, 1),
    PathGetName = [lists:concat(["[\"", word:to_lower_hump(PathName), "\"]"]) || PathName <- LastGetNames, Ancestor =/= [] andalso Parent =/= list andalso Parent =/= ets],

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)(System.UInt16)", Scope, PathGetName, "));"
    ],
    parse_encode_cs_loop(T, Depth, Parent, Ancestor, Scope, [PathHumpName | Fields], [Code | List]);

parse_encode_cs_loop([#meta{name = Name, type = u32, explain = undefined, comment = Comment} | T], Depth, Parent, Ancestor, Scope, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),
    LastGetNames = lists:sublist(NewAncestor, 1),
    PathGetName = [lists:concat(["[\"", word:to_lower_hump(PathName), "\"]"]) || PathName <- LastGetNames, Ancestor =/= [] andalso Parent =/= list andalso Parent =/= ets],

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int32)(System.UInt32)", Scope, PathGetName, "));"
    ],
    parse_encode_cs_loop(T, Depth, Parent, Ancestor, Scope, [PathHumpName | Fields], [Code | List]);

parse_encode_cs_loop([#meta{name = Name, type = u64, explain = undefined, comment = Comment} | T], Depth, Parent, Ancestor, Scope, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),
    LastGetNames = lists:sublist(NewAncestor, 1),
    PathGetName = [lists:concat(["[\"", word:to_lower_hump(PathName), "\"]"]) || PathName <- LastGetNames, Ancestor =/= [] andalso Parent =/= list andalso Parent =/= ets],

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)(System.UInt64)", Scope, PathGetName, "));"
    ],
    parse_encode_cs_loop(T, Depth, Parent, Ancestor, Scope, [PathHumpName | Fields], [Code | List]);

parse_encode_cs_loop([#meta{name = Name, type = i8, explain = undefined, comment = Comment} | T], Depth, Parent, Ancestor, Scope, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),
    LastGetNames = lists:sublist(NewAncestor, 1),
    PathGetName = [lists:concat(["[\"", word:to_lower_hump(PathName), "\"]"]) || PathName <- LastGetNames, Ancestor =/= [] andalso Parent =/= list andalso Parent =/= ets],

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "writer.Write((System.SByte)", Scope, PathGetName, ");"
    ],
    parse_encode_cs_loop(T, Depth, Parent, Ancestor, Scope, [PathHumpName | Fields], [Code | List]);

parse_encode_cs_loop([#meta{name = Name, type = i16, explain = undefined, comment = Comment} | T], Depth, Parent, Ancestor, Scope, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),
    LastGetNames = lists:sublist(NewAncestor, 1),
    PathGetName = [lists:concat(["[\"", word:to_lower_hump(PathName), "\"]"]) || PathName <- LastGetNames, Ancestor =/= [] andalso Parent =/= list andalso Parent =/= ets],

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)", Scope, PathGetName, "));"
    ],
    parse_encode_cs_loop(T, Depth, Parent, Ancestor, Scope, [PathHumpName | Fields], [Code | List]);

parse_encode_cs_loop([#meta{name = Name, type = i32, explain = undefined, comment = Comment} | T], Depth, Parent, Ancestor, Scope, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),
    LastGetNames = lists:sublist(NewAncestor, 1),
    PathGetName = [lists:concat(["[\"", word:to_lower_hump(PathName), "\"]"]) || PathName <- LastGetNames, Ancestor =/= [] andalso Parent =/= list andalso Parent =/= ets],

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int32)", Scope, PathGetName, "));"
    ],
    parse_encode_cs_loop(T, Depth, Parent, Ancestor, Scope, [PathHumpName | Fields], [Code | List]);

parse_encode_cs_loop([#meta{name = Name, type = i64, explain = undefined, comment = Comment} | T], Depth, Parent, Ancestor, Scope, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),
    LastGetNames = lists:sublist(NewAncestor, 1),
    PathGetName = [lists:concat(["[\"", word:to_lower_hump(PathName), "\"]"]) || PathName <- LastGetNames, Ancestor =/= [] andalso Parent =/= list andalso Parent =/= ets],

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)", Scope, PathGetName, "));"
    ],
    parse_encode_cs_loop(T, Depth, Parent, Ancestor, Scope, [PathHumpName | Fields], [Code | List]);

parse_encode_cs_loop([#meta{name = Name, type = f32, explain = undefined, comment = Comment} | T], Depth, Parent, Ancestor, Scope, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),
    LastGetNames = lists:sublist(NewAncestor, 1),
    PathGetName = [lists:concat(["[\"", word:to_lower_hump(PathName), "\"]"]) || PathName <- LastGetNames, Ancestor =/= [] andalso Parent =/= list andalso Parent =/= ets],

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", PathHumpName, "Bytes = System.BitConverter.GetBytes((System.Single)", Scope, PathGetName, ");", "\n",
        Padding, "if (System.BitConverter.IsLittleEndian) System.Array.Reverse(", PathHumpName, "Bytes);", "\n",
        Padding, "writer.Write(", PathHumpName, "Bytes);"
    ],
    parse_encode_cs_loop(T, Depth, Parent, Ancestor, Scope, [PathHumpName | Fields], [Code | List]);

parse_encode_cs_loop([#meta{name = Name, type = f64, explain = undefined, comment = Comment} | T], Depth, Parent, Ancestor, Scope, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),
    LastGetNames = lists:sublist(NewAncestor, 1),
    PathGetName = [lists:concat(["[\"", word:to_lower_hump(PathName), "\"]"]) || PathName <- LastGetNames, Ancestor =/= [] andalso Parent =/= list andalso Parent =/= ets],

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", PathHumpName, "Bytes = System.BitConverter.GetBytes((System.Double)", Scope, PathGetName, ");", "\n",
        Padding, "if (System.BitConverter.IsLittleEndian) System.Array.Reverse(", PathHumpName, "Bytes);", "\n",
        Padding, "writer.Write(", PathHumpName, "Bytes);"
    ],
    parse_encode_cs_loop(T, Depth, Parent, Ancestor, Scope, [PathHumpName | Fields], [Code | List]);

parse_encode_cs_loop([#meta{name = Name, type = str, explain = undefined, comment = Comment} | T], Depth, Parent, Ancestor, Scope, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),
    LastGetNames = lists:sublist(NewAncestor, 1),
    PathGetName = [lists:concat(["[\"", word:to_lower_hump(PathName), "\"]"]) || PathName <- LastGetNames, Ancestor =/= [] andalso Parent =/= list andalso Parent =/= ets],

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", PathHumpName, "Bytes = encoding.GetBytes((System.String)", Scope, PathGetName, ");", "\n",
        Padding, "writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)", PathHumpName, "Bytes.Length));", "\n",
        Padding, "writer.Write(", PathHumpName, "Bytes);"
    ],
    parse_encode_cs_loop(T, Depth, Parent, Ancestor, Scope, [PathHumpName | Fields], [Code | List]);

parse_encode_cs_loop([#meta{name = Name, type = bst, explain = undefined, comment = Comment} | T], Depth, Parent, Ancestor, Scope, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),
    LastGetNames = lists:sublist(NewAncestor, 1),
    PathGetName = [lists:concat(["[\"", word:to_lower_hump(PathName), "\"]"]) || PathName <- LastGetNames, Ancestor =/= [] andalso Parent =/= list andalso Parent =/= ets],

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", PathHumpName, "Bytes = encoding.GetBytes((System.String)", Scope, PathGetName, ");", "\n",
        Padding, "writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)", PathHumpName, "Bytes.Length));", "\n",
        Padding, "writer.Write(", PathHumpName, "Bytes);"
    ],
    parse_encode_cs_loop(T, Depth, Parent, Ancestor, Scope, [PathHumpName | Fields], [Code | List]);

parse_encode_cs_loop([#meta{name = Name, type = rst, explain = undefined, comment = Comment} | T], Depth, Parent, Ancestor, Scope, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),
    LastGetNames = lists:sublist(NewAncestor, 1),
    PathGetName = [lists:concat(["[\"", word:to_lower_hump(PathName), "\"]"]) || PathName <- LastGetNames, Ancestor =/= [] andalso Parent =/= list andalso Parent =/= ets],

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", PathHumpName, "Bytes = encoding.GetBytes((System.String)", "(", "(System.Collections.Generic.Dictionary<System.String, System.Object>)", Scope, PathGetName, ")", ");", "\n",
        Padding, "writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)", PathHumpName, "Bytes.Length));", "\n",
        Padding, "writer.Write(", PathHumpName, "Bytes);"
    ],
    parse_encode_cs_loop(T, Depth, Parent, Ancestor, Scope, [PathHumpName | Fields], [Code | List]);

parse_encode_cs_loop([#meta{name = Name, type = tuple, explain = Explain} | T], Depth, Parent, Ancestor, Scope, Names, List) ->
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),
    LastGetNames = lists:sublist(NewAncestor, 1),
    PathGetName = [lists:concat(["[\"", word:to_lower_hump(PathName), "\"]"]) || PathName <- LastGetNames, Ancestor =/= [] andalso Parent =/= list andalso Parent =/= ets],
    SubExplain = tuple_to_list(Explain),
    CastName = lists:flatten([PathHumpName, ["Cast" || length(SubExplain) > 0 andalso Ancestor == []]]),

    SubCode = lists:concat([
        Padding, "// ", "convert", "\n",
        Padding, "var ", CastName, " = (System.Collections.Generic.Dictionary<System.String, System.Object>)", Scope, PathGetName, ";"
    ]),

    %% Ancestor =/= [] is means not include root 
    NewList = lists:append([SubCode || length(SubExplain) > 0], List),

    %% recursive
    {SubNames, SubCodes} = parse_encode_cs_loop(SubExplain, Depth, tuple, NewAncestor, CastName, Names, NewList),

    %% flatten
    parse_encode_cs_loop(T, Depth, Parent, Ancestor, Scope, lists:reverse(SubNames), lists:reverse(SubCodes));

parse_encode_cs_loop([#meta{name = Name, tag = Tag, type = record, explain = Explain} | T], Depth, Parent, Ancestor, Scope, Names, List) ->
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    NewTagAncestor = lists:append([[Name || Ancestor == []], [Tag || Ancestor =/= []], Ancestor]),
    PathNames = lists:reverse(lists:sublist(NewTagAncestor, max(1, length(NewTagAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),
    LastGetNames = lists:sublist(NewAncestor, 1),
    PathGetName = [lists:concat(["[\"", word:to_lower_hump(PathName), "\"]"]) || PathName <- LastGetNames, Ancestor =/= [] andalso Parent =/= list andalso Parent =/= ets],
    SubExplain = [Meta || Meta = #meta{} <- tuple_to_list(Explain)],
    CastName = lists:flatten([PathHumpName, ["Cast" || length(SubExplain) > 0 andalso Ancestor == []]]),
    
    SubCode = lists:concat([
        Padding, "// ", "convert", "\n",
        Padding, "var ", CastName, " = (System.Collections.Generic.Dictionary<System.String, System.Object>)", Scope, PathGetName, ";"
    ]),

    %% Ancestor =/= [] is means not include root 
    NewList = lists:append([SubCode || length(SubExplain) > 0], List),

    %% recursive
    {SubNames, SubCodes} = parse_encode_cs_loop(SubExplain, Depth, tuple, NewAncestor, CastName, Names, NewList),

    %% flatten
    parse_encode_cs_loop(T, Depth, Parent, Ancestor, Scope, lists:reverse(SubNames), lists:reverse(SubCodes));

parse_encode_cs_loop([#meta{name = Name, type = maps, explain = Explain} | T], Depth, Parent, Ancestor, Scope, Names, List) ->
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),
    LastGetNames = lists:sublist(NewAncestor, 1),
    PathGetName = [lists:concat(["[\"", word:to_lower_hump(PathName), "\"]"]) || PathName <- LastGetNames, Ancestor =/= [] andalso Parent =/= list andalso Parent =/= ets],
    SubExplain = maps:values(Explain),
    CastName = lists:flatten([PathHumpName, ["Cast" || length(SubExplain) > 0 andalso Ancestor == []]]),
    
    SubCode = lists:concat([
        Padding, "// ", "convert", "\n",
        Padding, "var ", CastName, " = (System.Collections.Generic.Dictionary<System.String, System.Object>)", Scope, PathGetName, ";"
    ]),

    %% Ancestor =/= [] is means not include root 
    NewList = lists:append([SubCode || length(SubExplain) > 0], List),

    %% recursive
    {SubNames, SubCodes} = parse_encode_cs_loop(SubExplain, Depth, tuple, NewAncestor, CastName, Names, NewList),

    %% flatten
    parse_encode_cs_loop(T, Depth, Parent, Ancestor, Scope, lists:reverse(SubNames), lists:reverse(SubCodes));

parse_encode_cs_loop([#meta{name = Name, type = list, explain = Explain, comment = Comment, key = undefined} | T], Depth, Parent, Ancestor, Scope, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),
    LastGetNames = lists:sublist(NewAncestor, 1),
    PathGetName = [lists:concat(["[\"", word:to_lower_hump(PathName), "\"]"]) || PathName <- LastGetNames, Ancestor =/= [] andalso Parent =/= list andalso Parent =/= ets],
    CastName = lists:flatten([PathHumpName, ["Cast" || Ancestor == []]]),
    NewScope = lists:concat([CastName, "ItemRaw"]),

    %% recursive
    {_, SubCodes} = parse_encode_cs_loop(Explain, Depth + 1, list, NewAncestor, NewScope, [], []),

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", CastName, " = (System.Collections.Generic.List<System.Object>)", Scope, PathGetName, ";", "\n",
        Padding, "writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)", CastName, ".Count));", "\n",
        Padding, "foreach(var ", CastName, "ItemRaw in ", CastName, ")", "\n",
        Padding, "{", "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "}"
    ],
    parse_encode_cs_loop(T, Depth, Parent, Ancestor, Scope, [CastName, lists:concat([CastName, "Length"]) | Fields], [Code | List]);

parse_encode_cs_loop([#meta{name = Name, type = list, explain = Explain, comment = Comment, key = _} | T], Depth, Parent, Ancestor, Scope, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),
    LastGetNames = lists:sublist(NewAncestor, 1),
    PathGetName = [lists:concat(["[\"", word:to_lower_hump(PathName), "\"]"]) || PathName <- LastGetNames, Ancestor =/= [] andalso Parent =/= list andalso Parent =/= ets],
    CastName = lists:flatten([PathHumpName, ["Cast" || Ancestor == []]]),
    NewScope = lists:concat([CastName, "ItemRaw.Value"]),

    %% recursive
    {_, SubCodes} = parse_encode_cs_loop(Explain, Depth + 1, list, NewAncestor, NewScope, [], []),

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", CastName, " = (System.Collections.Generic.Dictionary<System.Object, System.Collections.Generic.Dictionary<System.String, System.Object>>)", Scope, PathGetName, ";", "\n",
        Padding, "writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)", CastName, ".Count));", "\n",
        Padding, "foreach(var ", CastName, "ItemRaw in ", CastName, ")", "\n",
        Padding, "{", "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "}"
    ],
    parse_encode_cs_loop(T, Depth, Parent, Ancestor, Scope, [CastName, lists:concat([CastName, "Length"]) | Fields], [Code | List]).

%%%===================================================================
%%% decode
%%%===================================================================
%% cs code
parse_decode_cs(Protocol, Meta) ->
    %% start with 3 tabs(4 space) padding
    Padding = lists:duplicate(3, "    "),

    {Fields, Codes} = parse_decode_cs_loop([Meta], 4, [], [], [], []),

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

parse_decode_cs_loop([], _, _, _, Fields, List) ->
    %% construct as a list
    {lists:reverse(Fields), lists:reverse(List)};

parse_decode_cs_loop([#meta{name = _, type = zero} | T], Depth, Parent, Ancestor, Fields, List) ->

    parse_decode_cs_loop(T, Depth, Parent, Ancestor, Fields, List);

parse_decode_cs_loop([#meta{name = Name, type = binary, explain = Length, comment = Comment} | T], Depth, Parent, Ancestor, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", PathHumpName, " = reader.ReadBytes(", integer_to_list(Length), ");"
    ],
    parse_decode_cs_loop(T, Depth, Parent, Ancestor, [PathHumpName | Fields], [Code | List]);

parse_decode_cs_loop([#meta{name = Name, type = bool, explain = undefined, comment = Comment} | T], Depth, Parent, Ancestor, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", PathHumpName, " = reader.ReadByte() != 0;"
    ],
    parse_decode_cs_loop(T, Depth, Parent, Ancestor, [PathHumpName | Fields], [Code | List]);

parse_decode_cs_loop([#meta{name = Name, type = u8, explain = undefined, comment = Comment} | T], Depth, Parent, Ancestor, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", PathHumpName, " = reader.ReadByte();"
    ],
    parse_decode_cs_loop(T, Depth, Parent, Ancestor, [PathHumpName | Fields], [Code | List]);

parse_decode_cs_loop([#meta{name = Name, type = u16, explain = undefined, comment = Comment} | T], Depth, Parent, Ancestor, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", PathHumpName, " = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());"
    ],
    parse_decode_cs_loop(T, Depth, Parent, Ancestor, [PathHumpName | Fields], [Code | List]);

parse_decode_cs_loop([#meta{name = Name, type = u32, explain = undefined, comment = Comment} | T], Depth, Parent, Ancestor, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", PathHumpName, " = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());"
    ],
    parse_decode_cs_loop(T, Depth, Parent, Ancestor, [PathHumpName | Fields], [Code | List]);

parse_decode_cs_loop([#meta{name = Name, type = u64, explain = undefined, comment = Comment} | T], Depth, Parent, Ancestor, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", PathHumpName, " = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());"
    ],
    parse_decode_cs_loop(T, Depth, Parent, Ancestor, [PathHumpName | Fields], [Code | List]);

parse_decode_cs_loop([#meta{name = Name, type = i8, explain = undefined, comment = Comment} | T], Depth, Parent, Ancestor, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", PathHumpName, " = reader.ReadSByte();"
    ],
    parse_decode_cs_loop(T, Depth, Parent, Ancestor, [PathHumpName | Fields], [Code | List]);

parse_decode_cs_loop([#meta{name = Name, type = i16, explain = undefined, comment = Comment} | T], Depth, Parent, Ancestor, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", PathHumpName, " = System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());"
    ],
    parse_decode_cs_loop(T, Depth, Parent, Ancestor, [PathHumpName | Fields], [Code | List]);

parse_decode_cs_loop([#meta{name = Name, type = i32, explain = undefined, comment = Comment} | T], Depth, Parent, Ancestor, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", PathHumpName, " = System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());"
    ],
    parse_decode_cs_loop(T, Depth, Parent, Ancestor, [PathHumpName | Fields], [Code | List]);

parse_decode_cs_loop([#meta{name = Name, type = i64, explain = undefined, comment = Comment} | T], Depth, Parent, Ancestor, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", PathHumpName, " = System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());"
    ],
    parse_decode_cs_loop(T, Depth, Parent, Ancestor, [PathHumpName | Fields], [Code | List]);

parse_decode_cs_loop([#meta{name = Name, type = f32, explain = undefined, comment = Comment} | T], Depth, Parent, Ancestor, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", PathHumpName, " = System.BitConverter.ToSingle(System.BitConverter.GetBytes(System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32())), 0);"
    ],
    parse_decode_cs_loop(T, Depth, Parent, Ancestor, [PathHumpName | Fields], [Code | List]);

parse_decode_cs_loop([#meta{name = Name, type = f64, explain = undefined, comment = Comment} | T], Depth, Parent, Ancestor, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", PathHumpName, " = System.BitConverter.ToDouble(System.BitConverter.GetBytes(System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64())), 0);"
    ],
    parse_decode_cs_loop(T, Depth, Parent, Ancestor, [PathHumpName | Fields], [Code | List]);

parse_decode_cs_loop([#meta{name = Name, type = str, explain = undefined, comment = Comment} | T], Depth, Parent, Ancestor, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", PathHumpName, "Length = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());", "\n",
        Padding, "var ", PathHumpName, " = encoding.GetString(reader.ReadBytes(", PathHumpName, "Length));"
    ],
    parse_decode_cs_loop(T, Depth, Parent, Ancestor, [PathHumpName | Fields], [Code | List]);

parse_decode_cs_loop([#meta{name = Name, type = bst, explain = undefined, comment = Comment} | T], Depth, Parent, Ancestor, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", PathHumpName, "Length = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());", "\n",
        Padding, "var ", PathHumpName, " = encoding.GetString(reader.ReadBytes(", PathHumpName, "Length));"
    ],
    parse_decode_cs_loop(T, Depth, Parent, Ancestor, [PathHumpName | Fields], [Code | List]);

parse_decode_cs_loop([#meta{name = Name, type = rst, explain = undefined, comment = Comment} | T], Depth, Parent, Ancestor, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),

    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", PathHumpName, "Length = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());", "\n",
        Padding, "var ", PathHumpName, " = encoding.GetString(reader.ReadBytes(", PathHumpName, "Length));"
    ],
    parse_decode_cs_loop(T, Depth, Parent, Ancestor, [PathHumpName | Fields], [Code | List]);

parse_decode_cs_loop([#meta{name = Name, type = tuple, explain = Explain, comment = Comment} | T], Depth, Parent, Ancestor, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),

    SubExplain = [Meta || Meta = #meta{type = SubType} <- tuple_to_list(Explain), SubType =/= zero],
    NewNextAncestor = lists:append([Name || Parent =/= list andalso Parent =/= ets], Ancestor),

    %% recursive
    {SubFields, SubCodes} = parse_decode_cs_loop(SubExplain, Depth, tuple, NewNextAncestor, [], []),

    Code = [
        Padding, "// ", Comment, "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "// object", "\n",
        Padding, "var ", PathHumpName, " = new System.Collections.Generic.Dictionary<System.String, System.Object>() {", string:join([lists:concat(["{\"", word:to_lower_hump(FieldName), "\", ", PathName, "}"]) || {#meta{name = FieldName}, PathName} <- lists:zip(SubExplain, SubFields)], ", "), "};"
    ],

    %% flatten
    parse_decode_cs_loop(T, Depth, Parent, Ancestor, [PathHumpName | Fields], [Code | List]);

parse_decode_cs_loop([#meta{name = Name, tag = Tag, type = record, explain = Explain, comment = Comment} | T], Depth, Parent, Ancestor, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Tag | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),

    SubExplain = [Meta || Meta = #meta{} <- tuple_to_list(Explain)],
    NewNextAncestor = lists:append([Name || Parent =/= list andalso Parent =/= ets], Ancestor),

    %% recursive
    {SubFields, SubCodes} = parse_decode_cs_loop(SubExplain, Depth, record, NewNextAncestor, [], []),

    Code = [
        Padding, "// ", Comment, "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "// object", "\n",
        Padding, "var ", PathHumpName, " = new System.Collections.Generic.Dictionary<System.String, System.Object>() {", string:join([lists:concat(["{\"", word:to_lower_hump(FieldName), "\", ", PathName, "}"]) || {#meta{name = FieldName}, PathName} <- lists:zip(SubExplain, SubFields)], ", "), "};"
    ],

    %% flatten
    parse_decode_cs_loop(T, Depth, Parent, Ancestor, [PathHumpName | Fields], [Code | List]);

parse_decode_cs_loop([#meta{name = Name, type = maps, explain = Explain, comment = Comment} | T], Depth, Parent, Ancestor, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),

    SubExplain = maps:values(Explain),
    NewNextAncestor = lists:append([Name || Parent =/= list andalso Parent =/= ets], Ancestor),

    %% recursive
    {SubFields, SubCodes} = parse_decode_cs_loop(SubExplain, Depth, maps, NewNextAncestor, [], []),

    Code = [
        Padding, "// ", Comment, "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "// object", "\n",
        Padding, "var ", PathHumpName, " = new System.Collections.Generic.Dictionary<System.String, System.Object>() {", string:join([lists:concat(["{\"", word:to_lower_hump(FieldName), "\", ", PathName, "}"]) || {#meta{name = FieldName}, PathName} <- lists:zip(SubExplain, SubFields)], ", "), "};"
    ],

    %% flatten
    parse_decode_cs_loop(T, Depth, Parent, Ancestor, [PathHumpName | Fields], [Code | List]);

parse_decode_cs_loop([#meta{name = Name, type = list, explain = Explain, comment = Comment, key = undefined} | T], Depth, Parent, Ancestor, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),
    NewNextAncestor = lists:append([Name || Parent =/= list andalso Parent =/= ets], Ancestor),

    %% recursive
    {SubFields, SubCodes} = parse_decode_cs_loop(Explain, Depth + 1, list, NewNextAncestor, [], []),
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
    parse_decode_cs_loop(T, Depth, Parent, Ancestor, [PathHumpName | Fields], [Code | List]);

parse_decode_cs_loop([#meta{name = Name, type = list, explain = Explain, comment = Comment, key = Key} | T], Depth, Parent, Ancestor, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),
    NewNextAncestor = lists:append([Name || Parent =/= list andalso Parent =/= ets], Ancestor),

    %% recursive
    {SubFields, SubCodes} = parse_decode_cs_loop(Explain, Depth + 1, list, NewNextAncestor, [], []),
    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", PathHumpName, "Length = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());", "\n",
        Padding, "var ", PathHumpName, " = new System.Collections.Generic.Dictionary<System.Object, System.Collections.Generic.Dictionary<System.String, System.Object>>(", PathHumpName, "Length);", "\n",
        Padding, "while (", PathHumpName, "Length-- > 0)", "\n",
        Padding, "{", "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "    // add", "\n",
        Padding, "    ", PathHumpName, "[", PathHumpName, word:to_hump(Key), "] = ", string:join(SubFields, ", "), ";", "\n",
        Padding, "}"
    ],
    parse_decode_cs_loop(T, Depth, Parent, Ancestor, [PathHumpName | Fields], [Code | List]);

parse_decode_cs_loop([#meta{name = Name, type = ets, explain = Explain, comment = Comment, key = []} | T], Depth, Parent, Ancestor, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),
    NewNextAncestor = lists:append([Name || Parent =/= list andalso Parent =/= ets], Ancestor),

    %% recursive
    {SubFields, SubCodes} = parse_decode_cs_loop(Explain, Depth + 1, ets, NewNextAncestor, [], []),
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
    parse_decode_cs_loop(T, Depth, Parent, Ancestor, [PathHumpName | Fields], [Code | List]);

parse_decode_cs_loop([#meta{name = Name, type = ets, explain = Explain, comment = Comment, key = [Key]} | T], Depth, Parent, Ancestor, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),
    NewNextAncestor = lists:append([Name || Parent =/= list andalso Parent =/= ets], Ancestor),

    %% recursive
    {SubFields, SubCodes} = parse_decode_cs_loop(Explain, Depth + 1, ets, NewNextAncestor, [], []),
    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", PathHumpName, "Length = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());", "\n",
        Padding, "var ", PathHumpName, " = new System.Collections.Generic.Dictionary<System.Object, System.Collections.Generic.Dictionary<System.String, System.Object>>(", PathHumpName, "Length);", "\n",
        Padding, "while (", PathHumpName, "Length-- > 0)", "\n",
        Padding, "{", "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "    // add", "\n",
        Padding, "    ", PathHumpName, "[", PathHumpName, word:to_hump(Key), "] = ", string:join(SubFields, ", "), ";", "\n",
        Padding, "}"
    ],
    parse_decode_cs_loop(T, Depth, Parent, Ancestor, [PathHumpName | Fields], [Code | List]).
