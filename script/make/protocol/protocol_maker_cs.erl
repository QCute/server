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
        "    ", "public static void Encode(System.Text.Encoding encoding, System.IO.BinaryWriter writer, System.UInt16 protocol, System.Collections.Generic.Dictionary<System.String, System.Object> data) ", "\n",
        "    ", "{", "\n",
        "    ", "    ", "switch (protocol) ", "\n",
        "    ", "    ", "{", "\n",
        string:join(Encode, "\n"),
        "    ", "    ", "}", "\n",
        "    ", "}", "\n\n",
        "    ", "public static System.Collections.Generic.Dictionary<System.String, System.Object> Decode(System.Text.Encoding encoding, System.IO.BinaryReader reader, System.UInt16 protocol) ", "\n",
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
        "    ", "    ", "    ", "    ", "{\"write\", new List() ", Read, "},", "\n",
        "    ", "    ", "    ", "    ", "{\"read\", new List() ", Write, "}", "\n",
        "    ", "    ", "    ", "}}"
    ]) || {Protocol, Comment, #data{cs = #set{meta = #file{extra = Read}}}, #data{cs = #set{meta = #file{extra = Write}}}} <- List, Protocol =/= 0], ",\n"),
    lists:concat([
        "using List = System.Collections.ArrayList;", "\n",
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
parse_meta_cs(_, []) ->
    "{}";
parse_meta_cs(_, Meta) ->
    %% start with 3 tabs(4 space) padding
    Padding = lists:duplicate(4, "    "),
    MetaData = parse_meta_cs_loop(Meta, 5, []),
    %% format one protocol define
    %% lists:concat(["        \"", Protocol, "\" : [\n", MetaData, "\n        ]"]).
    Code = lists:concat(["{\n", MetaData, "\n", Padding, "}"]),

    #file{extra = Code}.

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

parse_meta_cs_loop([#meta{name = Name, type = Type, explain = [], comment = Comment} | T], Depth, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    Code = lists:flatten(io_lib:format("~snew Map() { {\"name\", \"~s\"}, {\"type\", \"~s\"}, {\"comment\", \"~ts\"}, {\"explain\", new List()} }", [Padding, word:to_lower_hump(Name), Type, Comment])),
    parse_meta_cs_loop(T, Depth, [Code | List]);

parse_meta_cs_loop([#meta{name = _, type = tuple, comment = _, explain = Explain = [_ | _]} | T], Depth, List) ->
    %% recursive
    SubCodes = parse_meta_cs_loop(Explain, Depth, []),
    parse_meta_cs_loop(T, Depth, [SubCodes | List]);

parse_meta_cs_loop([#meta{name = _, type = record, comment = _, explain = Explain = [_ | _]} | T], Depth, List) ->
    %% recursive
    SubCodes = parse_meta_cs_loop(Explain, Depth, []),
    parse_meta_cs_loop(T, Depth, [SubCodes | List]);

parse_meta_cs_loop([#meta{name = Name, type = list, comment = Comment, explain = Explain = [_ | _], key = undefined} | T], Depth, List) ->
    %% recursive
    SubCodes = parse_meta_cs_loop(Explain, Depth + 1, []),
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    Code = lists:flatten(io_lib:format("~snew Map() { {\"name\", \"~s\"}, {\"type\", \"~s\"}, {\"comment\", \"~ts\"}, {\"explain\", new List() {\n~ts\n~s}}}", [Padding, word:to_lower_hump(Name), list, Comment, SubCodes, Padding])),
    parse_meta_cs_loop(T, Depth, [Code | List]);

parse_meta_cs_loop([#meta{name = Name, type = list, comment = Comment, explain = Explain = [_ | _], key = Key} | T], Depth, List) ->
    %% recursive
    SubCodes = parse_meta_cs_loop(Explain, Depth + 1, []),
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    Code = lists:flatten(io_lib:format("~snew Map() { {\"name\", \"~s\"}, {\"type\", \"~s\"}, {\"comment\", \"~ts\"}, {\"key\", \"~ts\"}, {\"explain\", new List() {\n~ts\n~s}}}", [Padding, word:to_lower_hump(Name), map, Comment, word:to_lower_hump(Key), SubCodes, Padding])),
    parse_meta_cs_loop(T, Depth, [Code | List]);

parse_meta_cs_loop([#meta{name = Name, type = ets, comment = Comment, explain = Explain = [_ | _], key = undefined} | T], Depth, List) ->
    %% recursive
    SubCodes = parse_meta_cs_loop(Explain, Depth + 1, []),
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    Code = lists:flatten(io_lib:format("~snew Map() { {\"name\", \"~s\"}, {\"type\", \"~s\"}, {\"comment\", \"~ts\"}, {\"explain\", new List() {\n~ts\n~s}}}", [Padding, word:to_lower_hump(Name), list, Comment, SubCodes, Padding])),
    parse_meta_cs_loop(T, Depth, [Code | List]);

parse_meta_cs_loop([#meta{name = Name, type = ets, comment = Comment, explain = Explain = [_ | _], key = Key} | T], Depth, List) ->
    %% recursive
    SubCodes = parse_meta_cs_loop(Explain, Depth + 1, []),
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    Code = lists:flatten(io_lib:format("~snew Map() { {\"name\", \"~s\"}, {\"type\", \"~s\"}, {\"comment\", \"~ts\"}, {\"key\", \"~ts\"}, {\"explain\", new List() {\n~ts\n~s}}}", [Padding, word:to_lower_hump(Name), map, Comment, word:to_lower_hump(Key), SubCodes, Padding])),
    parse_meta_cs_loop(T, Depth, [Code | List]).

%%%===================================================================
%%% encode
%%%===================================================================
%% cs code
parse_encode_cs(Protocol, Meta) ->
    %% start with 3 tabs(4 space) padding
    Padding = lists:duplicate(3, "    "),
    {_, Codes} = parse_encode_cs_loop(Meta, 4, "data", [], []),

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

parse_encode_cs_loop([], _, _, Fields, List) ->
    %% construct as a list
    %% {string:join(lists:reverse(Fields), ", "), string:join(lists:reverse(List), "\n")};
    {lists:reverse(Fields), lists:reverse(List)};

parse_encode_cs_loop([#meta{name = Name, type = binary, explain = _, comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "writer.Write((System.Byte[])", ScopeArgs, "[\"", HumpName, "\"]);"
    ],
    parse_encode_cs_loop(T, Depth, ScopeArgs, [HumpName | Fields], [Code | List]);

parse_encode_cs_loop([#meta{name = Name, type = bool, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "writer.Write((System.Byte)((System.Boolean)", ScopeArgs, "[\"", HumpName, "\"] ? 1 : 0));"
    ],
    parse_encode_cs_loop(T, Depth, ScopeArgs, [HumpName | Fields], [Code | List]);

parse_encode_cs_loop([#meta{name = Name, type = u8, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "writer.Write((System.Byte)", ScopeArgs, "[\"", HumpName, "\"]);"
    ],
    parse_encode_cs_loop(T, Depth, ScopeArgs, [HumpName | Fields], [Code | List]);

parse_encode_cs_loop([#meta{name = Name, type = u16, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)(System.UInt16)", ScopeArgs, "[\"", HumpName, "\"]));"
    ],
    parse_encode_cs_loop(T, Depth, ScopeArgs, [HumpName | Fields], [Code | List]);

parse_encode_cs_loop([#meta{name = Name, type = u32, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int32)(System.UInt32)", ScopeArgs, "[\"", HumpName, "\"]));"
    ],
    parse_encode_cs_loop(T, Depth, ScopeArgs, [HumpName | Fields], [Code | List]);

parse_encode_cs_loop([#meta{name = Name, type = u64, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)(System.UInt64)", ScopeArgs, "[\"", HumpName, "\"]));"
    ],
    parse_encode_cs_loop(T, Depth, ScopeArgs, [HumpName | Fields], [Code | List]);

parse_encode_cs_loop([#meta{name = Name, type = i8, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "writer.Write((System.SByte)", ScopeArgs, "[\"", HumpName, "\"]);"
    ],
    parse_encode_cs_loop(T, Depth, ScopeArgs, [HumpName | Fields], [Code | List]);

parse_encode_cs_loop([#meta{name = Name, type = i16, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)", ScopeArgs, "[\"", HumpName, "\"]));"
    ],
    parse_encode_cs_loop(T, Depth, ScopeArgs, [HumpName | Fields], [Code | List]);

parse_encode_cs_loop([#meta{name = Name, type = i32, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int32)", ScopeArgs, "[\"", HumpName, "\"]));"
    ],
    parse_encode_cs_loop(T, Depth, ScopeArgs, [HumpName | Fields], [Code | List]);

parse_encode_cs_loop([#meta{name = Name, type = i64, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)", ScopeArgs, "[\"", HumpName, "\"]));"
    ],
    parse_encode_cs_loop(T, Depth, ScopeArgs, [HumpName | Fields], [Code | List]);

parse_encode_cs_loop([#meta{name = Name, type = f32, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", HumpName, "Bytes = System.BitConverter.GetBytes((System.Single)", ScopeArgs, "[\"", HumpName, "\"]);", "\n",
        Padding, "if (System.BitConverter.IsLittleEndian) System.Array.Reverse(", HumpName, "Bytes);", "\n",
        Padding, "writer.Write(", HumpName, "Bytes);"
    ],
    parse_encode_cs_loop(T, Depth, ScopeArgs, [HumpName | Fields], [Code | List]);

parse_encode_cs_loop([#meta{name = Name, type = f64, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", HumpName, "Bytes = System.BitConverter.GetBytes((System.Double)", ScopeArgs, "[\"", HumpName, "\"]);", "\n",
        Padding, "if (System.BitConverter.IsLittleEndian) System.Array.Reverse(", HumpName, "Bytes);", "\n",
        Padding, "writer.Write(", HumpName, "Bytes);"
    ],
    parse_encode_cs_loop(T, Depth, ScopeArgs, [HumpName | Fields], [Code | List]);

parse_encode_cs_loop([#meta{name = Name, type = str, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", HumpName, "Bytes = encoding.GetBytes((System.String)", ScopeArgs, "[\"", HumpName, "\"]);", "\n",
        Padding, "writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)", HumpName, "Bytes.Length));", "\n",
        Padding, "writer.Write(", HumpName, "Bytes);"
    ],
    parse_encode_cs_loop(T, Depth, ScopeArgs, [HumpName | Fields], [Code | List]);

parse_encode_cs_loop([#meta{name = Name, type = bst, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", HumpName, "Bytes = encoding.GetBytes((System.String)", ScopeArgs, "[\"", HumpName, "\"]);", "\n",
        Padding, "writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)", HumpName, "Bytes.Length));", "\n",
        Padding, "writer.Write(", HumpName, "Bytes);"
    ],
    parse_encode_cs_loop(T, Depth, ScopeArgs, [HumpName | Fields], [Code | List]);

parse_encode_cs_loop([#meta{name = Name, type = rst, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", HumpName, "Bytes = encoding.GetBytes((System.String)", ScopeArgs, "[\"", HumpName, "\"]);", "\n",
        Padding, "writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)", HumpName, "Bytes.Length));", "\n",
        Padding, "writer.Write(", HumpName, "Bytes);"
    ],
    parse_encode_cs_loop(T, Depth, ScopeArgs, [HumpName | Fields], [Code | List]);

parse_encode_cs_loop([#meta{name = _, type = tuple, explain = Explain} | T], Depth, ScopeArgs, Names, List) ->
    %% recursive
    {SubNames, SubCodes} = parse_encode_cs_loop(Explain, Depth, ScopeArgs, Names, List),

    %% flatten
    parse_encode_cs_loop(T, Depth, ScopeArgs, lists:reverse(SubNames), lists:reverse(SubCodes));

parse_encode_cs_loop([#meta{name = _, type = record, explain = Explain} | T], Depth, ScopeArgs, Names, List) ->
    %% recursive
    {SubNames, SubCodes} = parse_encode_cs_loop(Explain, Depth, ScopeArgs, Names, List),

    %% flatten
    parse_encode_cs_loop(T, Depth, ScopeArgs, lists:reverse(SubNames), lists:reverse(SubCodes));


parse_encode_cs_loop([#meta{name = Name, type = list, comment = Comment, explain = Explain, key = undefined} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    %% recursive
    {_, SubCodes} = parse_encode_cs_loop(Explain, Depth + 1, lists:concat([HumpName, "DataItem"]), [], []),
    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", HumpName, "Data = (System.Collections.ArrayList)", ScopeArgs, "[\"", HumpName, "\"];", "\n",
        Padding, "writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)", HumpName, "Data.Count));", "\n",
        Padding, "foreach(System.Collections.Generic.Dictionary<System.String, System.Object> ", HumpName, "DataItem in ", HumpName, "Data)", "\n",
        Padding, "{", "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "}"
    ],
    parse_encode_cs_loop(T, Depth, ScopeArgs, [HumpName, lists:concat([HumpName, "Length"]) | Fields], [Code | List]);

parse_encode_cs_loop([#meta{name = Name, type = list, comment = Comment, explain = Explain = [#meta{explain = SubExplain}], key = Key} | T], Depth, ScopeArgs, Fields, List) ->
    Field = lists:keyfind(Key, #meta.name, SubExplain),
    Field == undefined andalso erlang:throw(lists:flatten(io_lib:format("Cound not found field ~ts in explain", [Key]))),
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    %% recursive
    {_, SubCodes} = parse_encode_cs_loop(Explain, Depth + 1, lists:concat([HumpName, "DataItem.Value"]), [], []),
    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", HumpName, "Data = (System.Collections.Generic.Dictionary<System.Object, System.Collections.Generic.Dictionary<System.String, System.Object>>)", ScopeArgs, "[\"", HumpName, "\"];", "\n",
        Padding, "writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)", HumpName, "Data.Count));", "\n",
        Padding, "foreach(System.Collections.Generic.KeyValuePair<System.Object, System.Collections.Generic.Dictionary<System.String, System.Object>> ", HumpName, "DataItem in ", HumpName, "Data)", "\n",
        Padding, "{", "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "}"
    ],
    parse_encode_cs_loop(T, Depth, ScopeArgs, [HumpName, lists:concat([HumpName, "Length"]) | Fields], [Code | List]).

%%%===================================================================
%%% decode
%%%===================================================================
%% cs code
parse_decode_cs(Protocol, Meta) ->
    %% start with 3 tabs(4 space) padding
    Padding = lists:duplicate(3, "    "),
    {Fields, Codes} = parse_decode_cs_loop(Meta, 4, [], []),

    %% codes format
    CodesDefault = lists:concat([
        Padding, "    ", "return new System.Collections.Generic.Dictionary<System.String, System.Object>() {", string:join([lists:concat(["{\"", Name, "\", ", Name, "}"]) || Name <- Fields], ", "), "};", "\n"
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

parse_decode_cs_loop([], _, Fields, List) ->
    %% construct as a list
    %% {string:join([lists:concat(["{\"", Name, "\", ", Name, "}"]) || Name <- lists:reverse(Fields)], ", "), string:join(lists:reverse(List), "\n")};
    {lists:reverse(Fields), lists:reverse(List)};

parse_decode_cs_loop([#meta{name = _, type = zero} | T], Depth, Fields, List) ->

    parse_decode_cs_loop(T, Depth, Fields, List);

parse_decode_cs_loop([#meta{name = Name, type = binary, explain = Length, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", HumpName, " = reader.ReadBytes(", integer_to_list(Length), ");"
    ],
    parse_decode_cs_loop(T, Depth, [HumpName | Fields], [Code | List]);

parse_decode_cs_loop([#meta{name = Name, type = bool, explain = [], comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", HumpName, " = reader.ReadByte() != 0;"
    ],
    parse_decode_cs_loop(T, Depth, [HumpName | Fields], [Code | List]);

parse_decode_cs_loop([#meta{name = Name, type = u8, explain = [], comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", HumpName, " = reader.ReadByte();"
    ],
    parse_decode_cs_loop(T, Depth, [HumpName | Fields], [Code | List]);

parse_decode_cs_loop([#meta{name = Name, type = u16, explain = [], comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", HumpName, " = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());"
    ],
    parse_decode_cs_loop(T, Depth, [HumpName | Fields], [Code | List]);

parse_decode_cs_loop([#meta{name = Name, type = u32, explain = [], comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", HumpName, " = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());"
    ],
    parse_decode_cs_loop(T, Depth, [HumpName | Fields], [Code | List]);

parse_decode_cs_loop([#meta{name = Name, type = u64, explain = [], comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", HumpName, " = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());"
    ],
    parse_decode_cs_loop(T, Depth, [HumpName | Fields], [Code | List]);

parse_decode_cs_loop([#meta{name = Name, type = i8, explain = [], comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", HumpName, " = reader.ReadSByte();"
    ],
    parse_decode_cs_loop(T, Depth, [HumpName | Fields], [Code | List]);

parse_decode_cs_loop([#meta{name = Name, type = i16, explain = [], comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", HumpName, " = System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());"
    ],
    parse_decode_cs_loop(T, Depth, [HumpName | Fields], [Code | List]);

parse_decode_cs_loop([#meta{name = Name, type = i32, explain = [], comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", HumpName, " = System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());"
    ],
    parse_decode_cs_loop(T, Depth, [HumpName | Fields], [Code | List]);

parse_decode_cs_loop([#meta{name = Name, type = i64, explain = [], comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", HumpName, " = System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());"
    ],
    parse_decode_cs_loop(T, Depth, [HumpName | Fields], [Code | List]);

parse_decode_cs_loop([#meta{name = Name, type = f32, explain = [], comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", HumpName, " = System.BitConverter.ToSingle(System.BitConverter.GetBytes(System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32())), 0);"
    ],
    parse_decode_cs_loop(T, Depth, [HumpName | Fields], [Code | List]);

parse_decode_cs_loop([#meta{name = Name, type = f64, explain = [], comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", HumpName, " = System.BitConverter.ToDouble(System.BitConverter.GetBytes(System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64())), 0);"
    ],
    parse_decode_cs_loop(T, Depth, [HumpName | Fields], [Code | List]);

parse_decode_cs_loop([#meta{name = Name, type = str, explain = [], comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", HumpName, "Length = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());", "\n",
        Padding, "var ", HumpName, " = encoding.GetString(reader.ReadBytes(", HumpName, "Length));"
    ],
    parse_decode_cs_loop(T, Depth, [HumpName | Fields], [Code | List]);

parse_decode_cs_loop([#meta{name = Name, type = bst, explain = [], comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", HumpName, "Length = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());", "\n",
        Padding, "var ", HumpName, " = encoding.GetString(reader.ReadBytes(", HumpName, "Length));"
    ],
    parse_decode_cs_loop(T, Depth, [HumpName | Fields], [Code | List]);

parse_decode_cs_loop([#meta{name = Name, type = rst, explain = [], comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", HumpName, "Length = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());", "\n",
        Padding, "var ", HumpName, " = encoding.GetString(reader.ReadBytes(", HumpName, "Length));"
    ],
    parse_decode_cs_loop(T, Depth, [HumpName | Fields], [Code | List]);

parse_decode_cs_loop([#meta{name = _, type = tuple, comment = _, explain = Explain} | T], Depth, Fields, List) ->
    %% recursive
    {SubFields, SubCodes} = parse_decode_cs_loop(Explain, Depth, Fields, List),

    %% flatten
    parse_decode_cs_loop(T, Depth, lists:reverse(SubFields), lists:reverse(SubCodes));

parse_decode_cs_loop([#meta{name = _, type = record, comment = _, explain = Explain} | T], Depth, Fields, List) ->
    %% recursive
    {SubFields, SubCodes} = parse_decode_cs_loop(Explain, Depth, Fields, List),

    %% flatten
    parse_decode_cs_loop(T, Depth, lists:reverse(SubFields), lists:reverse(SubCodes));

parse_decode_cs_loop([#meta{name = Name, type = list, comment = Comment, explain = Explain, key = undefined} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    %% recursive
    {SubFields, SubCodes} = parse_decode_cs_loop(Explain, Depth + 1, [], []),
    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", HumpName, "Length = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());", "\n",
        Padding, "var ", HumpName, " = new System.Collections.ArrayList(", HumpName, "Length);", "\n",
        Padding, "while (", HumpName, "Length-- > 0)", "\n",
        Padding, "{", "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "    // add", "\n",
        Padding, "    ", HumpName, ".Add(new System.Collections.Generic.Dictionary<System.String, System.Object>() {", string:join([lists:concat(["{\"", SubName, "\", ", SubName, "}"]) || SubName <- SubFields], ", "), "});", "\n",
        Padding, "}"
    ],
    parse_decode_cs_loop(T, Depth, [HumpName | Fields], [Code | List]);

parse_decode_cs_loop([#meta{name = Name, type = list, comment = Comment, explain = Explain = [#meta{explain = SubExplain}], key = Key} | T], Depth, Fields, List) ->
    Field = lists:keyfind(Key, #meta.name, SubExplain),
    Field == undefined andalso erlang:throw(lists:flatten(io_lib:format("Cound not found field ~ts in explain", [Key]))),
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    %% recursive
    {SubFields, SubCodes} = parse_decode_cs_loop(Explain, Depth + 1, [], []),
    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", HumpName, "Length = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());", "\n",
        Padding, "var ", HumpName, " = new System.Collections.Generic.Dictionary<System.Object, System.Collections.Generic.Dictionary<System.String, System.Object>>(", HumpName, "Length);", "\n",
        Padding, "while (", HumpName, "Length-- > 0)", "\n",
        Padding, "{", "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "    // add", "\n",
        Padding, "    ", HumpName, "[", word:to_lower_hump(Key), "] = new System.Collections.Generic.Dictionary<System.String, System.Object>() {", string:join([lists:concat(["{\"", SubName, "\", ", SubName, "}"]) || SubName <- SubFields], ", "), "};", "\n",
        Padding, "}"
    ],
    parse_decode_cs_loop(T, Depth, [HumpName | Fields], [Code | List]);

parse_decode_cs_loop([#meta{name = Name, type = ets, comment = Comment, explain = Explain, key = undefined} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    %% recursive
    {SubFields, SubCodes} = parse_decode_cs_loop(Explain, Depth + 1, [], []),
    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", HumpName, "Length = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());", "\n",
        Padding, "var ", HumpName, " = new System.Collections.ArrayList(", HumpName, "Length);", "\n",
        Padding, "while (", HumpName, "Length-- > 0)", "\n",
        Padding, "{", "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "    // add", "\n",
        Padding, "    ", HumpName, ".Add(new System.Collections.Generic.Dictionary<System.String, System.Object>() {", string:join([lists:concat(["{\"", SubName, "\", ", SubName, "}"]) || SubName <- SubFields], ", "), "});", "\n",
        Padding, "}"
    ],
    parse_decode_cs_loop(T, Depth, [HumpName | Fields], [Code | List]);

parse_decode_cs_loop([#meta{name = Name, type = ets, comment = Comment, explain = Explain = [#meta{explain = SubExplain}], key = Key} | T], Depth, Fields, List) ->
    Field = lists:keyfind(Key, #meta.name, SubExplain),
    Field == undefined andalso erlang:throw(lists:flatten(io_lib:format("Cound not found field ~ts in explain", [Key]))),
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    %% recursive
    {SubFields, SubCodes} = parse_decode_cs_loop(Explain, Depth + 1, [], []),
    Code = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", HumpName, "Length = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());", "\n",
        Padding, "var ", HumpName, " = new System.Collections.Generic.Dictionary<System.Object, System.Collections.Generic.Dictionary<System.String, System.Object>>(", HumpName, "Length);", "\n",
        Padding, "while (", HumpName, "Length-- > 0)", "\n",
        Padding, "{", "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "    // add", "\n",
        Padding, "    ", HumpName, "[", word:to_lower_hump(Key), "] = new System.Collections.Generic.Dictionary<System.String, System.Object>() {", string:join([lists:concat(["{\"", SubName, "\", ", SubName, "}"]) || SubName <- SubFields], ", "), "};", "\n",
        Padding, "}"
    ],
    parse_decode_cs_loop(T, Depth, [HumpName | Fields], [Code | List]).
