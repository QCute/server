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
        "function ", LuaName, ".decode(offset, protocol, bytes)", "\n",
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
    %% start with 1 tabs(4 space) padding
    %% Padding = lists:duplicate(1, "    "),
    Code = parse_meta_lua_loop([Meta], 2, []),
    %% format one protocol define
    %% lists:concat(["        [", Protocol, "] = {\n", MetaData, "\n        }"]).

    #file{extra = string:trim(Code)}.

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

parse_meta_lua_loop([#meta{name = Name, type = tuple, explain = Explain, comment = Comment} | T], Depth, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),

    %% recursive
    SubCodes = parse_meta_lua_loop(tuple_to_list(Explain), Depth + 1, []),

    %% format one field
    Code = lists:flatten(io_lib:format("~s{name = \"~s\", type = \"~s\", comment = \"~ts\", explain = {\n~ts\n~s}}", [Padding, word:to_lower_hump(Name), map, Comment, SubCodes, Padding])),

    parse_meta_lua_loop(T, Depth, [Code | List]);

parse_meta_lua_loop([#meta{name = Name, type = record, explain = Explain, comment = Comment} | T], Depth, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),

    SubExplain = [Meta || Meta = #meta{} <- tuple_to_list(Explain)],

    %% recursive
    SubCodes = parse_meta_lua_loop(SubExplain, Depth + 1, []),

    %% format one field
    Code = lists:flatten(io_lib:format("~s{name = \"~s\", type = \"~s\", comment = \"~ts\", explain = {\n~ts\n~s}}", [Padding, word:to_lower_hump(Name), map, Comment, SubCodes, Padding])),

    parse_meta_lua_loop(T, Depth, [Code | List]);

parse_meta_lua_loop([#meta{name = Name, type = maps, explain = Explain, comment = Comment} | T], Depth, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),

    %% recursive
    SubCodes = parse_meta_lua_loop(maps:values(Explain), Depth + 1, []),

    %% format one field
    Code = lists:flatten(io_lib:format("~s{name = \"~s\", type = \"~s\", comment = \"~ts\", explain = {\n~ts\n~s}}", [Padding, word:to_lower_hump(Name), map, Comment, SubCodes, Padding])),

    parse_meta_lua_loop(T, Depth, [Code | List]);

parse_meta_lua_loop([#meta{name = Name, type = list, explain = Explain, comment = Comment, key = undefined} | T], Depth, List) ->
    %% recursive
    SubCodes = parse_meta_lua_loop(Explain, Depth + 1, []),
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    Code = lists:flatten(io_lib:format("~s{name = \"~s\", type = \"~s\", comment = \"~ts\", explain = {\n~ts\n~s}}", [Padding, word:to_lower_hump(Name), list, Comment, SubCodes, Padding])),
    parse_meta_lua_loop(T, Depth, [Code | List]);

parse_meta_lua_loop([#meta{name = Name, type = list, explain = Explain, comment = Comment, key = Key} | T], Depth, List) ->
    %% recursive
    SubCodes = parse_meta_lua_loop(Explain, Depth + 1, []),
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    Code = lists:flatten(io_lib:format("~s{name = \"~s\", type = \"~s\", comment = \"~ts\", key = \"~ts\", explain = {\n~ts\n~s}}", [Padding, word:to_lower_hump(Name), list, Comment, word:to_lower_hump(Key), SubCodes, Padding])),
    parse_meta_lua_loop(T, Depth, [Code | List]);

parse_meta_lua_loop([#meta{name = Name, type = ets, explain = Explain, comment = Comment, key = undefined} | T], Depth, List) ->
    %% recursive
    SubCodes = parse_meta_lua_loop(Explain, Depth + 1, []),
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    Code = lists:flatten(io_lib:format("~s{name = \"~s\", type = \"~s\", comment = \"~ts\", explain = {\n~ts\n~s}}", [Padding, word:to_lower_hump(Name), list, Comment, SubCodes, Padding])),
    parse_meta_lua_loop(T, Depth, [Code | List]);

parse_meta_lua_loop([#meta{name = Name, type = ets, explain = Explain, comment = Comment, key = Key} | T], Depth, List) ->
    %% recursive
    SubCodes = parse_meta_lua_loop(Explain, Depth + 1, []),
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    Code = lists:flatten(io_lib:format("~s{name = \"~s\", type = \"~s\", comment = \"~ts\", key = \"~ts\", explain = {\n~ts\n~s}}", [Padding, word:to_lower_hump(Name), list, Comment, word:to_lower_hump(Key), SubCodes, Padding])),
    parse_meta_lua_loop(T, Depth, [Code | List]).

%%%===================================================================
%%% encode
%%%===================================================================
%% lua code
parse_encode_lua(Protocol, Meta = #meta{name = Name, type = Type}) ->
    %% start with 3 tabs(4 space) padding
    Padding = lists:duplicate(1, "    "),

    {_, Codes} = parse_encode_lua_loop([Meta], 2, Type, [], atom_to_list(Name), [], []),

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

%% lua code
parse_encode_lua_loop([], _, _, _, _, Fields, List) ->
    %% construct as a list
    {lists:reverse(Fields), lists:reverse(List)};

parse_encode_lua_loop([#meta{name = Name, type = binary, comment = Comment} | T], Depth, Parent, Ancestor, Scope, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),
    LastGetNames = lists:sublist(NewAncestor, 1),
    PathGetName = [lists:concat(["[\"", word:to_lower_hump(PathName), "\"]"]) || PathName <- LastGetNames, Ancestor =/= [] andalso Parent =/= list andalso Parent =/= ets],

    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = ", Scope, PathGetName, "\n",
        Padding, "offset = offset + 1"
    ],
    parse_encode_lua_loop(T, Depth, Parent, Ancestor, Scope, [PathHumpName | Fields], [Code | List]);

parse_encode_lua_loop([#meta{name = Name, type = bool, explain = undefined, comment = Comment} | T], Depth, Parent, Ancestor, Scope, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),
    LastGetNames = lists:sublist(NewAncestor, 1),
    PathGetName = [lists:concat(["[\"", word:to_lower_hump(PathName), "\"]"]) || PathName <- LastGetNames, Ancestor =/= [] andalso Parent =/= list andalso Parent =/= ets],

    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = string.pack(\">I1\", ", Scope, PathGetName, " ~= 0 and 1 or 0)", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_encode_lua_loop(T, Depth, Parent, Ancestor, Scope, [PathHumpName | Fields], [Code | List]);

parse_encode_lua_loop([#meta{name = Name, type = u8, explain = undefined, comment = Comment} | T], Depth, Parent, Ancestor, Scope, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),
    LastGetNames = lists:sublist(NewAncestor, 1),
    PathGetName = [lists:concat(["[\"", word:to_lower_hump(PathName), "\"]"]) || PathName <- LastGetNames, Ancestor =/= [] andalso Parent =/= list andalso Parent =/= ets],

    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = string.pack(\">I1\", ", Scope, PathGetName, ")", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_encode_lua_loop(T, Depth, Parent, Ancestor, Scope, [PathHumpName | Fields], [Code | List]);

parse_encode_lua_loop([#meta{name = Name, type = u16, explain = undefined, comment = Comment} | T], Depth, Parent, Ancestor, Scope, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),
    LastGetNames = lists:sublist(NewAncestor, 1),
    PathGetName = [lists:concat(["[\"", word:to_lower_hump(PathName), "\"]"]) || PathName <- LastGetNames, Ancestor =/= [] andalso Parent =/= list andalso Parent =/= ets],

    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = string.pack(\">I2\", ", Scope, PathGetName, ")", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_encode_lua_loop(T, Depth, Parent, Ancestor, Scope, [PathHumpName | Fields], [Code | List]);

parse_encode_lua_loop([#meta{name = Name, type = u32, explain = undefined, comment = Comment} | T], Depth, Parent, Ancestor, Scope, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),
    LastGetNames = lists:sublist(NewAncestor, 1),
    PathGetName = [lists:concat(["[\"", word:to_lower_hump(PathName), "\"]"]) || PathName <- LastGetNames, Ancestor =/= [] andalso Parent =/= list andalso Parent =/= ets],

    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = string.pack(\">I4\", ", Scope, PathGetName, ")", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_encode_lua_loop(T, Depth, Parent, Ancestor, Scope, [PathHumpName | Fields], [Code | List]);

parse_encode_lua_loop([#meta{name = Name, type = u64, explain = undefined, comment = Comment} | T], Depth, Parent, Ancestor, Scope, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),
    LastGetNames = lists:sublist(NewAncestor, 1),
    PathGetName = [lists:concat(["[\"", word:to_lower_hump(PathName), "\"]"]) || PathName <- LastGetNames, Ancestor =/= [] andalso Parent =/= list andalso Parent =/= ets],

    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = string.pack(\">I8\", ", Scope, PathGetName, ")", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_encode_lua_loop(T, Depth, Parent, Ancestor, Scope, [PathHumpName | Fields], [Code | List]);

parse_encode_lua_loop([#meta{name = Name, type = i8, explain = undefined, comment = Comment} | T], Depth, Parent, Ancestor, Scope, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),
    LastGetNames = lists:sublist(NewAncestor, 1),
    PathGetName = [lists:concat(["[\"", word:to_lower_hump(PathName), "\"]"]) || PathName <- LastGetNames, Ancestor =/= [] andalso Parent =/= list andalso Parent =/= ets],

    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = string.pack(\">i1\", ", Scope, PathGetName, ")", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_encode_lua_loop(T, Depth, Parent, Ancestor, Scope, [PathHumpName | Fields], [Code | List]);

parse_encode_lua_loop([#meta{name = Name, type = i16, explain = undefined, comment = Comment} | T], Depth, Parent, Ancestor, Scope, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),
    LastGetNames = lists:sublist(NewAncestor, 1),
    PathGetName = [lists:concat(["[\"", word:to_lower_hump(PathName), "\"]"]) || PathName <- LastGetNames, Ancestor =/= [] andalso Parent =/= list andalso Parent =/= ets],

    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = string.pack(\">i2\", ", Scope, PathGetName, ")", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_encode_lua_loop(T, Depth, Parent, Ancestor, Scope, [PathHumpName | Fields], [Code | List]);

parse_encode_lua_loop([#meta{name = Name, type = i32, explain = undefined, comment = Comment} | T], Depth, Parent, Ancestor, Scope, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),
    LastGetNames = lists:sublist(NewAncestor, 1),
    PathGetName = [lists:concat(["[\"", word:to_lower_hump(PathName), "\"]"]) || PathName <- LastGetNames, Ancestor =/= [] andalso Parent =/= list andalso Parent =/= ets],

    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = string.pack(\">i4\", ", Scope, PathGetName, ")", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_encode_lua_loop(T, Depth, Parent, Ancestor, Scope, [PathHumpName | Fields], [Code | List]);

parse_encode_lua_loop([#meta{name = Name, type = i64, explain = undefined, comment = Comment} | T], Depth, Parent, Ancestor, Scope, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),
    LastGetNames = lists:sublist(NewAncestor, 1),
    PathGetName = [lists:concat(["[\"", word:to_lower_hump(PathName), "\"]"]) || PathName <- LastGetNames, Ancestor =/= [] andalso Parent =/= list andalso Parent =/= ets],

    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = string.pack(\">i8\", ", Scope, PathGetName, ")", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_encode_lua_loop(T, Depth, Parent, Ancestor, Scope, [PathHumpName | Fields], [Code | List]);

parse_encode_lua_loop([#meta{name = Name, type = f32, explain = undefined, comment = Comment} | T], Depth, Parent, Ancestor, Scope, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),
    LastGetNames = lists:sublist(NewAncestor, 1),
    PathGetName = [lists:concat(["[\"", word:to_lower_hump(PathName), "\"]"]) || PathName <- LastGetNames, Ancestor =/= [] andalso Parent =/= list andalso Parent =/= ets],

    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = string.pack(\">f\", ", Scope, PathGetName, ")", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_encode_lua_loop(T, Depth, Parent, Ancestor, Scope, [PathHumpName | Fields], [Code | List]);

parse_encode_lua_loop([#meta{name = Name, type = f64, explain = undefined, comment = Comment} | T], Depth, Parent, Ancestor, Scope, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),
    LastGetNames = lists:sublist(NewAncestor, 1),
    PathGetName = [lists:concat(["[\"", word:to_lower_hump(PathName), "\"]"]) || PathName <- LastGetNames, Ancestor =/= [] andalso Parent =/= list andalso Parent =/= ets],

    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = string.pack(\">d\", ", Scope, PathGetName, ")", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_encode_lua_loop(T, Depth, Parent, Ancestor, Scope, [PathHumpName | Fields], [Code | List]);

parse_encode_lua_loop([#meta{name = Name, type = str, explain = undefined, comment = Comment} | T], Depth, Parent, Ancestor, Scope, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),
    LastGetNames = lists:sublist(NewAncestor, 1),
    PathGetName = [lists:concat(["[\"", word:to_lower_hump(PathName), "\"]"]) || PathName <- LastGetNames, Ancestor =/= [] andalso Parent =/= list andalso Parent =/= ets],

    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = string.pack(\">s2\", ", Scope, PathGetName, ")", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_encode_lua_loop(T, Depth, Parent, Ancestor, Scope, [PathHumpName | Fields], [Code | List]);

parse_encode_lua_loop([#meta{name = Name, type = bst, explain = undefined, comment = Comment} | T], Depth, Parent, Ancestor, Scope, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),
    LastGetNames = lists:sublist(NewAncestor, 1),
    PathGetName = [lists:concat(["[\"", word:to_lower_hump(PathName), "\"]"]) || PathName <- LastGetNames, Ancestor =/= [] andalso Parent =/= list andalso Parent =/= ets],

    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = string.pack(\">s2\", ", Scope, PathGetName, ")", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_encode_lua_loop(T, Depth, Parent, Ancestor, Scope, [PathHumpName | Fields], [Code | List]);

parse_encode_lua_loop([#meta{name = Name, type = ast, explain = undefined, comment = Comment} | T], Depth, Parent, Ancestor, Scope, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),
    LastGetNames = lists:sublist(NewAncestor, 1),
    PathGetName = [lists:concat(["[\"", word:to_lower_hump(PathName), "\"]"]) || PathName <- LastGetNames, Ancestor =/= [] andalso Parent =/= list andalso Parent =/= ets],

    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = string.pack(\">s2\", ", Scope, PathGetName, ")", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_encode_lua_loop(T, Depth, Parent, Ancestor, Scope, [PathHumpName | Fields], [Code | List]);

parse_encode_lua_loop([#meta{name = Name, type = tuple, explain = Explain} | T], Depth, Parent, Ancestor, Scope, Names, List) ->
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),
    LastGetNames = lists:sublist(NewAncestor, 1),
    PathGetName = [lists:concat(["[\"", word:to_lower_hump(PathName), "\"]"]) || PathName <- LastGetNames],
    SubExplain = tuple_to_list(Explain),

    SubCode = lists:concat([
        Padding, "-- ", "convert", "\n",
        Padding, "local ", PathHumpName, " = ", Scope, PathGetName, ";"
    ]),

    %% Ancestor =/= [] is means not include root 
    %% Parent =/= list andalso Parent =/= ets is means not include list/ets
    NewList = lists:append([SubCode || Ancestor =/= [] andalso Parent =/= list andalso Parent =/= ets], List),

    %% recursive
    {SubNames, SubCodes} = parse_encode_lua_loop(SubExplain, Depth, tuple, NewAncestor, PathHumpName, Names, NewList),

    %% flatten
    parse_encode_lua_loop(T, Depth, Parent, Ancestor, Scope, lists:reverse(SubNames), lists:reverse(SubCodes));

parse_encode_lua_loop([#meta{name = Name, tag = Tag, type = record, explain = Explain} | T], Depth, Parent, Ancestor, Scope, Names, List) ->
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    NewTagAncestor = lists:append([[Name || Ancestor == []], [Tag || Ancestor =/= []], Ancestor]),
    PathNames = lists:reverse(lists:sublist(NewTagAncestor, max(1, length(NewTagAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),
    LastGetNames = lists:sublist([Name | Ancestor], 1),
    PathGetName = [lists:concat(["[\"", word:to_lower_hump(PathName), "\"]"]) || PathName <- LastGetNames],
    SubExplain = [Meta || Meta = #meta{} <- tuple_to_list(Explain)],

    SubCode = lists:concat([
        Padding, "-- ", "convert", "\n",
        Padding, "local ", PathHumpName, " = ", Scope, PathGetName, ";"
    ]),

    %% Ancestor =/= [] is means not include root 
    %% Parent =/= list andalso Parent =/= ets is means not include list/ets
    NewList = lists:append([SubCode || Ancestor =/= [] andalso Parent =/= list andalso Parent =/= ets], List),

    %% recursive
    {SubNames, SubCodes} = parse_encode_lua_loop(SubExplain, Depth, tuple, NewAncestor, PathHumpName, Names, NewList),

    %% flatten
    parse_encode_lua_loop(T, Depth, Parent, Ancestor, Scope, lists:reverse(SubNames), lists:reverse(SubCodes));

parse_encode_lua_loop([#meta{name = Name, type = maps, explain = Explain} | T], Depth, Parent, Ancestor, Scope, Names, List) ->
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),
    LastGetNames = lists:sublist(NewAncestor, 1),
    PathGetName = [lists:concat(["[\"", word:to_lower_hump(PathName), "\"]"]) || PathName <- LastGetNames],
    SubExplain = maps:values(Explain),

    SubCode = lists:concat([
        Padding, "-- ", "convert", "\n",
        Padding, "local ", PathHumpName, " = ", Scope, PathGetName, ";"
    ]),

    %% Ancestor =/= [] is means not include root 
    %% Parent =/= list andalso Parent =/= ets is means not include list/ets
    NewList = lists:append([SubCode || Ancestor =/= [] andalso Parent =/= list andalso Parent =/= ets], List),

    %% recursive
    {SubNames, SubCodes} = parse_encode_lua_loop(SubExplain, Depth, tuple, NewAncestor, PathHumpName, Names, NewList),

    %% flatten
    parse_encode_lua_loop(T, Depth, Parent, Ancestor, Scope, lists:reverse(SubNames), lists:reverse(SubCodes));

parse_encode_lua_loop([#meta{name = Name, type = list, explain = Explain, comment = Comment, key = undefined} | T], Depth, Parent, Ancestor, Scope, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),
    LastGetNames = lists:sublist(NewAncestor, 1),
    PathGetName = [lists:concat(["[\"", word:to_lower_hump(PathName), "\"]"]) || PathName <- LastGetNames],
    NewScope = lists:concat([PathHumpName, "Item"]),

    %% recursive
    {_, SubCodes} = parse_encode_lua_loop(Explain, Depth + 1, list, NewAncestor, NewScope, [], []),

    GetCode = lists:concat([
        Padding, "local ", PathHumpName, " = ", Scope, PathGetName, "", "\n"
    ]),

    SubCode = [GetCode || Ancestor =/= []],

    Code = [
        Padding, "-- ", Comment, "\n",
        SubCode,
        Padding, "table[offset] = string.pack(\">I2\", #", PathHumpName, ")", "\n",
        Padding, "offset = offset + 1", "\n",
        Padding, "for ", PathHumpName, "Index = 1, #", PathHumpName, " do", "\n",
        Padding, "    ", "local ", PathHumpName, "Item = ", PathHumpName, "[", PathHumpName, "Index]", "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "end"
    ],
    parse_encode_lua_loop(T, Depth, Parent, Ancestor, Scope, [PathHumpName | Fields], [Code | List]);

parse_encode_lua_loop([#meta{name = Name, type = list, explain = Explain, comment = Comment, key = _} | T], Depth, Parent, Ancestor, Scope, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),
    LastGetNames = lists:sublist(NewAncestor, 1),
    PathGetName = [lists:concat(["[\"", word:to_lower_hump(PathName), "\"]"]) || PathName <- LastGetNames],
    NewScope = lists:concat([PathHumpName, "Item"]),

    %% recursive
    {_, SubCodes} = parse_encode_lua_loop(Explain, Depth + 1, list, NewAncestor, NewScope, [], []),

    GetCode = lists:concat([
        Padding, "local ", PathHumpName, " = ", Scope, PathGetName, "", "\n"
    ]),

    SubCode = [GetCode || Ancestor =/= []],

    Code = [
        Padding, "-- ", Comment, "\n",
        SubCode,
        Padding, "table[offset] = string.pack(\">I2\", #", PathHumpName, ")", "\n",
        Padding, "offset = offset + 1", "\n",
        Padding, "for _, ", PathHumpName, "Item in pairs(", PathHumpName, ") do", "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "end"
    ],
    parse_encode_lua_loop(T, Depth, Parent, Ancestor, Scope, [PathHumpName | Fields], [Code | List]).

%%%===================================================================
%%% decode
%%%===================================================================
%% lua code
parse_decode_lua(Protocol, Meta) ->
    %% start with 3 tabs(4 space) padding
    Padding = lists:duplicate(1, "    "),

    {Fields, Codes} = parse_decode_lua_loop([Meta], 2, [], [], [], []),

    %% codes format
    CodesDefault = lists:concat([
        Padding, "    ", "return ", string:join(Fields, ", "), "", "\n"
    ]),
    CodesBlock = lists:append(Codes, [CodesDefault]),

    %% format one protocol define
    Code = lists:concat([
        "if protocol == ", Protocol, " then", "\n",
        string:join(CodesBlock, "\n"),
        Padding, "else"
    ]),

    #file{function = Code}.

%% lua code
parse_decode_lua_loop([], _, _, _, Fields, List) ->
    %% construct as a list
    {lists:reverse(Fields), lists:reverse(List)};

parse_decode_lua_loop([#meta{type = zero} | T], Depth, Parent, Ancestor, Fields, List) ->

    parse_decode_lua_loop(T, Depth, Parent, Ancestor, Fields, List);

parse_decode_lua_loop([#meta{name = Name, type = binary, explain = Explain, comment = Comment} | T], Depth, Parent, Ancestor, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),

    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", PathHumpName, " = string.unpack(\"c", integer_to_list(Explain), "\", bytes, offset)", "\n",
        Padding, "offset = offset + ", integer_to_list(Explain)
    ],
    parse_decode_lua_loop(T, Depth, Parent, Ancestor, [PathHumpName | Fields], [Code | List]);

parse_decode_lua_loop([#meta{name = Name, type = bool, explain = undefined, comment = Comment} | T], Depth, Parent, Ancestor, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),

    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", PathHumpName, " = string.unpack(\">I1\", bytes, offset) ~= 0", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_decode_lua_loop(T, Depth, Parent, Ancestor, [PathHumpName | Fields], [Code | List]);

parse_decode_lua_loop([#meta{name = Name, type = u8, explain = undefined, comment = Comment} | T], Depth, Parent, Ancestor, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),

    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", PathHumpName, " = string.unpack(\">I1\", bytes, offset)", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_decode_lua_loop(T, Depth, Parent, Ancestor, [PathHumpName | Fields], [Code | List]);

parse_decode_lua_loop([#meta{name = Name, type = u16, explain = undefined, comment = Comment} | T], Depth, Parent, Ancestor, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),

    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", PathHumpName, " = string.unpack(\">I2\", bytes, offset)", "\n",
        Padding, "offset = offset + 2"
    ],
    parse_decode_lua_loop(T, Depth, Parent, Ancestor, [PathHumpName | Fields], [Code | List]);

parse_decode_lua_loop([#meta{name = Name, type = u32, explain = undefined, comment = Comment} | T], Depth, Parent, Ancestor, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),

    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", PathHumpName, " = string.unpack(\">I4\", bytes, offset)", "\n",
        Padding, "offset = offset + 4"
    ],
    parse_decode_lua_loop(T, Depth, Parent, Ancestor, [PathHumpName | Fields], [Code | List]);

parse_decode_lua_loop([#meta{name = Name, type = u64, explain = undefined, comment = Comment} | T], Depth, Parent, Ancestor, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),

    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", PathHumpName, " = string.unpack(\">I8\", bytes, offset)", "\n",
        Padding, "offset = offset + 8"
    ],
    parse_decode_lua_loop(T, Depth, Parent, Ancestor, [PathHumpName | Fields], [Code | List]);

parse_decode_lua_loop([#meta{name = Name, type = i8, explain = undefined, comment = Comment} | T], Depth, Parent, Ancestor, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),

    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", PathHumpName, " = string.unpack(\">i1\", bytes, offset)", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_decode_lua_loop(T, Depth, Parent, Ancestor, [PathHumpName | Fields], [Code | List]);

parse_decode_lua_loop([#meta{name = Name, type = i16, explain = undefined, comment = Comment} | T], Depth, Parent, Ancestor, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),

    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", PathHumpName, " = string.unpack(\">i2\", bytes, offset)", "\n",
        Padding, "offset = offset + 2"
    ],
    parse_decode_lua_loop(T, Depth, Parent, Ancestor, [PathHumpName | Fields], [Code | List]);

parse_decode_lua_loop([#meta{name = Name, type = i32, explain = undefined, comment = Comment} | T], Depth, Parent, Ancestor, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),

    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", PathHumpName, " = string.unpack(\">i4\", bytes, offset)", "\n",
        Padding, "offset = offset + 4"
    ],
    parse_decode_lua_loop(T, Depth, Parent, Ancestor, [PathHumpName | Fields], [Code | List]);

parse_decode_lua_loop([#meta{name = Name, type = i64, explain = undefined, comment = Comment} | T], Depth, Parent, Ancestor, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),

    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", PathHumpName, " = string.unpack(\">i8\", bytes, offset)", "\n",
        Padding, "offset = offset + 8"
    ],
    parse_decode_lua_loop(T, Depth, Parent, Ancestor, [PathHumpName | Fields], [Code | List]);

parse_decode_lua_loop([#meta{name = Name, type = f32, explain = undefined, comment = Comment} | T], Depth, Parent, Ancestor, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),

    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", PathHumpName, " = string.unpack(\">f\", bytes, offset)", "\n",
        Padding, "offset = offset + 4"
    ],
    parse_decode_lua_loop(T, Depth, Parent, Ancestor, [PathHumpName | Fields], [Code | List]);

parse_decode_lua_loop([#meta{name = Name, type = f64, explain = undefined, comment = Comment} | T], Depth, Parent, Ancestor, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),

    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", PathHumpName, " = string.unpack(\">d\", bytes, offset)", "\n",
        Padding, "offset = offset + 8"
    ],
    parse_decode_lua_loop(T, Depth, Parent, Ancestor, [PathHumpName | Fields], [Code | List]);

parse_decode_lua_loop([#meta{name = Name, type = str, explain = undefined, comment = Comment} | T], Depth, Parent, Ancestor, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),

    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", PathHumpName, " = string.unpack(\">s2\", bytes, offset)", "\n",
        Padding, "offset = offset + 2 + string.len(", PathHumpName, ")"
    ],
    parse_decode_lua_loop(T, Depth, Parent, Ancestor, [PathHumpName | Fields], [Code | List]);

parse_decode_lua_loop([#meta{name = Name, type = bst, explain = undefined, comment = Comment} | T], Depth, Parent, Ancestor, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),

    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", PathHumpName, " = string.unpack(\">s2\", bytes, offset)", "\n",
        Padding, "offset = offset + 2 + string.len(", PathHumpName, ")"
    ],
    parse_decode_lua_loop(T, Depth, Parent, Ancestor, [PathHumpName | Fields], [Code | List]);

parse_decode_lua_loop([#meta{name = Name, type = ast, explain = undefined, comment = Comment} | T], Depth, Parent, Ancestor, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),

    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", PathHumpName, " = string.unpack(\">s2\", bytes, offset)", "\n",
        Padding, "offset = offset + 2 + string.len(", PathHumpName, ")"
    ],
    parse_decode_lua_loop(T, Depth, Parent, Ancestor, [PathHumpName | Fields], [Code | List]);

parse_decode_lua_loop([#meta{name = Name, type = tuple, explain = Explain, comment = Comment} | T], Depth, Parent, Ancestor, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),

        SubExplain = [Meta || Meta = #meta{type = SubType} <- tuple_to_list(Explain), SubType =/= zero],
    NewNextAncestor = lists:append([Name || Parent =/= list andalso Parent =/= ets], Ancestor),

    %% recursive
    {SubFields, SubCodes} = parse_decode_lua_loop(SubExplain, Depth, tuple, NewNextAncestor, [], []),

    Code = [
        Padding, "-- ", Comment, "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "-- object", "\n",
        Padding, "local ", PathHumpName, " = {", string:join([lists:concat([word:to_lower_hump(FieldName), " = ", PathName]) || {#meta{name = FieldName}, PathName} <- lists:zip(SubExplain, SubFields)], ", "), "}"
    ],

    %% flatten
    parse_decode_lua_loop(T, Depth, Parent, Ancestor, [PathHumpName | Fields], [Code | List]);

parse_decode_lua_loop([#meta{name = Name, tag = Tag, type = record, explain = Explain, comment = Comment} | T], Depth, Parent, Ancestor, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Tag | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),

    SubExplain = [Meta || Meta = #meta{} <- tuple_to_list(Explain)],
    NewNextAncestor = lists:append([Name || Parent =/= list andalso Parent =/= ets], Ancestor),

    %% recursive
    {SubFields, SubCodes} = parse_decode_lua_loop(SubExplain, Depth, record, NewNextAncestor, [], []),

    Code = [
        Padding, "-- ", Comment, "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "-- object", "\n",
        Padding, "local ", PathHumpName, " = {", string:join([lists:concat([word:to_lower_hump(FieldName), " = ", PathName]) || {#meta{name = FieldName}, PathName} <- lists:zip(SubExplain, SubFields)], ", "), "}"
    ],

    %% flatten
    parse_decode_lua_loop(T, Depth, Parent, Ancestor, [PathHumpName | Fields], [Code | List]);

parse_decode_lua_loop([#meta{name = Name, type = maps, explain = Explain, comment = Comment} | T], Depth, Parent, Ancestor, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),

    SubExplain = maps:values(Explain),
    NewNextAncestor = lists:append([Name || Parent =/= list andalso Parent =/= ets], Ancestor),

    %% recursive
    {SubFields, SubCodes} = parse_decode_lua_loop(SubExplain, Depth, maps, NewNextAncestor, [], []),

    Code = [
        Padding, "-- ", Comment, "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "-- object", "\n",
        Padding, "local ", PathHumpName, " = {", string:join([lists:concat([word:to_lower_hump(FieldName), " = ", PathName]) || {#meta{name = FieldName}, PathName} <- lists:zip(SubExplain, SubFields)], ", "), "}"
    ],

    %% flatten
    parse_decode_lua_loop(T, Depth, Parent, Ancestor, [PathHumpName | Fields], [Code | List]);

parse_decode_lua_loop([#meta{name = Name, type = list, explain = Explain, comment = Comment, key = undefined} | T], Depth, Parent, Ancestor, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),
    NewNextAncestor = lists:append([Name || Parent =/= list andalso Parent =/= ets], Ancestor),

    %% recursive
    {SubFields, SubCodes} = parse_decode_lua_loop(Explain, Depth + 1, list, NewNextAncestor, [], []),
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
    parse_decode_lua_loop(T, Depth, Parent, Ancestor, [PathHumpName | Fields], [Code | List]);

parse_decode_lua_loop([#meta{name = Name, type = list, explain = Explain, comment = Comment, key = Key} | T], Depth, Parent, Ancestor, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),
    NewNextAncestor = lists:append([Name || Parent =/= list andalso Parent =/= ets], Ancestor),

    %% recursive
    {SubFields, SubCodes} = parse_decode_lua_loop(Explain, Depth + 1, list, NewNextAncestor, [], []),
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", PathHumpName, " = {}", "\n",
        Padding, "local ", PathHumpName, "Length = string.unpack(\">I2\", bytes, offset)", "\n",
        Padding, "offset = offset + 2", "\n",
        Padding, "for ", PathHumpName, "Index = 1, ", PathHumpName, "Length do", "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "    ", PathHumpName, "[", PathHumpName, word:to_hump(Key), "] = ", string:join(SubFields, ", "), "\n",
        Padding, "end"
    ],
    parse_decode_lua_loop(T, Depth, Parent, Ancestor, [PathHumpName | Fields], [Code | List]);

parse_decode_lua_loop([#meta{name = Name, type = ets, explain = Explain, comment = Comment, key = []} | T], Depth, Parent, Ancestor, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),
    NewNextAncestor = lists:append([Name || Parent =/= list andalso Parent =/= ets], Ancestor),

    %% recursive
    {SubFields, SubCodes} = parse_decode_lua_loop(Explain, Depth + 1, ets, NewNextAncestor, [], []),
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
    parse_decode_lua_loop(T, Depth, Parent, Ancestor, [PathHumpName | Fields], [Code | List]);

parse_decode_lua_loop([#meta{name = Name, type = ets, explain = Explain, comment = Comment, key = [Key]} | T], Depth, Parent, Ancestor, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = word:to_lower_hump(string:join([word:to_hump(PathName) || PathName <- PathNames], "")),
    NewNextAncestor = lists:append([Name || Parent =/= list andalso Parent =/= ets], Ancestor),

    %% recursive
    {SubFields, SubCodes} = parse_decode_lua_loop(Explain, Depth + 1, ets, NewNextAncestor, [], []),
    Code = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", PathHumpName, " = {}", "\n",
        Padding, "local ", PathHumpName, "Length = string.unpack(\">I2\", bytes, offset)", "\n",
        Padding, "offset = offset + 2", "\n",
        Padding, "for ", PathHumpName, "Index = 1, ", PathHumpName, "Length do", "\n",
        string:join(SubCodes, "\n"), "\n",
        Padding, "    ", PathHumpName, "[", PathHumpName, word:to_hump(Key), "] = ", string:join(SubFields, ", "), "\n",
        Padding, "end"
    ],
    parse_decode_lua_loop(T, Depth, Parent, Ancestor, [PathHumpName | Fields], [Code | List]).
