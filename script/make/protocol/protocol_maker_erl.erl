%%%-------------------------------------------------------------------
%%% @doc
%%% make protocol define to erl io code
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_maker_erl).
-export([format_code/3, format_handler/3]).
-export([parse_request_erl/3, parse_response_erl/4]).
-export([parse_encode_erl/2, parse_decode_erl/2]).
-include("../../../include/serialize.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
format_code(ErlName, DecodeList, EncodeList) ->
    %% decode
    ErlDecodeImport = [Import || #data{erl = #set{code = #file{import = Import}}} <- DecodeList],
    ErlDecode = [Code || #data{erl = #set{code = #file{function = Code}}} <- DecodeList],
    ErlDecodeFunction = [Function || #data{erl = #set{code = #file{extra = Function}}} <- DecodeList, Function =/= []],
    ErlDecodeDefault = lists:concat([
        "decode(Protocol, Binary) ->", "\n",
        "    ", "{error, Protocol, Binary}.", "\n"
    ]),
    Decode = lists:append(ErlDecode, [ErlDecodeDefault | ErlDecodeFunction]),

    %% encode
    ErlEncodeImport = [Import || #data{erl = #set{code = #file{import = Import}}} <- EncodeList],
    ErlEncode = [Code || #data{erl = #set{code = #file{function = Code}}} <- EncodeList],
    ErlEncodeFunction = [Function || #data{erl = #set{code = #file{extra = Function}}} <- EncodeList, Function =/= []],
    ErlEncodeDefault = lists:concat([
        "encode(Protocol, Data) ->", "\n"
        "    ", "{error, Protocol, Data}.", "\n"
    ]),
    Encode = lists:append(ErlEncode, [ErlEncodeDefault | ErlEncodeFunction]),

    Includes = string:join(maker:collect_include(lists:flatten([ErlDecodeImport, ErlEncodeImport])), ""),

    lists:concat([
        "-module(", ErlName, ").", "\n",
        "-export([decode/2, encode/2]).", "\n",
        Includes,
        "\n",
        "-spec decode(Protocol :: non_neg_integer(), Binary :: binary()) -> {ok, [integer() | binary() | list()]} | {error, Protocol :: non_neg_integer(), Binary :: binary()}.", "\n",
        string:join(Decode, "\n"), "\n",
        "\n",
        "-spec encode(Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()) -> {ok, binary()} | {error, Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()}.", "\n",
        string:join(Encode, "\n"), "\n"
    ]).

format_handler(ErlName, DecodeList, EncodeList) ->
    %% @todo handler spec

    Name = string:replace(ErlName, "_protocol", "_handler"),

    %% decode
    DecodeImport = listing:unique([Import || #data{erl = #set{handler = #file{import = Import}}} <- DecodeList, Import =/= []]),
    DecodeExport = listing:unique([Export || #data{erl = #set{handler = #file{export = Export}}} <- DecodeList, Export =/= []]),
    DecodeCode = [Decode || #data{erl = #set{handler = #file{function = Decode}}} <- DecodeList, Decode =/= []],

    %% encode
    EncodeImport = listing:unique([Import || #data{erl = #set{handler = #file{import = Import}}} <- EncodeList, Import =/= []]),
    EncodeExport = listing:unique([Export || #data{erl = #set{handler = #file{export = Export}}} <- EncodeList, Export =/= []]),
    EncodeCode = [Encode || #data{erl = #set{handler = #file{function = Encode}}} <- EncodeList, Encode =/= []],

    case lists:any(fun(#data{protocol = Protocol}) -> Protocol == 0 end, DecodeList) of
        true ->
            lists:concat([
                "-module(", Name, ").", "\n",
                DecodeExport,
                EncodeExport,
                DecodeImport,
                EncodeImport,
                "\n",
                DecodeCode,
                EncodeCode
            ]);
        false ->
            lists:concat([
                "-module(", Name, ").", "\n",
                DecodeExport,
                EncodeExport,
                DecodeImport,
                EncodeImport,
                "\n",
                DecodeCode,
                "handle(_, Protocol, Data) ->", "\n",
                "    ", "{error, Protocol, Data}.", "\n",
                "\n",
                EncodeCode
            ])
    end.

%%%===================================================================
%%% request handler
%%%===================================================================
parse_request_erl(0, _, #handler{module = undefined, function = undefined}) ->
    %% default handler code
    #file{};
parse_request_erl(0, _, #handler{module = Module, function = Function, state = State}) ->

    %% default handler code
    StateName = word:to_hump(hd(lists:flatten([State, '_']))),
    Input = string:join([StateName, "Protocol", "Data"], ", "),
    Output = string:join([StateName, "Protocol", "Data"], ", "),

    Export = lists:concat([
        "-export([handle/3]).", "\n"
    ]),

    Code = lists:concat([
        "handle(", Input, ") ->", "\n",
        "    ", Module, ":", Function, "(", Output, ").", "\n",
        "\n"
    ]),

    #file{export = Export, function = Code};

parse_request_erl(Protocol, Meta, #handler{module = Module, function = Function, state = State, protocol = IsContainProtocol}) ->

    ConstructNames = parse_construct_erl_name(Meta),
    DestructNames = parse_destruct_erl_name(Meta),

    StateName = word:to_hump(hd(lists:flatten([State, '_']))),
    Input = string:join([StateName, integer_to_list(Protocol), ConstructNames], ", "),
    ProtocolName = [integer_to_list(Protocol) || IsContainProtocol],
    AllOutput = [A || A <- [StateName, ProtocolName, string:join(DestructNames, ", ")], A =/= []],
    Output = string:join(AllOutput, ", "),

    Export = lists:concat([
        "-export([handle/3]).", "\n"
    ]),

    Code = lists:concat([
        "handle(", Input, ") ->", "\n",
        "    ", Module, ":", Function, "(", Output, ");", "\n",
        "\n"
    ]),

    #file{export = Export, function = Code}.

%%%===================================================================
%%% name resolver
%%%===================================================================

parse_construct_erl_name(#meta{name = _, type = tuple, explain = Explain, comment = _}) ->
    NameList = [word:to_hump(SubName) || #meta{name = SubName} <- tuple_to_list(Explain)],
    lists:concat(["{", string:join(NameList, ", "), "}"]);

%%parse_construct_erl_name(#meta{name = Name, type = record, explain = Explain, comment = _}) ->
%%   NameList = [lists:concat([SubName, " = ", word:to_hump(SubName)]) || #meta{name = SubName} <- tuple_to_list(Explain)],
%%   lists:concat(["#", word:to_snake(Name), "{", string:join(NameList, ", "), "}"]);

%%parse_construct_erl_name(#meta{name = _, type = maps, explain = Explain, comment = _}) ->
%%   NameList = [lists:concat([SubName, " =: ", word:to_hump(SubName)]) || #meta{name = SubName} <- maps:values(Explain)],
%%   lists:concat(["#", "{", string:join(NameList, ", "), "}"]);

parse_construct_erl_name(#meta{name = Name}) ->
    word:to_hump(Name).



parse_destruct_erl_name(#meta{name = _, type = tuple, explain = Explain, comment = _}) ->
    [word:to_hump(SubName) || #meta{name = SubName} <- tuple_to_list(Explain)];

%%parse_destruct_erl_name(#meta{name = _, type = record, explain = Explain, comment = _}) ->
%%   [lists:concat([SubName, " = ", word:to_hump(SubName)]) || #meta{name = SubName} <- tuple_to_list(Explain)];

%%parse_destruct_erl_name(#meta{name = _, type = maps, explain = Explain, comment = _}) ->
%%   [lists:concat([SubName, " =: ", word:to_hump(SubName)]) || #meta{name = SubName} <- maps:values(Explain)];

parse_destruct_erl_name(#meta{name = Name}) ->
    [word:to_hump(Name)].

%%%===================================================================
%%% response creator
%%%===================================================================
parse_response_erl(0, _, #handler{module = undefined, function = undefined}, _) ->
    %% default handler code
    #file{};
parse_response_erl(_, _, #handler{module = undefined, function = undefined}, _) ->
    %% default handler code
    #file{};
parse_response_erl(Protocol, Meta, #handler{module = Module, function = Function, state = State, alias = Alias, protocol = IsContainProtocol, response = send, imp = Imp}, Name) ->

    ConstructNames = parse_construct_erl_name(Meta),
    DestructNames = parse_destruct_erl_name(Meta),

    StateName = word:to_hump(hd(lists:flatten([State, '']))),
    ProtocolName = [integer_to_list(Protocol) || IsContainProtocol],
    AllOutput = [A || A <- lists:append([StateName, ProtocolName], DestructNames), A =/= []],
    Output = string:join(AllOutput, ", "),

    Method = maps:get(Alias, #{true => Module, false => Function}, Alias),

    Export = lists:concat([
        "-export([send_", Method, "/", length(AllOutput), "]).", "\n"
    ]),

    Sender = string:join([word:to_snake(Word) || Word <- lists:flatten([Imp, 'sender'])], "_"),

    Code = lists:concat([
        "send_", Method, "(", Output, ") ->", "\n",
        "    ", "{ok, Binary} = ", Name, ":encode(", Protocol, ", ", ConstructNames, "),", "\n",
        "    ", Sender, ":send(", StateName, ", Binary).", "\n",
        "\n"
    ]),

    #file{export = Export, function = Code};

parse_response_erl(Protocol, Meta, #handler{module = Module, function = Function, state = State, alias = Alias, protocol = IsContainProtocol, response = buffer}, Name) ->

    ConstructNames = parse_construct_erl_name(Meta),
    DestructNames = parse_destruct_erl_name(Meta),

    StateName = word:to_hump(hd(lists:flatten([State, '']))),
    ProtocolName = [integer_to_list(Protocol) || IsContainProtocol],
    AllOutput = [A || A <- lists:append([StateName, ProtocolName], DestructNames), A =/= []],
    Output = string:join(AllOutput, ", "),

    Method = maps:get(Alias, #{true => Module, false => Function}, Alias),

    Export = lists:concat([
        "-export([send_", Method, "/", length(AllOutput), "]).", "\n"
    ]),

    Code = lists:concat([
        "send_", Method, "(", Output, ") ->", "\n",
        "    ", "{ok, Binary} = ", Name, ":encode(", Protocol, ", ", ConstructNames, "),", "\n",
        "    ", StateName, "#", word:to_snake(StateName), "{buffer = <<(", StateName, "#", word:to_snake(StateName), ".buffer)/binary, Binary/binary>>}.", "\n",
        "\n"
    ]),

    %% include
    [Includes] = maker:collect_include([State]),

    #file{export = Export, import = Includes, function = Code}.

%%%===================================================================
%%% decode
%%%===================================================================
%% erl code
parse_decode_erl(Protocol, Meta) ->
    %% start with 3 tabs(4 space) padding
    {_, Records, Functions, Names, Codes} = parse_decode_erl_loop([Meta], Protocol, 1, [], [], "", [], [], [], []),

    %% codes format
    CodesDefault = lists:concat([
        "    ", "{ok, ", string:join(Names, ", "), "};", "\n"
    ]),
    CodesBlock = [Code || Code <- lists:append(Codes, [CodesDefault]), Code =/= []],

    %% format one protocol define
    Code = lists:concat([
        "decode(", Protocol, ", _Rest_ = <<_/binary>>) ->", "\n",
        string:join(CodesBlock, "\n")
    ]),

    #file{import = Records, function = Code, extra = string:join(Functions, "\n")}.

parse_decode_erl_loop([], _, _, _, _, Scope, Records, Functions, Names, List) ->
    %% construct as a list
    {Scope, lists:reverse(Records), lists:reverse(Functions), lists:reverse(Names), lists:reverse(List)};

parse_decode_erl_loop([#meta{name = Name, type = binary, explain = Length} | T], Protocol, Depth, Parent, Ancestor, Scope, Records, Functions, Names, List) ->
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = string:join([word:to_hump(PathName) || PathName <- PathNames], ""),

    Code = lists:concat([
        "    ", "<<", PathHumpName, ":", Length, "/binary, _", PathHumpName, "Rest_/binary>> = _", Scope, "Rest_,"
    ]),

    parse_decode_erl_loop(T, Protocol, Depth, Parent, Ancestor, PathHumpName, Records, Functions, [PathHumpName | Names], [Code | List]);

parse_decode_erl_loop([#meta{name = Name, type = bool, explain = undefined} | T], Protocol, Depth, Parent, Ancestor, Scope, Records, Functions, Names, List) ->
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = string:join([word:to_hump(PathName) || PathName <- PathNames], ""),

    Code = lists:concat([
        "    ", "<<", PathHumpName, "Flag:8, _", PathHumpName, "Rest_/binary>> = _", Scope, "Rest_,", "\n",
        "    ", PathHumpName, " = type:to_boolean(", PathHumpName, "Flag),"
    ]),

    parse_decode_erl_loop(T, Protocol, Depth, Parent, Ancestor, PathHumpName, Records, Functions, [PathHumpName | Names], [Code | List]);

parse_decode_erl_loop([#meta{name = Name, type = u8, explain = undefined} | T], Protocol, Depth, Parent, Ancestor, Scope, Records, Functions, Names, List) ->
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = string:join([word:to_hump(PathName) || PathName <- PathNames], ""),

    Code = lists:concat([
        "    ", "<<", PathHumpName, ":8, _", PathHumpName, "Rest_/binary>> = _", Scope, "Rest_,"
    ]),

    parse_decode_erl_loop(T, Protocol, Depth, Parent, Ancestor, PathHumpName, Records, Functions, [PathHumpName | Names], [Code | List]);

parse_decode_erl_loop([#meta{name = Name, type = u16, explain = undefined} | T], Protocol, Depth, Parent, Ancestor, Scope, Records, Functions, Names, List) ->
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = string:join([word:to_hump(PathName) || PathName <- PathNames], ""),

    Code = lists:concat([
        "    ", "<<", PathHumpName, ":16, _", PathHumpName, "Rest_/binary>> = _", Scope, "Rest_,"
    ]),

    parse_decode_erl_loop(T, Protocol, Depth, Parent, Ancestor, PathHumpName, Records, Functions, [PathHumpName | Names], [Code | List]);

parse_decode_erl_loop([#meta{name = Name, type = u32, explain = undefined} | T], Protocol, Depth, Parent, Ancestor, Scope, Records, Functions, Names, List) ->
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = string:join([word:to_hump(PathName) || PathName <- PathNames], ""),

    Code = lists:concat([
        "    ", "<<", PathHumpName, ":32, _", PathHumpName, "Rest_/binary>> = _", Scope, "Rest_,"
    ]),

    parse_decode_erl_loop(T, Protocol, Depth, Parent, Ancestor, PathHumpName, Records, Functions, [PathHumpName | Names], [Code | List]);

parse_decode_erl_loop([#meta{name = Name, type = u64, explain = undefined} | T], Protocol, Depth, Parent, Ancestor, Scope, Records, Functions, Names, List) ->
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = string:join([word:to_hump(PathName) || PathName <- PathNames], ""),

    Code = lists:concat([
        "    ", "<<", PathHumpName, ":64, _", PathHumpName, "Rest_/binary>> = _", Scope, "Rest_,"
    ]),

    parse_decode_erl_loop(T, Protocol, Depth, Parent, Ancestor, PathHumpName, Records, Functions, [PathHumpName | Names], [Code | List]);

parse_decode_erl_loop([#meta{name = Name, type = i8, explain = undefined} | T], Protocol, Depth, Parent, Ancestor, Scope, Records, Functions, Names, List) ->
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = string:join([word:to_hump(PathName) || PathName <- PathNames], ""),

    Code = lists:concat([
        "    ", "<<", PathHumpName, ":8/signed, _", PathHumpName, "Rest_/binary>> = _", Scope, "Rest_,"
    ]),

    parse_decode_erl_loop(T, Protocol, Depth, Parent, Ancestor, PathHumpName, Records, Functions, [PathHumpName | Names], [Code | List]);

parse_decode_erl_loop([#meta{name = Name, type = i16, explain = undefined} | T], Protocol, Depth, Parent, Ancestor, Scope, Records, Functions, Names, List) ->
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = string:join([word:to_hump(PathName) || PathName <- PathNames], ""),

    Code = lists:concat([
        "    ", "<<", PathHumpName, ":16/signed, _", PathHumpName, "Rest_/binary>> = _", Scope, "Rest_,"
    ]),

    parse_decode_erl_loop(T, Protocol, Depth, Parent, Ancestor, PathHumpName, Records, Functions, [PathHumpName | Names], [Code | List]);

parse_decode_erl_loop([#meta{name = Name, type = i32, explain = undefined} | T], Protocol, Depth, Parent, Ancestor, Scope, Records, Functions, Names, List) ->
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = string:join([word:to_hump(PathName) || PathName <- PathNames], ""),

    Code = lists:concat([
        "    ", "<<", PathHumpName, ":32/signed, _", PathHumpName, "Rest_/binary>> = _", Scope, "Rest_,"
    ]),

    parse_decode_erl_loop(T, Protocol, Depth, Parent, Ancestor, PathHumpName, Records, Functions, [PathHumpName | Names], [Code | List]);

parse_decode_erl_loop([#meta{name = Name, type = i64, explain = undefined} | T], Protocol, Depth, Parent, Ancestor, Scope, Records, Functions, Names, List) ->
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = string:join([word:to_hump(PathName) || PathName <- PathNames], ""),

    Code = lists:concat([
        "    ", "<<", PathHumpName, ":64/signed, _", PathHumpName, "Rest_/binary>> = _", Scope, "Rest_,"
    ]),

    parse_decode_erl_loop(T, Protocol, Depth, Parent, Ancestor, PathHumpName, Records, Functions, [PathHumpName | Names], [Code | List]);

parse_decode_erl_loop([#meta{name = Name, type = f32, explain = undefined} | T], Protocol, Depth, Parent, Ancestor, Scope, Records, Functions, Names, List) ->
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = string:join([word:to_hump(PathName) || PathName <- PathNames], ""),

    Code = lists:concat([
        "    ", "<<", PathHumpName, ":32/float, _", PathHumpName, "Rest_/binary>> = _", Scope, "Rest_,"
    ]),

    parse_decode_erl_loop(T, Protocol, Depth, Parent, Ancestor, PathHumpName, Records, Functions, [PathHumpName | Names], [Code | List]);

parse_decode_erl_loop([#meta{name = Name, type = f64, explain = undefined} | T], Protocol, Depth, Parent, Ancestor, Scope, Records, Functions, Names, List) ->
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = string:join([word:to_hump(PathName) || PathName <- PathNames], ""),

    Code = lists:concat([
        "    ", "<<", PathHumpName, ":64/float, _", PathHumpName, "Rest_/binary>> = _", Scope, "Rest_,"
    ]),

    parse_decode_erl_loop(T, Protocol, Depth, Parent, Ancestor, PathHumpName, Records, Functions, [PathHumpName | Names], [Code | List]);

parse_decode_erl_loop([#meta{name = Name, type = str, explain = undefined} | T], Protocol, Depth, Parent, Ancestor, Scope, Records, Functions, Names, List) ->
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = string:join([word:to_hump(PathName) || PathName <- PathNames], ""),

    Code = lists:concat([
        "    ", "<<", PathHumpName, "BinaryLength:16, ", PathHumpName, "Binary:", PathHumpName, "BinaryLength/binary, _", PathHumpName, "Rest_/binary>> = _", Scope, "Rest_,", "\n",
        "    ", PathHumpName, " = unicode:characters_to_list(", PathHumpName, "Binary),"
    ]),

    parse_decode_erl_loop(T, Protocol, Depth, Parent, Ancestor, PathHumpName, Records, Functions, [PathHumpName | Names], [Code | List]);

parse_decode_erl_loop([#meta{name = Name, type = bst, explain = undefined} | T], Protocol, Depth, Parent, Ancestor, Scope, Records, Functions, Names, List) ->
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = string:join([word:to_hump(PathName) || PathName <- PathNames], ""),

    Code = lists:concat([
        "    ", "<<", PathHumpName, "Length:16, ", PathHumpName, ":", PathHumpName, "Length/binary, _", PathHumpName, "Rest_/binary>> = _", Scope, "Rest_,"
    ]),

    parse_decode_erl_loop(T, Protocol, Depth, Parent, Ancestor, PathHumpName, Records, Functions, [PathHumpName | Names], [Code | List]);

parse_decode_erl_loop([#meta{name = Name, type = ast, explain = undefined} | T], Protocol, Depth, Parent, Ancestor, Scope, Records, Functions, Names, List) ->
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = string:join([word:to_hump(PathName) || PathName <- PathNames], ""),

    Code = lists:concat([
        "    ", "<<", PathHumpName, "Length:16, ", PathHumpName, "Binary:", PathHumpName, "Length/binary, _", PathHumpName, "Rest_/binary>> = _", Scope, "Rest_," "\n",
        "    ", PathHumpName, " = type:to_atom(", PathHumpName, "Binary),"
    ]),

    %% stacked
    parse_decode_erl_loop(T, Protocol, Depth, Parent, Ancestor, PathHumpName, Records, Functions, [PathHumpName | Names], [Code | List]);

parse_decode_erl_loop([#meta{name = Name, type = tuple, explain = Explain} | T], Protocol, Depth, Parent, Ancestor, Scope, Records, Functions, Names, List) ->
    SubExplain = [Meta || Meta = #meta{} <- tuple_to_list(Explain)],

    NewAncestor = lists:append([Name || Parent =/= list], Ancestor),

    %% recursive
    {NextScope, SubRecords, SubFunctions, SubNames, SubCodes} = parse_decode_erl_loop(SubExplain, Protocol, Depth, tuple, NewAncestor, Scope, [], [], [], []),

    SubName = lists:concat([
        "{", string:join(SubNames, ", "), "}"
    ]),

    SubCode = string:join(SubCodes, "\n"),

    NewRecords = lists:append(lists:reverse(SubRecords), Records),
    NewFunctions = lists:append(lists:reverse(SubFunctions), Functions),
    parse_decode_erl_loop(T, Protocol, Depth, Parent, Ancestor, NextScope, NewRecords, NewFunctions, [SubName | Names], [SubCode | List]);

parse_decode_erl_loop([#meta{name = Name, tag = Tag, type = record, explain = Explain} | T], Protocol, Depth, Parent, Ancestor, Scope, Records, Functions, Names, List) ->
    SubExplain = [Meta || Meta = #meta{} <- tuple_to_list(Explain)],

    NewAncestor = lists:append([Name || Parent =/= list], Ancestor),

    %% recursive
    {NextScope, SubRecords, SubFunctions, SubNames, SubCodes} = parse_decode_erl_loop(SubExplain, Protocol, Depth, record, NewAncestor, Scope, [], [], [], []),

    SubName = lists:concat([
        "#", word:to_snake(Tag), "{", string:join([lists:concat([word:to_snake(FieldName), " = ", PathName]) || {#meta{name = FieldName}, PathName} <- lists:zip(SubExplain, SubNames)], ", "), "}"
    ]),

    Record = type:to_atom(word:to_snake(Tag)),

    SubCode = string:join(SubCodes, "\n"),

    NewRecords = lists:append(lists:reverse(SubRecords), [Record | Records]),
    NewFunctions = lists:append(lists:reverse(SubFunctions), Functions),
    parse_decode_erl_loop(T, Protocol, Depth, Parent, Ancestor, NextScope, NewRecords, NewFunctions, [SubName | Names], [SubCode | List]);

parse_decode_erl_loop([#meta{name = Name, type = maps, explain = Explain} | T], Protocol, Depth, Parent, Ancestor, Scope, Records, Functions, Names, List) ->
    SubExplain = maps:values(Explain),

    NewAncestor = lists:append([Name || Parent =/= list], Ancestor),

    %% recursive
    {NextScope, SubRecords, SubFunctions, SubNames, SubCodes} = parse_decode_erl_loop(SubExplain, Protocol, Depth, maps, NewAncestor, Scope, [], [], [], []),

    SubName = lists:concat([
        "#", "{", string:join([lists:concat([word:to_snake(FieldName), " => ", PathName]) || {#meta{name = FieldName}, PathName} <- lists:zip(SubExplain, SubNames)], ", "), "}"
    ]),

    SubCode = string:join(SubCodes, "\n"),

    NewRecords = lists:append(lists:reverse(SubRecords), Records),
    NewFunctions = lists:append(lists:reverse(SubFunctions), Functions),
    parse_decode_erl_loop(T, Protocol, Depth, Parent, Ancestor, NextScope, NewRecords, NewFunctions, [SubName | Names], [SubCode | List]);

parse_decode_erl_loop([#meta{name = Name, type = list, explain = Explain, key = _} | T], Protocol, Depth, Parent, Ancestor, Scope, Records, Functions, Names, List) ->
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = string:join([word:to_hump(PathName) || PathName <- PathNames], ""),
    NewAncestor = lists:append([Name || Parent =/= list], Ancestor),

    %% recursive
    {NextScope, SubRecords, SubFunctions, SubNames, SubCodes} = parse_decode_erl_loop(Explain, Protocol, Depth + 1, list, NewAncestor, "", [], [], [], []),

    PathSnakeName = string:join([word:to_snake(PathName) || PathName <- PathNames], "_"),
    SubFunction = lists:concat([
        "decode_", PathSnakeName, "_", Protocol, "(<<_/binary>>, Size, 0, List) ->", "\n",
        "    ", "{Size, List};", "\n",
        "decode_", PathSnakeName, "_", Protocol, "(_Rest_ = <<_/binary>>, Size, ", "Length, List) ->", "\n",
        string:join(SubCodes, "\n"), "\n",
        "    ", "decode_", PathSnakeName, "_", Protocol, "(_", NextScope, "Rest_, Size + byte_size(_Rest_) - byte_size(_", NextScope, "Rest_), ", "Length - 1, ", "[", string:join(SubNames, ", "), " | List]).", "\n",
        [lists:concat(["\n", Sub]) || Sub <- SubFunctions]
    ]),

    Code = lists:concat([
        "    ", "<<", PathHumpName, "Length:16, _", PathHumpName, "LengthRest_/binary>> = _", Scope, "Rest_,", "\n",
        "    ", "{", PathHumpName, "ByteSize, ", PathHumpName, "} = decode_", PathSnakeName, "_", Protocol, "(_", PathHumpName, "LengthRest_, 0, ", PathHumpName, "Length, [])", ",", "\n",
        "    ", "<<_:", PathHumpName, "ByteSize/binary, _", PathHumpName, "Rest_/binary>> = _", PathHumpName, "LengthRest_,"
    ]),

    NewRecords = lists:append(lists:reverse(SubRecords), Records),
    parse_decode_erl_loop(T, Protocol, Depth, Parent, Ancestor, PathHumpName, NewRecords, [SubFunction | Functions], [PathHumpName | Names], [Code | List]).

%%%===================================================================
%%% encode
%%%===================================================================
%% erl code
parse_encode_erl(Protocol, Meta) ->
    %% start with 3 tabs(4 space) padding
    {Records, Functions, Names, Codes} = parse_encode_erl_loop([Meta], Protocol, 1, [], [], [], [], [], []),

    %% format one protocol define
    Code = lists:concat([
        "encode(", Protocol, ", ", string:join(Names, ", "), ") ->" "\n",
        "    ", "Data", Protocol, " = <<", string:join([Code || Code <- Codes, Code =/= []], ", "), ">>,", "\n",
        "    ", "{ok, <<(byte_size(Data", Protocol, ")):16, ", Protocol, ":16, Data", Protocol, "/binary>>};", "\n"
    ]),

    #file{import = Records, function = Code, extra = string:join(Functions, "\n")}.

parse_encode_erl_loop([], _, _, _, _, Records, Functions, Names, List) ->
    %% construct as a list
    {lists:reverse(Records), lists:reverse(Functions), lists:reverse(Names), lists:reverse(List)};

parse_encode_erl_loop([#meta{name = _, type = zero, explain = undefined} | T], Protocol, Depth, Parent, Ancestor, Records, Functions, Names, List) ->
    HumpName = "_",
    parse_encode_erl_loop(T, Protocol, Depth, Parent, Ancestor, Records, Functions, [HumpName | Names], List);

parse_encode_erl_loop([#meta{name = Name, type = binary, explain = Explain} | T], Protocol, Depth, Parent, Ancestor, Records, Functions, Names, List) ->
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = string:join([word:to_hump(PathName) || PathName <- PathNames], ""),

    Code = io_lib:format("~s:~tp/binary", [PathHumpName, Explain]),
    parse_encode_erl_loop(T, Protocol, Depth, Parent, Ancestor, Records, Functions, [PathHumpName | Names], [Code | List]);

parse_encode_erl_loop([#meta{name = Name, type = bool, explain = undefined} | T], Protocol, Depth, Parent, Ancestor, Records, Functions, Names, List) ->
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = string:join([word:to_hump(PathName) || PathName <- PathNames], ""),

    Code = io_lib:format("(type:to_flag(~s)):8", [PathHumpName]),
    parse_encode_erl_loop(T, Protocol, Depth, Parent, Ancestor, Records, Functions, [PathHumpName | Names], [Code | List]);

parse_encode_erl_loop([#meta{name = Name, type = u8, explain = undefined} | T], Protocol, Depth, Parent, Ancestor, Records, Functions, Names, List) ->
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = string:join([word:to_hump(PathName) || PathName <- PathNames], ""),

    Code = io_lib:format("~s:8", [PathHumpName]),
    parse_encode_erl_loop(T, Protocol, Depth, Parent, Ancestor, Records, Functions, [PathHumpName | Names], [Code | List]);

parse_encode_erl_loop([#meta{name = Name, type = u16, explain = undefined} | T], Protocol, Depth, Parent, Ancestor, Records, Functions, Names, List) ->
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = string:join([word:to_hump(PathName) || PathName <- PathNames], ""),

    Code = io_lib:format("~s:16", [PathHumpName]),
    parse_encode_erl_loop(T, Protocol, Depth, Parent, Ancestor, Records, Functions, [PathHumpName | Names], [Code | List]);

parse_encode_erl_loop([#meta{name = Name, type = u32, explain = undefined} | T], Protocol, Depth, Parent, Ancestor, Records, Functions, Names, List) ->
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = string:join([word:to_hump(PathName) || PathName <- PathNames], ""),

    Code = io_lib:format("~s:32", [PathHumpName]),
    parse_encode_erl_loop(T, Protocol, Depth, Parent, Ancestor, Records, Functions, [PathHumpName | Names], [Code | List]);

parse_encode_erl_loop([#meta{name = Name, type = u64, explain = undefined} | T], Protocol, Depth, Parent, Ancestor, Records, Functions, Names, List) ->
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = string:join([word:to_hump(PathName) || PathName <- PathNames], ""),

    Code = io_lib:format("~s:64", [PathHumpName]),
    parse_encode_erl_loop(T, Protocol, Depth, Parent, Ancestor, Records, Functions, [PathHumpName | Names], [Code | List]);

parse_encode_erl_loop([#meta{name = Name, type = i8, explain = undefined} | T], Protocol, Depth, Parent, Ancestor, Records, Functions, Names, List) ->
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = string:join([word:to_hump(PathName) || PathName <- PathNames], ""),

    Code = io_lib:format("~s:8/signed", [PathHumpName]),
    parse_encode_erl_loop(T, Protocol, Depth, Parent, Ancestor, Records, Functions, [PathHumpName | Names], [Code | List]);

parse_encode_erl_loop([#meta{name = Name, type = i16, explain = undefined} | T], Protocol, Depth, Parent, Ancestor, Records, Functions, Names, List) ->
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = string:join([word:to_hump(PathName) || PathName <- PathNames], ""),

    Code = io_lib:format("~s:16/signed", [PathHumpName]),
    parse_encode_erl_loop(T, Protocol, Depth, Parent, Ancestor, Records, Functions, [PathHumpName | Names], [Code | List]);

parse_encode_erl_loop([#meta{name = Name, type = i32, explain = undefined} | T], Protocol, Depth, Parent, Ancestor, Records, Functions, Names, List) ->
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = string:join([word:to_hump(PathName) || PathName <- PathNames], ""),

    Code = io_lib:format("~s:32/signed", [PathHumpName]),
    parse_encode_erl_loop(T, Protocol, Depth, Parent, Ancestor, Records, Functions, [PathHumpName | Names], [Code | List]);

parse_encode_erl_loop([#meta{name = Name, type = i64, explain = undefined} | T], Protocol, Depth, Parent, Ancestor, Records, Functions, Names, List) ->
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = string:join([word:to_hump(PathName) || PathName <- PathNames], ""),

    Code = io_lib:format("~s:64/signed", [PathHumpName]),
    parse_encode_erl_loop(T, Protocol, Depth, Parent, Ancestor, Records, Functions, [PathHumpName | Names], [Code | List]);

parse_encode_erl_loop([#meta{name = Name, type = f32, explain = undefined} | T], Protocol, Depth, Parent, Ancestor, Records, Functions, Names, List) ->
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = string:join([word:to_hump(PathName) || PathName <- PathNames], ""),

    Code = io_lib:format("~s:32/float", [PathHumpName]),
    parse_encode_erl_loop(T, Protocol, Depth, Parent, Ancestor, Records, Functions, [PathHumpName | Names], [Code | List]);

parse_encode_erl_loop([#meta{name = Name, type = f64, explain = undefined} | T], Protocol, Depth, Parent, Ancestor, Records, Functions, Names, List) ->
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = string:join([word:to_hump(PathName) || PathName <- PathNames], ""),

    Code = io_lib:format("~s:64/float", [PathHumpName]),
    parse_encode_erl_loop(T, Protocol, Depth, Parent, Ancestor, Records, Functions, [PathHumpName | Names], [Code | List]);

parse_encode_erl_loop([#meta{name = Name, type = str, explain = undefined} | T], Protocol, Depth, Parent, Ancestor, Records, Functions, Names, List) ->
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = string:join([word:to_hump(PathName) || PathName <- PathNames], ""),

    Code = io_lib:format("(begin ~sBinary = unicode:characters_to_binary(~s), <<(byte_size(~sBinary)):16, ~sBinary/binary>> end)/binary", [PathHumpName, PathHumpName, PathHumpName, PathHumpName]),
    parse_encode_erl_loop(T, Protocol, Depth, Parent, Ancestor, Records, Functions, [PathHumpName | Names], [Code | List]);

parse_encode_erl_loop([#meta{name = Name, type = bst, explain = undefined} | T], Protocol, Depth, Parent, Ancestor, Records, Functions, Names, List) ->
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = string:join([word:to_hump(PathName) || PathName <- PathNames], ""),

    Code = io_lib:format("(byte_size(~s)):16, (~s)/binary", [PathHumpName, PathHumpName]),
    parse_encode_erl_loop(T, Protocol, Depth, Parent, Ancestor, Records, Functions, [PathHumpName | Names], [Code | List]);

parse_encode_erl_loop([#meta{name = Name, type = ast, explain = undefined} | T], Protocol, Depth, Parent, Ancestor, Records, Functions, Names, List) ->
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = string:join([word:to_hump(PathName) || PathName <- PathNames], ""),

    Code = io_lib:format("(protocol:text(~s))/binary", [PathHumpName]),
    parse_encode_erl_loop(T, Protocol, Depth, Parent, Ancestor, Records, Functions, [PathHumpName | Names], [Code | List]);

parse_encode_erl_loop([#meta{name = Name, type = tuple, explain = Explain} | T], Protocol, Depth, Parent, Ancestor, Records, Functions, Names, List) ->

    NewAncestor = lists:append([Name || Parent =/= list andalso Parent =/= ets], Ancestor),

    %% recursive
    {SubRecords, SubFunctions, SubNames, SubCodes} = parse_encode_erl_loop(tuple_to_list(Explain), Protocol, Depth, tuple, NewAncestor, [], [], [], []),

    SubName = lists:concat([
        "{", string:join(SubNames, ", "), "}"
    ]),

    SubCode = string:join(SubCodes, ", "),

    NewRecords = lists:append(lists:reverse(SubRecords), Records),
    NewFunctions = lists:append(lists:reverse(SubFunctions), Functions),
    parse_encode_erl_loop(T, Protocol, Depth, Parent, Ancestor, NewRecords, NewFunctions, [SubName | Names], [SubCode | List]);

parse_encode_erl_loop([#meta{name = Name, tag = Tag, type = record, explain = Explain} | T], Protocol, Depth, Parent, Ancestor, Records, Functions, Names, List) ->

    SubExplain = [Meta || Meta = #meta{} <- tuple_to_list(Explain)],

    NewAncestor = lists:append([Name || Parent =/= list andalso Parent =/= ets], Ancestor),

    %% recursive
    {SubRecords, SubFunctions, SubNames, SubCodes} = parse_encode_erl_loop(SubExplain, Protocol, Depth, record, NewAncestor, [], [], [], []),

    KeyNameList = [KeyName || #meta{name = KeyName} <- SubExplain],
    SubName = lists:concat([
        "#", word:to_snake(Tag), "{", string:join([lists:concat([word:to_snake(SubKey), " = ", SubName]) || {SubKey, SubName} <- lists:zip(KeyNameList, SubNames)], ", "), "}"
    ]),

    Record = type:to_atom(word:to_snake(Tag)),

    SubCode = string:join(SubCodes, ", "),

    NewRecords = lists:append(lists:reverse(SubRecords), [Record | Records]),
    NewFunctions = lists:append(lists:reverse(SubFunctions), Functions),
    parse_encode_erl_loop(T, Protocol, Depth, Parent, Ancestor, NewRecords, NewFunctions, [SubName | Names], [SubCode | List]);

parse_encode_erl_loop([#meta{name = Name, type = maps, explain = Explain} | T], Protocol, Depth, Parent, Ancestor, Records, Functions, Names, List) ->

    NewAncestor = lists:append([Name || Parent =/= list andalso Parent =/= ets], Ancestor),

    %% recursive
    {SubRecords, SubFunctions, SubNames, SubCodes} = parse_encode_erl_loop(maps:values(Explain), Protocol, Depth, maps, NewAncestor, [], [], [], []),

    KeyNameList = maps:keys(Explain),

    SubName = lists:concat([
        "#", "{", string:join([lists:concat([word:to_snake(SubKey), " := ", SubName]) || {SubKey, SubName} <- lists:zip(KeyNameList, SubNames)], ", "), "}"
    ]),

    SubCode = string:join(SubCodes, ", "),

    NewRecords = lists:append(lists:reverse(SubRecords), Records),
    NewFunctions = lists:append(lists:reverse(SubFunctions), Functions),
    parse_encode_erl_loop(T, Protocol, Depth, Parent, Ancestor, NewRecords, NewFunctions, [SubName | Names], [SubCode | List]);

parse_encode_erl_loop([#meta{name = Name, type = list, explain = Explain, key = _} | T], Protocol, Depth, Parent, Ancestor, Records, Functions, Names, List) ->
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = string:join([word:to_hump(PathName) || PathName <- PathNames], ""),
    NewAncestor = lists:append([Name || Parent =/= list andalso Parent =/= ets], Ancestor),

    %% recursive
    {SubRecords, SubFunctions, SubNames, SubCodes} = parse_encode_erl_loop(Explain, Protocol, Depth + 1, list, NewAncestor, [], [], [], []),

    PathSnakeName = string:join([word:to_snake(PathName) || PathName <- PathNames], "_"),
    SubFunction = lists:concat([
        "encode_", PathSnakeName, "_", Protocol, "(Acc = <<_/binary>>, Length, []) ->", "\n",
        "    ", "<<Length:16, Acc/binary>>;", "\n",
        "encode_", PathSnakeName, "_", Protocol, "(Acc = <<_/binary>>, Length, [", string:join(SubNames, ", "), " | ", PathHumpName, "]) ->", "\n",
        "    ", "encode_", PathSnakeName, "_", Protocol, "(<<Acc/binary, ",  string:join(SubCodes, ", "), ">>, Length + 1, ", PathHumpName, ").", "\n",
        [lists:concat(["\n", Sub]) || Sub <- SubFunctions]
    ]),

    Code = lists:concat([
        "(encode_", PathSnakeName, "_", Protocol, "(<<>>, 0, ", PathHumpName, "))/binary"
    ]),

    NewRecords = lists:append(lists:reverse(SubRecords), Records),
    parse_encode_erl_loop(T, Protocol, Depth, Parent, Ancestor, NewRecords, [SubFunction | Functions], [PathHumpName | Names], [Code | List]);

parse_encode_erl_loop([#meta{name = Name, type = ets, explain = Explain, key = _} | T], Protocol, Depth, Parent, Ancestor, Records, Functions, Names, List) ->
    NewAncestor = [Name | Ancestor],
    PathNames = lists:reverse(lists:sublist(NewAncestor, max(1, length(NewAncestor) - 1))),
    PathHumpName = string:join([word:to_hump(PathName) || PathName <- PathNames], ""),
    NewAncestor = lists:append([Name || Parent =/= list andalso Parent =/= ets], Ancestor),

    %% recursive
    {SubRecords, SubFunctions, SubNames, SubCodes} = parse_encode_erl_loop(Explain, Protocol, Depth + 1, ets, NewAncestor, [], [], [], []),

    PathSnakeName = string:join([word:to_snake(PathName) || PathName <- PathNames], "_"),
    SubFunction = lists:concat([
        "encode_", PathSnakeName, "_", Protocol, "(Acc = <<_/binary>>, Length, Tab, '$end_of_table') ->", "\n",
        "    ", "ets:safe_fixtable(Tab, false),", "\n",
        "    ", "<<Length:16, Acc/binary>>;", "\n",
        "encode_", PathSnakeName, "_", Protocol, "(Acc = <<_/binary>>, Length, Tab, Key) ->", "\n",
        "    ", "case ets:lookup(Tab, Key) of", "\n",
        "    ", "    ", "[] ->", "\n",
        "    ", "    ", "    ", "encode_", PathSnakeName, "_", Protocol, "(Acc, Length, Tab, ets:next(Tab, Key));", "\n",
        "    ", "    ", "[", string:join(SubNames, ", "), "] ->", "\n",
        "    ", "    ", "    ", "encode_", PathSnakeName, "_", Protocol, "(<<Acc/binary, ", string:join(SubCodes, ", "), ">>, Length + 1, Tab, ets:next(Tab, Key))", "\n",
        "    ", "end.", "\n",
        [lists:concat(["\n", Sub]) || Sub <- SubFunctions]
    ]),

    Code = lists:concat([
        "(encode_", PathSnakeName, "_", Protocol, "(<<>>, 0, ets:safe_fixtable(", PathHumpName, ", true) andalso ", PathHumpName, ", ets:first(", PathHumpName, ")))/binary"
    ]),

    NewRecords = lists:append(lists:reverse(SubRecords), Records),
    parse_encode_erl_loop(T, Protocol, Depth, Parent, Ancestor, NewRecords, [SubFunction | Functions], [PathHumpName | Names], [Code | List]).
