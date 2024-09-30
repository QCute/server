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
    case State of
        [] ->
            StateInput = "_",
            StateOutput = "";
        _ ->
            StateInput = word:to_hump(State),
            StateOutput = word:to_hump(State)
    end,

    AllInput = [P || P <- [StateInput, "Protocol", "Data"], P =/= []],
    Input = string:join(AllInput, ", "),
    AllOutput = [A || A <- [StateOutput, "Protocol", "Data"], A =/= []],
    Output = string:join(AllOutput, ", "),

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

    Names = parse_request_erl_name(Meta),

    case State of
        [] ->
            StateInput = "_",
            StateOutput = "";
        _ ->
            StateInput = word:to_hump(State),
            StateOutput = word:to_hump(State)
    end,

    case IsContainProtocol of
        true ->
            ProtocolInput = lists:concat([Protocol]),
            ProtocolOutput = lists:concat([Protocol]);
        false ->
            ProtocolInput = lists:concat([Protocol]),
            ProtocolOutput = ""
    end,

    AllInput = [P || P <- [StateInput, ProtocolInput, Names], P =/= []],
    Input = string:join(AllInput, ", "),
    AllOutput = [A || A <- [StateOutput, ProtocolOutput, Names], A =/= []],
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


%%parse_request_erl_name(#meta{name = _, type = tuple, explain = Explain, comment = _}) ->
%%    NameList = [parse_request_erl_name(Item) || Item <- tuple_to_list(Explain)],
%%    lists:concat(["{", string:join(NameList, ", "), "}"]);
%%
%%parse_request_erl_name(#meta{name = Name, type = record, explain = Explain, comment = _}) ->
%%    NameList = [lists:concat([SubName, " = ", parse_request_erl_name(Item)]) || Item = #meta{name = SubName} <- tuple_to_list(Explain)],
%%    lists:concat(["#", word:to_snake(Name), "{", string:join(NameList, ", "), "}"]);
%%
%%parse_request_erl_name(#meta{name = _, type = maps, explain = Explain, comment = _}) ->
%%    NameList = [lists:concat([SubName, " =: ", parse_request_erl_name(Item)]) || Item = #meta{name = SubName} <- maps:values(Explain)],
%%    lists:concat(["#", "{", string:join(NameList, ", "), "}"]);

parse_request_erl_name(#meta{name = Name}) ->
    NewName = maps:get(Name == [], #{true => "data", false => Name}),
    word:to_hump(NewName).

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

    Names = parse_response_erl_name(Meta),

    case State of
        [] ->
            StateOutput = "";
        _ ->
            StateOutput = word:to_hump(State)
    end,

    case IsContainProtocol of
        true ->
            ProtocolOutput = lists:concat([Protocol]);
        false ->
            ProtocolOutput = ""
    end,

    case Alias of
        true ->
            Method = Module;
        false ->
            Method = Function;
        _ ->
            Method = Alias
    end,

    case Imp of
        "" ->
            Sender = "sender";
        _ ->
            Sender = lists:concat([word:to_snake(Imp), "_", "sender"])
    end,

    AllOutput = [A || A <- [StateOutput, ProtocolOutput, Names], A =/= []],
    Output = string:join(AllOutput, ", "),

    Export = lists:concat([
        "-export([send_", Method, "/", length(AllOutput), "]).", "\n"
    ]),

    Code = lists:concat([
        "send_", Method, "(", Output, ") ->", "\n",
        "    ", "{ok, Binary} = ", Name, ":encode(", Protocol, ", ", Names, "),", "\n",
        "    ", Sender, ":send(", StateOutput, ", Binary).", "\n",
        "\n"
    ]),

    #file{export = Export, function = Code};

parse_response_erl(Protocol, Meta, #handler{module = Module, function = Function, state = State, alias = Alias, protocol = IsContainProtocol, response = buffer}, Name) ->

    Names = parse_response_erl_name(Meta),

    case State of
        [] ->
            StateOutput = "";
        _ ->
            StateOutput = word:to_hump(State)
    end,

    case IsContainProtocol of
        true ->
            ProtocolOutput = lists:concat([Protocol]);
        false ->
            ProtocolOutput = ""
    end,

    AllOutput = [A || A <- [StateOutput, ProtocolOutput, Names], A =/= []],
    Output = string:join(AllOutput, ", "),

    case Alias of
        true ->
            Method = Module;
        false ->
            Method = Function;
        _ ->
            Method = Alias
    end,

    Export = lists:concat([
        "-export([send_", Method, "/", length(AllOutput), "]).", "\n"
    ]),

    Code = lists:concat([
        "send_", Method, "(", Output, ") ->", "\n",
        "    ", "{ok, Binary} = ", Name, ":encode(", Protocol, ", ", Names, "),", "\n",
        "    ", StateOutput, "#", word:to_snake(StateOutput), "{buffer = <<(", StateOutput, "#", word:to_snake(StateOutput), ".buffer)/binary, Binary/binary>>}.", "\n",
        "\n"
    ]),

    %% include
    [Includes] = maker:collect_include([State]),

    #file{export = Export, import = Includes, function = Code}.


%%parse_response_erl_name(#meta{name = Name, type = tuple, comment = _}) ->
%%    word:to_hump(Name);
%%
%%parse_response_erl_name(#meta{name = Name, type = record, comment = _}) ->
%%    word:to_hump(Name);
%%
%%parse_response_erl_name(#meta{name = Name, type = maps, comment = _}) ->
%%    word:to_hump(Name);

parse_response_erl_name(#meta{name = Name}) ->
    NewName = maps:get(Name == [], #{true => "data", false => Name}),
    word:to_hump(NewName).

%%%===================================================================
%%% decode
%%%===================================================================
%% erl code
parse_decode_erl(Protocol, Meta = #meta{name = Name, type = Type}) ->
    NewName = maps:get(Name == [] andalso ?IS_UNIT(Type), #{true => "data", false => Name}),
    %% start with 3 tabs(4 space) padding
    {_, Records, Functions, Names, Codes} = parse_decode_erl_loop([Meta#meta{name = NewName}], Protocol, 1, "", [], [], [], []),

    %% codes format
    CodesDefault = lists:concat([
        "    ", "{ok, ", string:join(Names, ", "), "};", "\n"
    ]),
    CodesBlock = lists:append(Codes, [CodesDefault]),

    %% format one protocol define
    Code = lists:concat([
        "decode(", Protocol, ", _Rest_ = <<_/binary>>) ->", "\n",
        string:join(CodesBlock, "\n")
    ]),

    #file{import = Records, function = Code, extra = string:join(Functions, "\n")}.

parse_decode_erl_loop([], _, _, ScopeArgs, Records, Functions, Names, List) ->
    %% construct as a list
    %% {string:join(lists:reverse(Functions), "\n"), string:join(lists:reverse(Names), ", "), string:join(lists:reverse(List), "\n")};
    {ScopeArgs, lists:reverse(Records), lists:reverse(Functions), lists:reverse(Names), lists:reverse(List)};

parse_decode_erl_loop([#meta{name = Name, type = binary, explain = Length} | T], Protocol, Depth, ScopeArgs, Records, Functions, Names, List) ->
    HumpName = word:to_hump(Name),
    Code = lists:concat([
        "    ", "<<", HumpName, ":", Length, "/binary, _", HumpName, "Rest_/binary>> = _", ScopeArgs, "Rest_,"
    ]),
    parse_decode_erl_loop(T, Protocol, Depth, HumpName, Records, Functions, [HumpName | Names], [Code | List]);

parse_decode_erl_loop([#meta{name = Name, type = bool, explain = undefined} | T], Protocol, Depth, ScopeArgs, Records, Functions, Names, List) ->
    HumpName = word:to_hump(Name),
    Code = lists:concat([
        "    ", "<<", HumpName, "Flag:8, _", HumpName, "Rest_/binary>> = _", ScopeArgs, "Rest_,", "\n",
        "    ", HumpName, " = type:to_boolean(", HumpName, "Flag),"
    ]),

    parse_decode_erl_loop(T, Protocol, Depth, HumpName, Records, Functions, [HumpName | Names], [Code | List]);

parse_decode_erl_loop([#meta{name = Name, type = u8, explain = undefined} | T], Protocol, Depth, ScopeArgs, Records, Functions, Names, List) ->
    HumpName = word:to_hump(Name),
    Code = lists:concat([
        "    ", "<<", HumpName, ":8, _", HumpName, "Rest_/binary>> = _", ScopeArgs, "Rest_,"
    ]),

    parse_decode_erl_loop(T, Protocol, Depth, HumpName, Records, Functions, [HumpName | Names], [Code | List]);

parse_decode_erl_loop([#meta{name = Name, type = u16, explain = undefined} | T], Protocol, Depth, ScopeArgs, Records, Functions, Names, List) ->
    HumpName = word:to_hump(Name),
    Code = lists:concat([
        "    ", "<<", HumpName, ":16, _", HumpName, "Rest_/binary>> = _", ScopeArgs, "Rest_,"
    ]),

    parse_decode_erl_loop(T, Protocol, Depth, HumpName, Records, Functions, [HumpName | Names], [Code | List]);

parse_decode_erl_loop([#meta{name = Name, type = u32, explain = undefined} | T], Protocol, Depth, ScopeArgs, Records, Functions, Names, List) ->
    HumpName = word:to_hump(Name),
    Code = lists:concat([
        "    ", "<<", HumpName, ":32, _", HumpName, "Rest_/binary>> = _", ScopeArgs, "Rest_,"
    ]),

    parse_decode_erl_loop(T, Protocol, Depth, HumpName, Records, Functions, [HumpName | Names], [Code | List]);

parse_decode_erl_loop([#meta{name = Name, type = u64, explain = undefined} | T], Protocol, Depth, ScopeArgs, Records, Functions, Names, List) ->
    HumpName = word:to_hump(Name),
    Code = lists:concat([
        "    ", "<<", HumpName, ":64, _", HumpName, "Rest_/binary>> = _", ScopeArgs, "Rest_,"
    ]),

    parse_decode_erl_loop(T, Protocol, Depth, HumpName, Records, Functions, [HumpName | Names], [Code | List]);

parse_decode_erl_loop([#meta{name = Name, type = i8, explain = undefined} | T], Protocol, Depth, ScopeArgs, Records, Functions, Names, List) ->
    HumpName = word:to_hump(Name),
    Code = lists:concat([
        "    ", "<<", HumpName, ":8/signed, _", HumpName, "Rest_/binary>> = _", ScopeArgs, "Rest_,"
    ]),

    parse_decode_erl_loop(T, Protocol, Depth, HumpName, Records, Functions, [HumpName | Names], [Code | List]);

parse_decode_erl_loop([#meta{name = Name, type = i16, explain = undefined} | T], Protocol, Depth, ScopeArgs, Records, Functions, Names, List) ->
    HumpName = word:to_hump(Name),
    Code = lists:concat([
        "    ", "<<", HumpName, ":16/signed, _", HumpName, "Rest_/binary>> = _", ScopeArgs, "Rest_,"
    ]),

    parse_decode_erl_loop(T, Protocol, Depth, HumpName, Records, Functions, [HumpName | Names], [Code | List]);

parse_decode_erl_loop([#meta{name = Name, type = i32, explain = undefined} | T], Protocol, Depth, ScopeArgs, Records, Functions, Names, List) ->
    HumpName = word:to_hump(Name),
    Code = lists:concat([
        "    ", "<<", HumpName, ":32/signed, _", HumpName, "Rest_/binary>> = _", ScopeArgs, "Rest_,"
    ]),

    parse_decode_erl_loop(T, Protocol, Depth, HumpName, Records, Functions, [HumpName | Names], [Code | List]);

parse_decode_erl_loop([#meta{name = Name, type = i64, explain = undefined} | T], Protocol, Depth, ScopeArgs, Records, Functions, Names, List) ->
    HumpName = word:to_hump(Name),
    Code = lists:concat([
        "    ", "<<", HumpName, ":64/signed, _", HumpName, "Rest_/binary>> = _", ScopeArgs, "Rest_,"
    ]),

    parse_decode_erl_loop(T, Protocol, Depth, HumpName, Records, Functions, [HumpName | Names], [Code | List]);

parse_decode_erl_loop([#meta{name = Name, type = f32, explain = undefined} | T], Protocol, Depth, ScopeArgs, Records, Functions, Names, List) ->
    HumpName = word:to_hump(Name),
    Code = lists:concat([
        "    ", "<<", HumpName, ":32/float, _", HumpName, "Rest_/binary>> = _", ScopeArgs, "Rest_,"
    ]),

    parse_decode_erl_loop(T, Protocol, Depth, HumpName, Records, Functions, [HumpName | Names], [Code | List]);

parse_decode_erl_loop([#meta{name = Name, type = f64, explain = undefined} | T], Protocol, Depth, ScopeArgs, Records, Functions, Names, List) ->
    HumpName = word:to_hump(Name),
    Code = lists:concat([
        "    ", "<<", HumpName, ":64/float, _", HumpName, "Rest_/binary>> = _", ScopeArgs, "Rest_,"
    ]),

    parse_decode_erl_loop(T, Protocol, Depth, HumpName, Records, Functions, [HumpName | Names], [Code | List]);

parse_decode_erl_loop([#meta{name = Name, type = str, explain = undefined} | T], Protocol, Depth, ScopeArgs, Records, Functions, Names, List) ->
    HumpName = word:to_hump(Name),
    Code = lists:concat([
        "    ", "<<", HumpName, "BinaryLength:16, ", HumpName, "Binary:", HumpName, "BinaryLength/binary, _", HumpName, "Rest_/binary>> = _", ScopeArgs, "Rest_,", "\n",
        "    ", HumpName, " = unicode:characters_to_list(", HumpName, "Binary),"
    ]),

    parse_decode_erl_loop(T, Protocol, Depth, HumpName, Records, Functions, [HumpName | Names], [Code | List]);

parse_decode_erl_loop([#meta{name = Name, type = bst, explain = undefined} | T], Protocol, Depth, ScopeArgs, Records, Functions, Names, List) ->
    HumpName = word:to_hump(Name),
    Code = lists:concat([
        "    ", "<<", HumpName, "Length:16, ", HumpName, ":", HumpName, "Length/binary, _", HumpName, "Rest_/binary>> = _", ScopeArgs, "Rest_,"
    ]),

    parse_decode_erl_loop(T, Protocol, Depth, HumpName, Records, Functions, [HumpName | Names], [Code | List]);

parse_decode_erl_loop([#meta{name = Name, type = rst, explain = undefined} | T], Protocol, Depth, ScopeArgs, Records, Functions, Names, List) ->
    HumpName = word:to_hump(Name),
    Code = lists:concat([
        "    ", "<<", HumpName, "Length:16, ", HumpName, ":", HumpName, "Length/binary, _", HumpName, "Rest_/binary>> = _", ScopeArgs, "Rest_,"
    ]),

    %% stacked
    parse_decode_erl_loop(T, Protocol, Depth, HumpName, Records, Functions, [HumpName | Names], [Code | List]);

parse_decode_erl_loop([#meta{name = _, type = tuple, explain = Explain} | T], Protocol, Depth, ScopeArgs, Records, Functions, Names, List) ->
    %% recursive
    {NextScopeArgs, SubRecords, SubFunctions, SubNames, SubCodes} = parse_decode_erl_loop(tuple_to_list(Explain), Protocol, Depth, ScopeArgs, [], [], [], []),

    SubName = lists:concat([
        "{", string:join(SubNames, ", "), "}"
    ]),

    SubCode = string:join(SubCodes, "\n"),

    %% stacked
    parse_decode_erl_loop(T, Protocol, Depth, NextScopeArgs, lists:append(SubRecords, Records), lists:append(SubFunctions, Functions), [SubName | Names], [SubCode | List]);

parse_decode_erl_loop([#meta{name = Name, type = record, explain = Explain} | T], Protocol, Depth, ScopeArgs, Records, Functions, Names, List) ->
    SubExplain = [Meta || Meta = #meta{} <- tuple_to_list(Explain)],

    %% recursive
    {NextScopeArgs, SubRecords, SubFunctions, SubNames, SubCodes} = parse_decode_erl_loop(SubExplain, Protocol, Depth, ScopeArgs, [], [], [], []),

    SubName = lists:concat([
        "#", word:to_snake(Name), "{", string:join([lists:concat([word:to_snake(FieldName), " = ", FieldName]) || FieldName <- SubNames], ", "), "}"
    ]),

    Record = type:to_atom(word:to_snake(Name)),
    
    SubCode = string:join(SubCodes, "\n"),

    parse_decode_erl_loop(T, Protocol, Depth, NextScopeArgs, lists:append(SubRecords, [Record | Records]), lists:append(SubFunctions, Functions), [SubName | Names], [SubCode | List]);

parse_decode_erl_loop([#meta{name = _, type = maps, explain = Explain} | T], Protocol, Depth, ScopeArgs, Records, Functions, Names, List) ->
    SubExplain = maps:values(Explain),

    %% recursive
    {NextScopeArgs, SubRecords, SubFunctions, SubNames, SubCodes} = parse_decode_erl_loop(SubExplain, Protocol, Depth, ScopeArgs, [], [], [], []),

    SubName = lists:concat([
        "#", "{", string:join([lists:concat([word:to_snake(FieldName), " => ", FieldName]) || FieldName <- SubNames], ", "), "}"
    ]),

    SubCode = string:join(SubCodes, "\n"),

    parse_decode_erl_loop(T, Protocol, Depth, NextScopeArgs, lists:append(SubRecords, Records), lists:append(SubFunctions, Functions), [SubName | Names], [SubCode | List]);

parse_decode_erl_loop([#meta{name = Name, type = list, explain = Explain, key = _} | T], Protocol, Depth, ScopeArgs, Records, Functions, Names, List) ->
    HumpName = word:to_hump(Name),

    SubExplain = [Meta#meta{name = maps:get(SubName, #{[] => lists:concat([HumpName, "Item"])}, SubName)} || Meta = #meta{name = SubName} <- Explain],

    %% recursive
    {NextScopeArgs, SubRecords, SubFunctions, SubNames, SubCodes} = parse_decode_erl_loop(SubExplain, Protocol, Depth + 1, "", [], [], [], []),

    SubFunction = lists:concat([
        "decode_", word:to_snake(HumpName), "_", Protocol, "(<<_/binary>>, Size, 0, List) ->", "\n",
        "    ", "{Size, List};", "\n",
        "decode_", word:to_snake(HumpName), "_", Protocol, "(_Rest_ = <<_/binary>>, Size, ", HumpName, "Length, List) ->", "\n",
        string:join(SubCodes, "\n"), "\n",
        "    ", "decode_", word:to_snake(HumpName), "_", Protocol, "(_", NextScopeArgs, "Rest_, Size + byte_size(_Rest_) - byte_size(_", NextScopeArgs, "Rest_), ", HumpName, "Length - 1, ", "[", string:join(SubNames, ", "), " | List]).", "\n",
        [lists:concat(["\n", Sub]) || Sub <- SubFunctions]
    ]),

    Code = lists:concat([
        "    ", "<<", HumpName, "Length:16, _", HumpName, "LengthRest_/binary>> = _", ScopeArgs, "Rest_,", "\n",
        "    ", "{", HumpName, "ByteSize, ", HumpName, "} = decode_", word:to_snake(HumpName), "_", Protocol, "(_", HumpName, "LengthRest_, 0, ", HumpName, "Length, [])", ",", "\n",
        "    ", "<<_:", HumpName, "ByteSize/binary, _", HumpName, "Rest_/binary>> = _", HumpName, "LengthRest_,"
    ]),

    parse_decode_erl_loop(T, Protocol, Depth, HumpName, lists:append(SubRecords, Records), [SubFunction | Functions], [HumpName | Names], [Code | List]).

%%%===================================================================
%%% encode
%%%===================================================================
%% erl code
parse_encode_erl(Protocol, Meta = #meta{name = Name, type = Type}) ->
    NewName = maps:get(Name == [] andalso ?IS_UNIT(Type), #{true => "data", false => Name}),
    %% start with 3 tabs(4 space) padding
    {Records, Functions, Names, Codes} = parse_encode_erl_loop([Meta#meta{name = NewName}], Protocol, 1, [], [], [], []),

    %% format one protocol define
    Code = lists:concat([
        "encode(", Protocol, ", ", string:join(Names, ", "), ") ->" "\n",
        "    ", "Data", Protocol, " = <<", string:join(Codes, ", "), ">>,", "\n",
        "    ", "{ok, <<(byte_size(Data", Protocol, ")):16, ", Protocol, ":16, Data", Protocol, "/binary>>};", "\n"
    ]),

    #file{import = Records, function = Code, extra = string:join(Functions, "\n")}.

parse_encode_erl_loop([], _, _, Records, Functions, Names, List) ->
    %% construct as a list
    %% {string:join(lists:reverse(Functions), "\n"), string:join(lists:reverse(Names), ", "), string:join(lists:reverse(List), ", ")};
    {lists:reverse(Records), lists:reverse(Functions), lists:reverse(Names), lists:reverse(List)};

parse_encode_erl_loop([#meta{name = _, type = zero, explain = undefined} | T], Protocol, Depth, Records, Functions, Names, List) ->
    HumpName = "_",
    parse_encode_erl_loop(T, Protocol, Depth, Records, Functions, [HumpName | Names], List);

parse_encode_erl_loop([#meta{name = Name, type = binary, explain = Explain} | T], Protocol, Depth, Records, Functions, Names, List) ->
    HumpName = word:to_hump(Name),
    Code = io_lib:format("~s:~tp/binary", [HumpName, Explain]),
    parse_encode_erl_loop(T, Protocol, Depth, Records, Functions, [HumpName | Names], [Code | List]);

parse_encode_erl_loop([#meta{name = Name, type = bool, explain = undefined} | T], Protocol, Depth, Records, Functions, Names, List) ->
    HumpName = word:to_hump(Name),
    Code = io_lib:format("(type:to_flag(~s)):8", [HumpName]),
    parse_encode_erl_loop(T, Protocol, Depth, Records, Functions, [HumpName | Names], [Code | List]);

parse_encode_erl_loop([#meta{name = Name, type = u8, explain = undefined} | T], Protocol, Depth, Records, Functions, Names, List) ->
    HumpName = word:to_hump(Name),
    Code = io_lib:format("~s:8", [HumpName]),
    parse_encode_erl_loop(T, Protocol, Depth, Records, Functions, [HumpName | Names], [Code | List]);

parse_encode_erl_loop([#meta{name = Name, type = u16, explain = undefined} | T], Protocol, Depth, Records, Functions, Names, List) ->
    HumpName = word:to_hump(Name),
    Code = io_lib:format("~s:16", [HumpName]),
    parse_encode_erl_loop(T, Protocol, Depth, Records, Functions, [HumpName | Names], [Code | List]);

parse_encode_erl_loop([#meta{name = Name, type = u32, explain = undefined} | T], Protocol, Depth, Records, Functions, Names, List) ->
    HumpName = word:to_hump(Name),
    Code = io_lib:format("~s:32", [HumpName]),
    parse_encode_erl_loop(T, Protocol, Depth, Records, Functions, [HumpName | Names], [Code | List]);

parse_encode_erl_loop([#meta{name = Name, type = u64, explain = undefined} | T], Protocol, Depth, Records, Functions, Names, List) ->
    HumpName = word:to_hump(Name),
    Code = io_lib:format("~s:64", [HumpName]),
    parse_encode_erl_loop(T, Protocol, Depth, Records, Functions, [HumpName | Names], [Code | List]);

parse_encode_erl_loop([#meta{name = Name, type = i8, explain = undefined} | T], Protocol, Depth, Records, Functions, Names, List) ->
    HumpName = word:to_hump(Name),
    Code = io_lib:format("~s:8/signed", [HumpName]),
    parse_encode_erl_loop(T, Protocol, Depth, Records, Functions, [HumpName | Names], [Code | List]);

parse_encode_erl_loop([#meta{name = Name, type = i16, explain = undefined} | T], Protocol, Depth, Records, Functions, Names, List) ->
    HumpName = word:to_hump(Name),
    Code = io_lib:format("~s:16/signed", [HumpName]),
    parse_encode_erl_loop(T, Protocol, Depth, Records, Functions, [HumpName | Names], [Code | List]);

parse_encode_erl_loop([#meta{name = Name, type = i32, explain = undefined} | T], Protocol, Depth, Records, Functions, Names, List) ->
    HumpName = word:to_hump(Name),
    Code = io_lib:format("~s:32/signed", [HumpName]),
    parse_encode_erl_loop(T, Protocol, Depth, Records, Functions, [HumpName | Names], [Code | List]);

parse_encode_erl_loop([#meta{name = Name, type = i64, explain = undefined} | T], Protocol, Depth, Records, Functions, Names, List) ->
    HumpName = word:to_hump(Name),
    Code = io_lib:format("~s:64/signed", [HumpName]),
    parse_encode_erl_loop(T, Protocol, Depth, Records, Functions, [HumpName | Names], [Code | List]);

parse_encode_erl_loop([#meta{name = Name, type = f32, explain = undefined} | T], Protocol, Depth, Records, Functions, Names, List) ->
    HumpName = word:to_hump(Name),
    Code = io_lib:format("~s:32/float", [HumpName]),
    parse_encode_erl_loop(T, Protocol, Depth, Records, Functions, [HumpName | Names], [Code | List]);

parse_encode_erl_loop([#meta{name = Name, type = f64, explain = undefined} | T], Protocol, Depth, Records, Functions, Names, List) ->
    HumpName = word:to_hump(Name),
    Code = io_lib:format("~s:64/float", [HumpName]),
    parse_encode_erl_loop(T, Protocol, Depth, Records, Functions, [HumpName | Names], [Code | List]);

parse_encode_erl_loop([#meta{name = Name, type = str, explain = undefined} | T], Protocol, Depth, Records, Functions, Names, List) ->
    HumpName = word:to_hump(Name),
    Code = io_lib:format("(begin ~sBinary = unicode:characters_to_binary(~s), <<(byte_size(~sBinary)):16, ~sBinary/binary>> end)/binary", [Name, Name, Name, Name]),
    parse_encode_erl_loop(T, Protocol, Depth, Records, Functions, [HumpName | Names], [Code | List]);

parse_encode_erl_loop([#meta{name = Name, type = bst, explain = undefined} | T], Protocol, Depth, Records, Functions, Names, List) ->
    HumpName = word:to_hump(Name),
    Code = io_lib:format("(byte_size(~s)):16, (~s)/binary", [Name, Name]),
    parse_encode_erl_loop(T, Protocol, Depth, Records, Functions, [HumpName | Names], [Code | List]);

parse_encode_erl_loop([#meta{name = Name, type = rst, explain = undefined} | T], Protocol, Depth, Records, Functions, Names, List) ->
    HumpName = word:to_hump(Name),
    Code = io_lib:format("(protocol:text(~s))/binary", [HumpName]),
    parse_encode_erl_loop(T, Protocol, Depth, Records, Functions, [HumpName | Names], [Code | List]);

parse_encode_erl_loop([#meta{name = _, type = tuple, explain = Explain} | T], Protocol, Depth, Records, Functions, Names, List) ->
    %% recursive
    {SubRecords, SubFunctions, SubNames, SubCodes} = parse_encode_erl_loop(tuple_to_list(Explain), Protocol, Depth, [], [], [], []),

    SubName = lists:concat([
        "{", string:join(SubNames, ", "), "}"
    ]),

    SubCode = string:join(SubCodes, ", "),

    %% stacked
    parse_encode_erl_loop(T, Protocol, Depth, lists:append(SubRecords, Records), lists:append(SubFunctions, Functions), [SubName | Names], [SubCode | List]);

parse_encode_erl_loop([#meta{name = Name, type = record, explain = Explain} | T], Protocol, Depth, Records, Functions, Names, List) ->

    SubExplain = [Meta || Meta = #meta{} <- tuple_to_list(Explain)],

    %% recursive
    {SubRecords, SubFunctions, SubNames, SubCodes} = parse_encode_erl_loop(SubExplain, Protocol, Depth, [], [], [], []),

    KeyNameList = [KeyName || #meta{name = KeyName} <- SubExplain],
    SubName = lists:concat([
        "#", word:to_snake(Name), "{", string:join([lists:concat([word:to_snake(SubKey), " = ", SubName]) || {SubKey, SubName} <- lists:zip(KeyNameList, SubNames)], ", "), "}"
    ]),

    Record = type:to_atom(word:to_snake(Name)),

    SubCode = string:join(SubCodes, ", "),

    %% stacked
    parse_encode_erl_loop(T, Protocol, Depth, lists:append(SubRecords, [Record | Records]), lists:append(SubFunctions, Functions), [SubName | Names], [SubCode | List]);

parse_encode_erl_loop([#meta{name = _, type = maps, explain = Explain} | T], Protocol, Depth, Records, Functions, Names, List) ->

    %% recursive
    {SubRecords, SubFunctions, SubNames, SubCodes} = parse_encode_erl_loop(maps:values(Explain), Protocol, Depth, [], [], [], []),

    KeyNameList = maps:keys(Explain),

    SubName = lists:concat([
        "#", "{", string:join([lists:concat([word:to_snake(SubKey), " := ", SubName]) || {SubKey, SubName} <- lists:zip(KeyNameList, SubNames)], ", "), "}"
    ]),

    SubCode = string:join(SubCodes, ", "),

    %% stacked
    parse_encode_erl_loop(T, Protocol, Depth, lists:append(SubRecords, Records), lists:append(SubFunctions, Functions), [SubName | Names], [SubCode | List]);

parse_encode_erl_loop([#meta{name = Name, type = list, explain = Explain, key = _} | T], Protocol, Depth, Records, Functions, Names, List) ->
    HumpName = word:to_hump(Name),

    SubExplain = [Meta#meta{name = maps:get(SubName, #{[] => lists:concat([HumpName, "Item"])}, SubName)} || Meta = #meta{name = SubName} <- Explain],

    %% recursive
    {SubRecords, SubFunctions, SubNames, SubCodes} = parse_encode_erl_loop(SubExplain, Protocol, Depth + 1, [], [], [], []),

    SubFunction = lists:concat([
        "encode_", word:to_snake(HumpName), "_", Protocol, "(Acc = <<_/binary>>, Length, []) ->", "\n",
        "    ", "<<Length:16, Acc/binary>>;", "\n",
        "encode_", word:to_snake(HumpName), "_", Protocol, "(Acc = <<_/binary>>, Length, [", string:join(SubNames, ", "), " | ", HumpName, "]) ->", "\n",
        "    ", "encode_", word:to_snake(HumpName), "_", Protocol, "(<<Acc/binary, ",  string:join(SubCodes, ", "), ">>, Length + 1, ", HumpName, ").", "\n",
        [lists:concat(["\n", Sub]) || Sub <- SubFunctions]
    ]),

    Code = lists:concat([
        "(encode_", word:to_snake(HumpName), "_", Protocol, "(<<>>, 0, ", HumpName, "))/binary"
    ]),

    parse_encode_erl_loop(T, Protocol, Depth, lists:append(SubRecords, Records), [SubFunction | Functions], [HumpName | Names], [Code | List]);

parse_encode_erl_loop([#meta{name = Name, type = ets, explain = Explain, key = _} | T], Protocol, Depth, Records, Functions, Names, List) ->
    HumpName = word:to_hump(Name),

    SubExplain = [Meta#meta{name = maps:get(SubName, #{[] => lists:concat([HumpName, "Item"])}, SubName)} || Meta = #meta{name = SubName} <- Explain],

    %% recursive
    {SubRecords, SubFunctions, SubNames, SubCodes} = parse_encode_erl_loop(SubExplain, Protocol, Depth + 1, [], [], [], []),

    SubFunction = lists:concat([
        "encode_", word:to_snake(HumpName), "_", Protocol, "(Acc = <<_/binary>>, Length, Tab, '$end_of_table') ->", "\n",
        "    ", "ets:safe_fixtable(Tab, false),", "\n",
        "    ", "<<Length:16, Acc/binary>>;", "\n",
        "encode_", word:to_snake(HumpName), "_", Protocol, "(Acc = <<_/binary>>, Length, Tab, Key) ->", "\n",
        "    ", "case ets:lookup(Tab, Key) of", "\n",
        "    ", "    ", "[] ->", "\n",
        "    ", "    ", "    ", "encode_", word:to_snake(HumpName), "_", Protocol, "(Acc, Length, Tab, ets:next(Tab, Key));", "\n",
        "    ", "    ", "[", string:join(SubNames, ", "), "] ->", "\n",
        "    ", "    ", "    ", "encode_", word:to_snake(HumpName), "_", Protocol, "(<<Acc/binary, ", string:join(SubCodes, ", "), ">>, Length + 1, Tab, ets:next(Tab, Key))", "\n",
        "    ", "end.", "\n",
        [lists:concat(["\n", Sub]) || Sub <- SubFunctions]
    ]),

    Code = lists:concat([
        "(encode_", word:to_snake(HumpName), "_", Protocol, "(<<>>, 0, ets:safe_fixtable(", HumpName, ", true) andalso ", HumpName, ", ets:first(", HumpName, ")))/binary"
    ]),

    parse_encode_erl_loop(T, Protocol, Depth, lists:append(SubRecords, Records), [SubFunction | Functions], [HumpName | Names], [Code | List]).
