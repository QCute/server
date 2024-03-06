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

    Names = parse_request_erl_loop(Meta, []),

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

    case Meta of
        [_] ->
            NamesInput = Names;
        _ ->
            NamesInput = [lists:concat(["[", string:join(Names, ", "), "]"])]
    end,

    AllInput = [P || P <- [StateInput, ProtocolInput | NamesInput], P =/= []],
    Input = string:join(AllInput, ", "),
    AllOutput = [A || A <- [StateOutput, ProtocolOutput | Names], A =/= []],
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


parse_request_erl_loop([], List) ->
    %% construct as a list
    lists:reverse(List);

parse_request_erl_loop([#meta{name = _, type = tuple, comment = _, explain = Explain} | T], List) ->
    SubNames = parse_request_erl_loop(Explain, []),
    parse_request_erl_loop(T, [SubNames | List]);

parse_request_erl_loop([#meta{name = _, type = record, comment = _, explain = Explain} | T], List) ->
    SubNames = parse_request_erl_loop(Explain, []),
    parse_request_erl_loop(T, [SubNames | List]);

parse_request_erl_loop([#meta{name = Name} | T], List) ->
    parse_request_erl_loop(T, [Name | List]).


%%%===================================================================
%%% response creator
%%%===================================================================
parse_response_erl(0, _, #handler{module = undefined, function = undefined}, _) ->
    %% default handler code
    #file{};
parse_response_erl(_, _, #handler{module = undefined, function = undefined}, _) ->
    %% default handler code
    #file{};
parse_response_erl(Protocol, Meta, #handler{module = Module, function = Function, state = State, alias = Alias, protocol = IsContainProtocol, response = sender, imp = Imp}, Name) ->

    Names = parse_response_erl_loop(Meta, []),

    DuplicateName = listing:duplicate(Names),
    DuplicateName =/= [] andalso erlang:throw(lists:flatten(io_lib:format("Found duplicate name: ~tp", [DuplicateName]))),

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

    case Meta of
        [_] ->
            NamesInput = Names;
        _ ->
            NamesInput = [lists:concat(["[", string:join(Names, ", "), "]"])]
    end,

    case lists:concat([Module, "_", "protocol"]) == Name of
        true ->
            NameSpace = "";
        _ ->
            NameSpace = lists:concat([Module, "_"])
    end,

    case Alias of
        true ->
            Method = Function;
        _ ->
            Method = Alias
    end,

    case Imp of
        [] ->
            Sender = "sender";
        _ ->
            Sender = lists:concat([word:to_snake(Imp), "_", "sender"])
    end,

    AllInput = [P || P <- NamesInput, P =/= []],
    Input = string:join(AllInput, ", "),
    AllOutput = [A || A <- [StateOutput, ProtocolOutput | Names], A =/= []],
    Output = string:join(AllOutput, ", "),

    Export = lists:concat([
        "-export([send_", NameSpace, Method, "/", length(AllOutput), "]).", "\n"
    ]),

    Code = lists:concat([
        "send_", NameSpace, Method, "(", Output, ") ->", "\n",
        "    ", "{ok, Binary} = ", Name, ":encode(", Protocol, ", ", Input, "),", "\n",
        "    ", Sender, ":send(", StateOutput, ", Binary).", "\n",
        "\n"
    ]),

    #file{export = Export, function = Code};

parse_response_erl(Protocol, Meta, #handler{module = Module, function = Function, state = State, alias = Alias, protocol = IsContainProtocol, response = buffer}, Name) ->

    Names = parse_response_erl_loop(Meta, []),

    DuplicateName = listing:duplicate(Names),
    DuplicateName =/= [] andalso erlang:throw(lists:flatten(io_lib:format("Found duplicate name: ~tp", [DuplicateName]))),

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

    case Meta of
        [_] ->
            NamesInput = Names;
        _ ->
            NamesInput = [lists:concat(["[", string:join(Names, ", "), "]"])]
    end,

    AllInput = [P || P <- NamesInput, P =/= []],
    Input = string:join(AllInput, ", "),
    AllOutput = [A || A <- [StateOutput, ProtocolOutput | Names], A =/= []],
    Output = string:join(AllOutput, ", "),

    case lists:concat([Module, "_", "protocol"]) == Name of
        true ->
            NameSpace = "";
        _ ->
            NameSpace = lists:concat([Module, "_"])
    end,

    case Alias of
        true ->
            Method = Function;
        _ ->
            Method = Alias
    end,

    Export = lists:concat([
        "-export([send_", NameSpace, Method, "/", length(AllOutput), "]).", "\n"
    ]),

    Code = lists:concat([
        "send_", NameSpace, Method, "(", Output, ") ->", "\n",
        "    ", "{ok, Binary} = ", Name, ":encode(", Protocol, ", ", Input, "),", "\n",
        "    ", StateOutput, "#", word:to_snake(StateOutput), "{buffer = [Binary | ", StateOutput, "#", word:to_snake(StateOutput), ".buffer]}.", "\n",
        "\n"
    ]),

    %% include
    [Includes] = maker:collect_include([State]),

    #file{export = Export, import = Includes, function = Code}.


parse_response_erl_loop([], List) ->
    %% construct as a list
    lists:reverse(List);

parse_response_erl_loop([#meta{name = Name, type = tuple, comment = _} | T], List) ->
    parse_response_erl_loop(T, [Name | List]);

parse_response_erl_loop([#meta{name = Name, type = record, comment = _} | T], List) ->
    parse_response_erl_loop(T, [Name | List]);

parse_response_erl_loop([#meta{name = Name} | T], List) ->
    parse_response_erl_loop(T, [Name | List]).

%%%===================================================================
%%% decode
%%%===================================================================
%% erl code
%%parse_decode_erl(Protocol, []) ->
%%    Code = lists:concat([
%%        "decode(", Protocol, ", _Rest_ = <<_/binary>>) ->", "\n",
%%        "    ", "{ok, []};", "\n"
%%    ]),
%%    {Code, ""};
parse_decode_erl(Protocol, Meta) ->
    %% start with 3 tabs(4 space) padding
    {_, Records, Functions, Names, Codes} = parse_decode_erl_loop(Meta, Protocol, 1, "", [], [], [], []),

    %% args shrink
    case Meta of
        [_] ->
            Arg = string:join(Names, ", ");
        _ ->
            Arg = lists:concat(["[", string:join(Names, ", "), "]"])
    end,

    %% codes format
    CodesDefault = lists:concat([
        "    ", "{ok, ", Arg, "};", "\n"
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
    Code = lists:concat([
        "    ", "<<", Name, ":", Length, "/binary, _", Name, "Rest_/binary>> = _", ScopeArgs, "Rest_,"
    ]),
    parse_decode_erl_loop(T, Protocol, Depth, Name, Records, Functions, [Name | Names], [Code | List]);

parse_decode_erl_loop([#meta{name = Name, type = bool, explain = []} | T], Protocol, Depth, ScopeArgs, Records, Functions, Names, List) ->
    Code = lists:concat([
        "    ", "<<", Name, "Flag:8, _", Name, "Rest_/binary>> = _", ScopeArgs, "Rest_,", "\n",
        "    ", Name, " = type:to_boolean(", Name, "Flag),"
    ]),

    parse_decode_erl_loop(T, Protocol, Depth, Name, Records, Functions, [Name | Names], [Code | List]);

parse_decode_erl_loop([#meta{name = Name, type = u8, explain = []} | T], Protocol, Depth, ScopeArgs, Records, Functions, Names, List) ->
    Code = lists:concat([
        "    ", "<<", Name, ":8, _", Name, "Rest_/binary>> = _", ScopeArgs, "Rest_,"
    ]),

    parse_decode_erl_loop(T, Protocol, Depth, Name, Records, Functions, [Name | Names], [Code | List]);

parse_decode_erl_loop([#meta{name = Name, type = u16, explain = []} | T], Protocol, Depth, ScopeArgs, Records, Functions, Names, List) ->
    Code = lists:concat([
        "    ", "<<", Name, ":16, _", Name, "Rest_/binary>> = _", ScopeArgs, "Rest_,"
    ]),

    parse_decode_erl_loop(T, Protocol, Depth, Name, Records, Functions, [Name | Names], [Code | List]);

parse_decode_erl_loop([#meta{name = Name, type = u32, explain = []} | T], Protocol, Depth, ScopeArgs, Records, Functions, Names, List) ->
    Code = lists:concat([
        "    ", "<<", Name, ":32, _", Name, "Rest_/binary>> = _", ScopeArgs, "Rest_,"
    ]),

    parse_decode_erl_loop(T, Protocol, Depth, Name, Records, Functions, [Name | Names], [Code | List]);

parse_decode_erl_loop([#meta{name = Name, type = u64, explain = []} | T], Protocol, Depth, ScopeArgs, Records, Functions, Names, List) ->
    Code = lists:concat([
        "    ", "<<", Name, ":64, _", Name, "Rest_/binary>> = _", ScopeArgs, "Rest_,"
    ]),

    parse_decode_erl_loop(T, Protocol, Depth, Name, Records, Functions, [Name | Names], [Code | List]);

parse_decode_erl_loop([#meta{name = Name, type = i8, explain = []} | T], Protocol, Depth, ScopeArgs, Records, Functions, Names, List) ->
    Code = lists:concat([
        "    ", "<<", Name, ":8/signed, _", Name, "Rest_/binary>> = _", ScopeArgs, "Rest_,"
    ]),

    parse_decode_erl_loop(T, Protocol, Depth, Name, Records, Functions, [Name | Names], [Code | List]);

parse_decode_erl_loop([#meta{name = Name, type = i16, explain = []} | T], Protocol, Depth, ScopeArgs, Records, Functions, Names, List) ->
    Code = lists:concat([
        "    ", "<<", Name, ":16/signed, _", Name, "Rest_/binary>> = _", ScopeArgs, "Rest_,"
    ]),

    parse_decode_erl_loop(T, Protocol, Depth, Name, Records, Functions, [Name | Names], [Code | List]);

parse_decode_erl_loop([#meta{name = Name, type = i32, explain = []} | T], Protocol, Depth, ScopeArgs, Records, Functions, Names, List) ->
    Code = lists:concat([
        "    ", "<<", Name, ":32/signed, _", Name, "Rest_/binary>> = _", ScopeArgs, "Rest_,"
    ]),

    parse_decode_erl_loop(T, Protocol, Depth, Name, Records, Functions, [Name | Names], [Code | List]);

parse_decode_erl_loop([#meta{name = Name, type = i64, explain = []} | T], Protocol, Depth, ScopeArgs, Records, Functions, Names, List) ->
    Code = lists:concat([
        "    ", "<<", Name, ":64/signed, _", Name, "Rest_/binary>> = _", ScopeArgs, "Rest_,"
    ]),

    parse_decode_erl_loop(T, Protocol, Depth, Name, Records, Functions, [Name | Names], [Code | List]);

parse_decode_erl_loop([#meta{name = Name, type = f32, explain = []} | T], Protocol, Depth, ScopeArgs, Records, Functions, Names, List) ->
    Code = lists:concat([
        "    ", "<<", Name, ":32/float, _", Name, "Rest_/binary>> = _", ScopeArgs, "Rest_,"
    ]),

    parse_decode_erl_loop(T, Protocol, Depth, Name, Records, Functions, [Name | Names], [Code | List]);

parse_decode_erl_loop([#meta{name = Name, type = f64, explain = []} | T], Protocol, Depth, ScopeArgs, Records, Functions, Names, List) ->
    Code = lists:concat([
        "    ", "<<", Name, ":64/float, _", Name, "Rest_/binary>> = _", ScopeArgs, "Rest_,"
    ]),

    parse_decode_erl_loop(T, Protocol, Depth, Name, Records, Functions, [Name | Names], [Code | List]);

parse_decode_erl_loop([#meta{name = Name, type = str, explain = []} | T], Protocol, Depth, ScopeArgs, Records, Functions, Names, List) ->
    Code = lists:concat([
        "    ", "<<", Name, "BinaryLength:16, ", Name, "Binary:", Name, "BinaryLength/binary, _", Name, "Rest_/binary>> = _", ScopeArgs, "Rest_,", "\n",
        "    ", Name, " = unicode:characters_to_list(", Name, "Binary),"
    ]),

    parse_decode_erl_loop(T, Protocol, Depth, Name, Records, Functions, [Name | Names], [Code | List]);

parse_decode_erl_loop([#meta{name = Name, type = bst, explain = []} | T], Protocol, Depth, ScopeArgs, Records, Functions, Names, List) ->
    Code = lists:concat([
        "    ", "<<", Name, "Length:16, ", Name, ":", Name, "Length/binary, _", Name, "Rest_/binary>> = _", ScopeArgs, "Rest_,"
    ]),

    parse_decode_erl_loop(T, Protocol, Depth, Name, Records, Functions, [Name | Names], [Code | List]);

parse_decode_erl_loop([#meta{name = Name, type = rst, explain = []} | T], Protocol, Depth, ScopeArgs, Records, Functions, Names, List) ->
    Code = lists:concat([
        "    ", "<<", Name, "Length:16, ", Name, ":", Name, "Length/binary, _", Name, "Rest_/binary>> = _", ScopeArgs, "Rest_,"
    ]),

    %% stacked
    parse_decode_erl_loop(T, Protocol, Depth, Name, Records, Functions, [Name | Names], [Code | List]);

parse_decode_erl_loop([#meta{name = _, type = tuple, explain = Explain} | T], Protocol, Depth, ScopeArgs, Records, Functions, Names, List) ->
    %% recursive
    {NextScopeArgs, SubRecords, SubFunctions, SubNames, SubCodes} = parse_decode_erl_loop(Explain, Protocol, Depth, ScopeArgs, [], [], [], []),

    SubName = lists:concat([
        "{", string:join(SubNames, ", "), "}"
    ]),

    SubCode = string:join(SubCodes, "\n"),

    %% stacked
    parse_decode_erl_loop(T, Protocol, Depth, NextScopeArgs, lists:append(SubRecords, Records), lists:append(SubFunctions, Functions), [SubName | Names], [SubCode | List]);

parse_decode_erl_loop([#meta{name = Name, type = record, explain = Explain} | T], Protocol, Depth, ScopeArgs, Records, Functions, Names, List) ->
    %% recursive
    {NextScopeArgs, SubRecords, SubFunctions, _, SubCodes} = parse_decode_erl_loop(Explain, Protocol, Depth, ScopeArgs, [], [], [], []),

    SubName = lists:concat([
        "#", word:to_snake(Name), "{", string:join([lists:concat([word:to_snake(FieldName), " = ", FieldName]) || #meta{name = FieldName} <- Explain], ", "), "}"
    ]),

    Record = type:to_atom(word:to_snake(Name)),
    
    SubCode = string:join(SubCodes, "\n"),

    parse_decode_erl_loop(T, Protocol, Depth, NextScopeArgs, lists:append(SubRecords, [Record | Records]), lists:append(SubFunctions, Functions), [SubName | Names], [SubCode | List]);

parse_decode_erl_loop([#meta{name = Name, type = list, explain = Explain = [#meta{explain = SubExplain}], key = Key} | T], Protocol, Depth, ScopeArgs, Records, Functions, Names, List) ->
    KeyField = lists:keyfind(Key, #meta.name, SubExplain),
    Key =/= undefined andalso KeyField == undefined andalso erlang:throw(lists:flatten(io_lib:format("Cound not found field ~ts in explain", [Key]))),

    %% recursive
    {NextScopeArgs, SubRecords, SubFunctions, SubNames, SubCodes} = parse_decode_erl_loop(Explain, Protocol, Depth + 1, "", [], [], [], []),

    SubFunction = lists:concat([
        "decode_", word:to_snake(Name), "_", Protocol, "(<<_/binary>>, Size, 0, List) ->", "\n",
        "    ", "{Size, List};", "\n",
        "decode_", word:to_snake(Name), "_", Protocol, "(_Rest_ = <<_/binary>>, Size, ", Name, "Length, List) ->", "\n",
        string:join(SubCodes, "\n"), "\n",
        "    ", "decode_", word:to_snake(Name), "_", Protocol, "(_", NextScopeArgs, "Rest_, Size + byte_size(_Rest_) - byte_size(_", NextScopeArgs, "Rest_), ", Name, "Length - 1, ", "[", string:join(SubNames, ", "), " | List]).", "\n",
        [lists:concat(["\n", Sub]) || Sub <- SubFunctions]
    ]),

    Code = lists:concat([
        "    ", "<<", Name, "Length:16, _", Name, "LengthRest_/binary>> = _", ScopeArgs, "Rest_,", "\n",
        "    ", "{", Name, "ByteSize, ", Name, "} = decode_", word:to_snake(Name), "_", Protocol, "(_",  Name, "LengthRest_, 0, ", Name, "Length, [])", ",", "\n",
        "    ", "<<_:", Name, "ByteSize/binary, _", Name, "Rest_/binary>> = _", Name, "LengthRest_,"
    ]),

    parse_decode_erl_loop(T, Protocol, Depth, Name, lists:append(SubRecords, Records), [SubFunction | Functions], [Name | Names], [Code | List]).

%%%===================================================================
%%% encode
%%%===================================================================
%% erl code
%%parse_encode_erl(Protocol, []) ->
%%    Code = lists:concat([
%%        "encode(", Protocol, ", _) ->", "\n",
%%        "    ", "{ok, <<0:16, ", Protocol, ":16>>};", "\n"
%%    ]),
%%    {Code, ""};
parse_encode_erl(Protocol, Meta) ->
    %% start with 3 tabs(4 space) padding
    {Records, Functions, Names, Codes} = parse_encode_erl_loop(Meta, Protocol, 1, [], [], [], []),

    %% args shrink
    case Meta of
        [_] ->
            Arg = string:join(Names, ", ");
        _ ->
            Arg = lists:concat(["[", string:join(Names, ", "), "]"])
    end,

    %% format one protocol define
    Code = lists:concat([
        "encode(", Protocol, ", ", Arg, ") ->" "\n",
        "    ", "Data", Protocol, " = <<", string:join(Codes, ", "), ">>,", "\n",
        "    ", "{ok, <<(byte_size(Data", Protocol, ")):16, ", Protocol, ":16, Data", Protocol, "/binary>>};", "\n"
    ]),
    #file{import = Records, function = Code, extra = string:join(Functions, "\n")}.

parse_encode_erl_loop([], _, _, Records, Functions, Names, List) ->
    %% construct as a list
    %% {string:join(lists:reverse(Functions), "\n"), string:join(lists:reverse(Names), ", "), string:join(lists:reverse(List), ", ")};
    {lists:reverse(Records), lists:reverse(Functions), lists:reverse(Names), lists:reverse(List)};

parse_encode_erl_loop([#meta{name = _, type = zero, explain = []} | T], Protocol, Depth, Records, Functions, Names, List) ->
    Name = "_",
    parse_encode_erl_loop(T, Protocol, Depth, Records, Functions, [Name | Names], List);

parse_encode_erl_loop([#meta{name = Name, type = binary, explain = Explain} | T], Protocol, Depth, Records, Functions, Names, List) ->
    Code = io_lib:format("~s:~tp/binary", [Name, Explain]),
    parse_encode_erl_loop(T, Protocol, Depth, Records, Functions, [Name | Names], [Code | List]);

parse_encode_erl_loop([#meta{name = Name, type = bool, explain = []} | T], Protocol, Depth, Records, Functions, Names, List) ->
    Code = io_lib:format("(type:to_flag(~s)):8", [Name]),
    parse_encode_erl_loop(T, Protocol, Depth, Records, Functions, [Name | Names], [Code | List]);

parse_encode_erl_loop([#meta{name = Name, type = u8, explain = []} | T], Protocol, Depth, Records, Functions, Names, List) ->
    Code = io_lib:format("~s:8", [Name]),
    parse_encode_erl_loop(T, Protocol, Depth, Records, Functions, [Name | Names], [Code | List]);

parse_encode_erl_loop([#meta{name = Name, type = u16, explain = []} | T], Protocol, Depth, Records, Functions, Names, List) ->
    Code = io_lib:format("~s:16", [Name]),
    parse_encode_erl_loop(T, Protocol, Depth, Records, Functions, [Name | Names], [Code | List]);

parse_encode_erl_loop([#meta{name = Name, type = u32, explain = []} | T], Protocol, Depth, Records, Functions, Names, List) ->
    Code = io_lib:format("~s:32", [Name]),
    parse_encode_erl_loop(T, Protocol, Depth, Records, Functions, [Name | Names], [Code | List]);

parse_encode_erl_loop([#meta{name = Name, type = u64, explain = []} | T], Protocol, Depth, Records, Functions, Names, List) ->
    Code = io_lib:format("~s:64", [Name]),
    parse_encode_erl_loop(T, Protocol, Depth, Records, Functions, [Name | Names], [Code | List]);

parse_encode_erl_loop([#meta{name = Name, type = i8, explain = []} | T], Protocol, Depth, Records, Functions, Names, List) ->
    Code = io_lib:format("~s:8/signed", [Name]),
    parse_encode_erl_loop(T, Protocol, Depth, Records, Functions, [Name | Names], [Code | List]);

parse_encode_erl_loop([#meta{name = Name, type = i16, explain = []} | T], Protocol, Depth, Records, Functions, Names, List) ->
    Code = io_lib:format("~s:16/signed", [Name]),
    parse_encode_erl_loop(T, Protocol, Depth, Records, Functions, [Name | Names], [Code | List]);

parse_encode_erl_loop([#meta{name = Name, type = i32, explain = []} | T], Protocol, Depth, Records, Functions, Names, List) ->
    Code = io_lib:format("~s:32/signed", [Name]),
    parse_encode_erl_loop(T, Protocol, Depth, Records, Functions, [Name | Names], [Code | List]);

parse_encode_erl_loop([#meta{name = Name, type = i64, explain = []} | T], Protocol, Depth, Records, Functions, Names, List) ->
    Code = io_lib:format("~s:64/signed", [Name]),
    parse_encode_erl_loop(T, Protocol, Depth, Records, Functions, [Name | Names], [Code | List]);

parse_encode_erl_loop([#meta{name = Name, type = f32, explain = []} | T], Protocol, Depth, Records, Functions, Names, List) ->
    Code = io_lib:format("~s:32/float", [Name]),
    parse_encode_erl_loop(T, Protocol, Depth, Records, Functions, [Name | Names], [Code | List]);

parse_encode_erl_loop([#meta{name = Name, type = f64, explain = []} | T], Protocol, Depth, Records, Functions, Names, List) ->
    Code = io_lib:format("~s:64/float", [Name]),
    parse_encode_erl_loop(T, Protocol, Depth, Records, Functions, [Name | Names], [Code | List]);

parse_encode_erl_loop([#meta{name = Name, type = str, explain = []} | T], Protocol, Depth, Records, Functions, Names, List) ->
    Code = io_lib:format("(begin ~sBinary = unicode:characters_to_binary(~s), <<(byte_size(~sBinary)):16, ~sBinary/binary>> end)/binary", [Name, Name, Name, Name]),
    parse_encode_erl_loop(T, Protocol, Depth, Records, Functions, [Name | Names], [Code | List]);

parse_encode_erl_loop([#meta{name = Name, type = bst, explain = []} | T], Protocol, Depth, Records, Functions, Names, List) ->
    Code = io_lib:format("(byte_size(~s)):16, (~s)/binary", [Name, Name]),
    parse_encode_erl_loop(T, Protocol, Depth, Records, Functions, [Name | Names], [Code | List]);

parse_encode_erl_loop([#meta{name = Name, type = rst, explain = []} | T], Protocol, Depth, Records, Functions, Names, List) ->
    Code = io_lib:format("(protocol:text(~s))/binary", [Name]),
    parse_encode_erl_loop(T, Protocol, Depth, Records, Functions, [Name | Names], [Code | List]);

parse_encode_erl_loop([#meta{name = _, type = tuple, explain = Explain} | T], Protocol, Depth, Records, Functions, Names, List) ->
    %% recursive
    {SubRecords, SubFunctions, SubNames, SubCodes} = parse_encode_erl_loop(Explain, Protocol, Depth, [], [], [], []),

    SubName = lists:concat([
        "{", string:join(SubNames, ", "), "}"
    ]),

    SubCode = string:join(SubCodes, ", "),

    %% stacked
    parse_encode_erl_loop(T, Protocol, Depth, lists:append(SubRecords, Records), lists:append(SubFunctions, Functions), [SubName | Names], [SubCode | List]);

parse_encode_erl_loop([#meta{name = Name, type = record, explain = Explain, key = KeyNameList} | T], Protocol, Depth, Records, Functions, Names, List) ->
    %% recursive
    {SubRecords, SubFunctions, SubNames, SubCodes} = parse_encode_erl_loop(Explain, Protocol, Depth, [], [], [], []),

    SubName = lists:concat([
        "#", word:to_snake(Name), "{", string:join([lists:concat([word:to_snake(SubKey), " = ", SubName]) || {SubKey, SubName} <- lists:zip(KeyNameList, SubNames)], ", "), "}"
    ]),

    Record = type:to_atom(word:to_snake(Name)),

    SubCode = string:join(SubCodes, ", "),

    %% stacked
    parse_encode_erl_loop(T, Protocol, Depth, lists:append(SubRecords, [Record | Records]), lists:append(SubFunctions, Functions), [SubName | Names], [SubCode | List]);

parse_encode_erl_loop([#meta{name = Name, type = list, explain = Explain = [#meta{explain = SubExplain}], key = Key} | T], Protocol, Depth, Records, Functions, Names, List) ->
    KeyField = lists:keyfind(Key, #meta.name, SubExplain),
    Key =/= undefined andalso KeyField == undefined andalso erlang:throw(lists:flatten(io_lib:format("Cound not found field ~ts in explain", [Key]))),

    %% recursive
    {SubRecords, SubFunctions, SubNames, SubCodes} = parse_encode_erl_loop(Explain, Protocol, Depth + 1, [], [], [], []),

    SubFunction = lists:concat([
        "encode_", word:to_snake(Name), "_", Protocol, "(Acc = <<_/binary>>, Length, []) ->", "\n",
        "    ", "<<Length:16, Acc/binary>>;", "\n",
        "encode_", word:to_snake(Name), "_", Protocol, "(Acc = <<_/binary>>, Length, [", string:join(SubNames, ", "), " | ", Name, "]) ->", "\n",
        "    ", "encode_", word:to_snake(Name), "_", Protocol, "(<<Acc/binary, ",  string:join(SubCodes, ", "), ">>, Length + 1, ", Name, ").", "\n",
        [lists:concat(["\n", Sub]) || Sub <- SubFunctions]
    ]),

    Code = lists:concat([
        "(encode_", word:to_snake(Name), "_", Protocol, "(<<>>, 0, ", Name, "))/binary"
    ]),

    parse_encode_erl_loop(T, Protocol, Depth, lists:append(SubRecords, Records), [SubFunction | Functions], [Name | Names], [Code | List]);

parse_encode_erl_loop([#meta{name = Name, type = ets, explain = Explain = [#meta{explain = SubExplain}], key = Key} | T], Protocol, Depth, Records, Functions, Names, List) ->
    KeyField = lists:keyfind(Key, #meta.name, SubExplain),
    Key =/= undefined andalso KeyField == undefined andalso erlang:throw(lists:flatten(io_lib:format("Cound not found field ~ts in explain", [Key]))),

    %% recursive
    {SubRecords, SubFunctions, SubNames, SubCodes} = parse_encode_erl_loop(Explain, Protocol, Depth + 1, [], [], [], []),

    SubFunction = lists:concat([
        "encode_", word:to_snake(Name), "_", Protocol, "(Acc = <<_/binary>>, Length, Tab, '$end_of_table') ->", "\n",
        "    ", "ets:safe_fixtable(Tab, false),", "\n",
        "    ", "<<Length:16, Acc/binary>>;", "\n",
        "encode_", word:to_snake(Name), "_", Protocol, "(Acc = <<_/binary>>, Length, Tab, Key) ->", "\n",
        "    ", "case ets:lookup(Tab, Key) of", "\n",
        "    ", "    ", "[] ->", "\n",
        "    ", "    ", "    ", "encode_", word:to_snake(Name), "_", Protocol, "(Acc, Length, Tab, ets:next(Tab, Key));", "\n",
        "    ", "    ", "[", string:join(SubNames, ", "), "] ->", "\n",
        "    ", "    ", "    ", "encode_", word:to_snake(Name), "_", Protocol, "(<<Acc/binary, ", string:join(SubCodes, ", "), ">>, Length + 1, Tab, ets:next(Tab, Key))", "\n",
        "    ", "end.", "\n",
        [lists:concat(["\n", Sub]) || Sub <- SubFunctions]
    ]),

    Code = lists:concat([
        "(encode_", word:to_snake(Name), "_", Protocol, "(<<>>, 0, ets:safe_fixtable(", Name, ", true) andalso ", Name, ", ets:first(", Name, ")))/binary"
    ]),

    parse_encode_erl_loop(T, Protocol, Depth, lists:append(SubRecords, Records), [SubFunction | Functions], [Name | Names], [Code | List]).
