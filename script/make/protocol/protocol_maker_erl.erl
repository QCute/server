%%%-------------------------------------------------------------------
%%% @doc
%%% make protocol define to erl io code
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_maker_erl).
-export([format_code/2, format_handler/1]).
-export([parse_handler_erl/3]).
-export([parse_encode_erl/2, parse_decode_erl/2]).
-include("../../../include/serialize.hrl").
%% ast metadata
-record(meta, {name = [], type, comment = [], explain = [], key}).
%% lang code
-record(code, {protocol = 0, erl = [], handler = [], html = [], lua_code = [], lua_meta = [], js_code = [], js_meta = [], cs_code = [], cs_meta = []}).
%%%===================================================================
%%% API functions
%%%===================================================================
format_code(DecodeList, EncodeList) ->
    %% decode
    ErlDecode = [Code || #code{erl = {Code, _}} <- DecodeList],
    ErlDecodeFunction = [Function || #code{erl = {_, Function}} <- DecodeList, Function =/= []],
    ErlDecodeDefault = lists:concat([
        "decode(Protocol, Binary) ->", "\n",
        "    ", "{error, Protocol, Binary}.", "\n"
    ]),
    Decode = lists:append(ErlDecode, [ErlDecodeDefault | ErlDecodeFunction]),

    %% encode
    ErlEncode = [Code || #code{erl = {Code, _}} <- EncodeList],
    ErlEncodeFunction = [Function || #code{erl = {_, Function}} <- EncodeList, Function =/= []],
    ErlEncodeDefault = lists:concat([
        "encode(Protocol, Data) ->", "\n"
        "    ", "{error, Protocol, Data}.", "\n"
    ]),
    Encode = lists:append(ErlEncode, [ErlEncodeDefault | ErlEncodeFunction]),

    lists:concat([
        "\n",
        "\n",
        "-spec decode(Protocol :: non_neg_integer(), Binary :: binary()) -> {ok, [integer() | binary() | list()]} | {error, Protocol :: non_neg_integer(), Binary :: binary()}.", "\n",
        string:join(Decode, "\n"),
        "\n",
        "\n",
        "-spec encode(Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()) -> {ok, binary()} | {error, Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()}.", "\n",
        string:join(Encode, "\n"),
        "\n"
    ]).

format_handler(DecodeList) ->
    %% handler code
    %% @todo handler spec
    Code = listing:collect(#code.handler, DecodeList, []),
    case lists:any(fun(#code{protocol = Protocol}) -> Protocol == 0 end, DecodeList) of
        true ->
            Code;
        false ->
            lists:concat([
                Code,
                "handle(_, Protocol, Data) ->", "\n",
                "    ", "{error, Protocol, Data}.", "\n"
            ])
    end.

%%%===================================================================
%%% handler
%%%===================================================================
parse_handler_erl(0, _, #handler{module = undefined, function = undefined}) ->
    %% default handler code
    "";
parse_handler_erl(0, _, #handler{module = Module, function = Function, arg = Arg, protocol = IsContainProtocol}) ->
    %% default handler code
    case Arg of
        [] ->
            StateParam = "_",
            StateArg = "";
        _ ->
            StateParam = word:to_hump(Arg),
            StateArg = word:to_hump(Arg)
    end,

    case IsContainProtocol of
        true ->
            ProtocolArg = "Protocol";
        false ->
            ProtocolArg = ""
    end,

    Params = string:join([P || P <- [StateParam, "Protocol", "Data"], P =/= []], ", "),
    Args = string:join([A || A <- [StateArg, ProtocolArg, "Data"], A =/= []], ", "),

    lists:concat([
        "handle(", Params, ") ->", "\n",
        "    ", Module, ":", Function, "(", Args, ").", "\n",
        "\n"
    ]);
parse_handler_erl(Protocol, Meta, #handler{module = Module, function = Function, arg = Arg, protocol = IsContainProtocol}) ->

    Names = parse_handler_erl_loop(Meta, []),

    case Arg of
        [] ->
            StateParam = "_",
            StateArg = "";
        _ ->
            StateParam = word:to_hump(Arg),
            StateArg = word:to_hump(Arg)
    end,

    case IsContainProtocol of
        true ->
            ProtocolParam = lists:concat([Protocol]),
            ProtocolArg = lists:concat([Protocol]);
        false ->
            ProtocolParam = lists:concat([Protocol]),
            ProtocolArg = ""
    end,

    case Meta of
        [_] ->
            NamesParam = string:join(Names, ", "),
            NamesArg = string:join(Names, ", ");
        _ ->
            NamesParam = lists:concat(["[", string:join(Names, ", "), "]"]),
            NamesArg = string:join(Names, ", ")
    end,

    Params = string:join([P || P <- [StateParam, ProtocolParam, NamesParam], P =/= []], ", "),
    Args = string:join([A || A <- [StateArg, ProtocolArg, NamesArg], A =/= []], ", "),

    lists:concat([
        "handle(", Params, ") ->", "\n",
        "    ", Module, ":", Function, "(", Args, ");", "\n",
        "\n"
    ]).

parse_handler_erl_loop([], List) ->
    %% construct as a list
    lists:reverse(List);

parse_handler_erl_loop([#meta{name = _, type = tuple, comment = _, explain = Explain} | T], List) ->
    SubNames = parse_handler_erl_loop(Explain, []),
    parse_handler_erl_loop(T, [SubNames | List]);

parse_handler_erl_loop([#meta{name = _, type = record, comment = _, explain = Explain} | T], List) ->
    SubNames = parse_handler_erl_loop(Explain, []),
    parse_handler_erl_loop(T, [SubNames | List]);

parse_handler_erl_loop([#meta{name = Name} | T], List) ->
    parse_handler_erl_loop(T, [Name | List]).

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
    {_, Functions, Names, Codes} = parse_decode_erl_loop(Meta, Protocol, 1, "", [], [], []),

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
    {Code, string:join(Functions, "\n")}.

parse_decode_erl_loop([], _, _, ScopeArgs, Functions, Names, List) ->
    %% construct as a list
    %% {string:join(lists:reverse(Functions), "\n"), string:join(lists:reverse(Names), ", "), string:join(lists:reverse(List), "\n")};
    {ScopeArgs, lists:reverse(Functions), lists:reverse(Names), lists:reverse(List)};

parse_decode_erl_loop([#meta{name = Name, type = binary, explain = Length} | T], Protocol, Depth, ScopeArgs, Functions, Names, List) ->
    Code = lists:concat([
       "    ", "<<", Name, ":", Length, "/binary, _", Name, "Rest_/binary>> = _", ScopeArgs, "Rest_,"
    ]),
    parse_decode_erl_loop(T, Protocol, Depth, Name, Functions, [Name | Names], [Code | List]);

parse_decode_erl_loop([#meta{name = Name, type = bool, explain = []} | T], Protocol, Depth, ScopeArgs, Functions, Names, List) ->
    Code = lists:concat([
        "    ", "<<", Name, "Flag:8, _", Name, "Rest_/binary>> = _", ScopeArgs, "Rest_,", "\n",
        "    ", Name, " = type:to_boolean(", Name, "Flag),"
    ]),

    parse_decode_erl_loop(T, Protocol, Depth, Name, Functions, [Name | Names], [Code | List]);

parse_decode_erl_loop([#meta{name = Name, type = u8, explain = []} | T], Protocol, Depth, ScopeArgs, Functions, Names, List) ->
    Code = lists:concat([
        "    ", "<<", Name, ":8, _", Name, "Rest_/binary>> = _", ScopeArgs, "Rest_,"
    ]),

    parse_decode_erl_loop(T, Protocol, Depth, Name, Functions, [Name | Names], [Code | List]);

parse_decode_erl_loop([#meta{name = Name, type = u16, explain = []} | T], Protocol, Depth, ScopeArgs, Functions, Names, List) ->
    Code = lists:concat([
        "    ", "<<", Name, ":16, _", Name, "Rest_/binary>> = _", ScopeArgs, "Rest_,"
    ]),

    parse_decode_erl_loop(T, Protocol, Depth, Name, Functions, [Name | Names], [Code | List]);

parse_decode_erl_loop([#meta{name = Name, type = u32, explain = []} | T], Protocol, Depth, ScopeArgs, Functions, Names, List) ->
    Code = lists:concat([
        "    ", "<<", Name, ":32, _", Name, "Rest_/binary>> = _", ScopeArgs, "Rest_,"
    ]),

    parse_decode_erl_loop(T, Protocol, Depth, Name, Functions, [Name | Names], [Code | List]);

parse_decode_erl_loop([#meta{name = Name, type = u64, explain = []} | T], Protocol, Depth, ScopeArgs, Functions, Names, List) ->
    Code = lists:concat([
        "    ", "<<", Name, ":64, _", Name, "Rest_/binary>> = _", ScopeArgs, "Rest_,"
    ]),

    parse_decode_erl_loop(T, Protocol, Depth, Name, Functions, [Name | Names], [Code | List]);

parse_decode_erl_loop([#meta{name = Name, type = i8, explain = []} | T], Protocol, Depth, ScopeArgs, Functions, Names, List) ->
    Code = lists:concat([
        "    ", "<<", Name, ":8/signed, _", Name, "Rest_/binary>> = _", ScopeArgs, "Rest_,"
    ]),

    parse_decode_erl_loop(T, Protocol, Depth, Name, Functions, [Name | Names], [Code | List]);

parse_decode_erl_loop([#meta{name = Name, type = i16, explain = []} | T], Protocol, Depth, ScopeArgs, Functions, Names, List) ->
    Code = lists:concat([
        "    ", "<<", Name, ":16/signed, _", Name, "Rest_/binary>> = _", ScopeArgs, "Rest_,"
    ]),

    parse_decode_erl_loop(T, Protocol, Depth, Name, Functions, [Name | Names], [Code | List]);

parse_decode_erl_loop([#meta{name = Name, type = i32, explain = []} | T], Protocol, Depth, ScopeArgs, Functions, Names, List) ->
    Code = lists:concat([
        "    ", "<<", Name, ":32/signed, _", Name, "Rest_/binary>> = _", ScopeArgs, "Rest_,"
    ]),

    parse_decode_erl_loop(T, Protocol, Depth, Name, Functions, [Name | Names], [Code | List]);

parse_decode_erl_loop([#meta{name = Name, type = i64, explain = []} | T], Protocol, Depth, ScopeArgs, Functions, Names, List) ->
    Code = lists:concat([
        "    ", "<<", Name, ":64/signed, _", Name, "Rest_/binary>> = _", ScopeArgs, "Rest_,"
    ]),

    parse_decode_erl_loop(T, Protocol, Depth, Name, Functions, [Name | Names], [Code | List]);

parse_decode_erl_loop([#meta{name = Name, type = f32, explain = []} | T], Protocol, Depth, ScopeArgs, Functions, Names, List) ->
    Code = lists:concat([
        "    ", "<<", Name, ":32/float, _", Name, "Rest_/binary>> = _", ScopeArgs, "Rest_,"
    ]),

    parse_decode_erl_loop(T, Protocol, Depth, Name, Functions, [Name | Names], [Code | List]);

parse_decode_erl_loop([#meta{name = Name, type = f64, explain = []} | T], Protocol, Depth, ScopeArgs, Functions, Names, List) ->
    Code = lists:concat([
        "    ", "<<", Name, ":64/float, _", Name, "Rest_/binary>> = _", ScopeArgs, "Rest_,"
    ]),

    parse_decode_erl_loop(T, Protocol, Depth, Name, Functions, [Name | Names], [Code | List]);

parse_decode_erl_loop([#meta{name = Name, type = str, explain = []} | T], Protocol, Depth, ScopeArgs, Functions, Names, List) ->
    Code = lists:concat([
        "    ", "<<", Name, "BinaryLength:16, ", Name, "Binary:", Name, "BinaryLength/binary, _", Name, "Rest_/binary>> = _", ScopeArgs, "Rest_,", "\n",
        "    ", Name, " = unicode:characters_to_list(", Name, "Binary),"
    ]),

    parse_decode_erl_loop(T, Protocol, Depth, Name, Functions, [Name | Names], [Code | List]);

parse_decode_erl_loop([#meta{name = Name, type = bst, explain = []} | T], Protocol, Depth, ScopeArgs, Functions, Names, List) ->
    Code = lists:concat([
        "    ", "<<", Name, "Length:16, ", Name, ":", Name, "Length/binary, _", Name, "Rest_/binary>> = _", ScopeArgs, "Rest_,"
    ]),

    parse_decode_erl_loop(T, Protocol, Depth, Name, Functions, [Name | Names], [Code | List]);

parse_decode_erl_loop([#meta{name = Name, type = rst, explain = []} | T], Protocol, Depth, ScopeArgs, Functions, Names, List) ->
    Code = lists:concat([
        "    ", "<<", Name, "Length:16, ", Name, ":", Name, "Length/binary, _", Name, "Rest_/binary>> = _", ScopeArgs, "Rest_,"
    ]),

    %% stacked
    parse_decode_erl_loop(T, Protocol, Depth, Name, Functions, [Name | Names], [Code | List]);

parse_decode_erl_loop([#meta{name = _, type = tuple, explain = Explain} | T], Protocol, Depth, ScopeArgs, Functions, Names, List) ->
    %% recursive
    {NextScopeArgs, SubFunctions, SubNames, SubCodes} = parse_decode_erl_loop(Explain, Protocol, Depth, ScopeArgs, [], [], []),

    SubName = lists:concat([
        "{", string:join(SubNames, ", "), "}"
    ]),

    SubCode = string:join(SubCodes, "\n"),

    %% stacked
    parse_decode_erl_loop(T, Protocol, Depth, NextScopeArgs, lists:append(SubFunctions, Functions), [SubName | Names], [SubCode | List]);

parse_decode_erl_loop([#meta{name = Name, type = record, explain = Explain} | T], Protocol, Depth, ScopeArgs, Functions, Names, List) ->
    %% recursive
    {NextScopeArgs, SubFunctions, _, SubCodes} = parse_decode_erl_loop(Explain, Protocol, Depth, ScopeArgs, [], [], []),

    SubName = lists:concat([
        "#", word:to_snake(Name), "{", string:join([lists:concat([word:to_snake(FieldName), " = ", FieldName]) || #meta{name = FieldName} <- Explain], ", "), "}"
    ]),

    SubCode = string:join(SubCodes, "\n"),

    parse_decode_erl_loop(T, Protocol, Depth, NextScopeArgs, lists:append(SubFunctions, Functions), [SubName | Names], [SubCode | List]);

parse_decode_erl_loop([#meta{name = Name, type = list, explain = Explain = [#meta{explain = SubExplain}], key = Key} | T], Protocol, Depth, ScopeArgs, Functions, Names, List) ->
    KeyField = lists:keyfind(Key, #meta.name, SubExplain),
    Key =/= undefined andalso KeyField == undefined andalso erlang:throw(lists:flatten(io_lib:format("Cound not found field ~ts in explain", [Key]))),

    %% recursive
    {NextScopeArgs, SubFunctions, SubNames, SubCodes} = parse_decode_erl_loop(Explain, Protocol, Depth + 1, "", [], [], []),

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

    parse_decode_erl_loop(T, Protocol, Depth, Name, [SubFunction | Functions], [Name | Names], [Code | List]).

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
    {Functions, Names, Codes} = parse_encode_erl_loop(Meta, Protocol, 1, [], [], []),

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
    {Code, string:join(Functions, "\n")}.

parse_encode_erl_loop([], _, _, Functions, Names, List) ->
    %% construct as a list
    %% {string:join(lists:reverse(Functions), "\n"), string:join(lists:reverse(Names), ", "), string:join(lists:reverse(List), ", ")};
    {lists:reverse(Functions), lists:reverse(Names), lists:reverse(List)};

parse_encode_erl_loop([#meta{name = _, type = zero, explain = []} | T], Protocol, Depth, Functions, Names, List) ->
    Name = "_",
    parse_encode_erl_loop(T, Protocol, Depth, Functions, [Name | Names], List);

parse_encode_erl_loop([#meta{name = Name, type = binary, explain = Explain} | T], Protocol, Depth, Functions, Names, List) ->
    Code = io_lib:format("~s:~tp/binary", [Name, Explain]),
    parse_encode_erl_loop(T, Protocol, Depth, Functions, [Name | Names], [Code | List]);

parse_encode_erl_loop([#meta{name = Name, type = bool, explain = []} | T], Protocol, Depth, Functions, Names, List) ->
    Code = io_lib:format("(type:to_flag(~s)):8", [Name]),
    parse_encode_erl_loop(T, Protocol, Depth, Functions, [Name | Names], [Code | List]);

parse_encode_erl_loop([#meta{name = Name, type = u8, explain = []} | T], Protocol, Depth, Functions, Names, List) ->
    Code = io_lib:format("~s:8", [Name]),
    parse_encode_erl_loop(T, Protocol, Depth, Functions, [Name | Names], [Code | List]);

parse_encode_erl_loop([#meta{name = Name, type = u16, explain = []} | T], Protocol, Depth, Functions, Names, List) ->
    Code = io_lib:format("~s:16", [Name]),
    parse_encode_erl_loop(T, Protocol, Depth, Functions, [Name | Names], [Code | List]);

parse_encode_erl_loop([#meta{name = Name, type = u32, explain = []} | T], Protocol, Depth, Functions, Names, List) ->
    Code = io_lib:format("~s:32", [Name]),
    parse_encode_erl_loop(T, Protocol, Depth, Functions, [Name | Names], [Code | List]);

parse_encode_erl_loop([#meta{name = Name, type = u64, explain = []} | T], Protocol, Depth, Functions, Names, List) ->
    Code = io_lib:format("~s:64", [Name]),
    parse_encode_erl_loop(T, Protocol, Depth, Functions, [Name | Names], [Code | List]);

parse_encode_erl_loop([#meta{name = Name, type = i8, explain = []} | T], Protocol, Depth, Functions, Names, List) ->
    Code = io_lib:format("~s:8/signed", [Name]),
    parse_encode_erl_loop(T, Protocol, Depth, Functions, [Name | Names], [Code | List]);

parse_encode_erl_loop([#meta{name = Name, type = i16, explain = []} | T], Protocol, Depth, Functions, Names, List) ->
    Code = io_lib:format("~s:16/signed", [Name]),
    parse_encode_erl_loop(T, Protocol, Depth, Functions, [Name | Names], [Code | List]);

parse_encode_erl_loop([#meta{name = Name, type = i32, explain = []} | T], Protocol, Depth, Functions, Names, List) ->
    Code = io_lib:format("~s:32/signed", [Name]),
    parse_encode_erl_loop(T, Protocol, Depth, Functions, [Name | Names], [Code | List]);

parse_encode_erl_loop([#meta{name = Name, type = i64, explain = []} | T], Protocol, Depth, Functions, Names, List) ->
    Code = io_lib:format("~s:64/signed", [Name]),
    parse_encode_erl_loop(T, Protocol, Depth, Functions, [Name | Names], [Code | List]);

parse_encode_erl_loop([#meta{name = Name, type = f32, explain = []} | T], Protocol, Depth, Functions, Names, List) ->
    Code = io_lib:format("~s:32/float", [Name]),
    parse_encode_erl_loop(T, Protocol, Depth, Functions, [Name | Names], [Code | List]);

parse_encode_erl_loop([#meta{name = Name, type = f64, explain = []} | T], Protocol, Depth, Functions, Names, List) ->
    Code = io_lib:format("~s:64/float", [Name]),
    parse_encode_erl_loop(T, Protocol, Depth, Functions, [Name | Names], [Code | List]);

parse_encode_erl_loop([#meta{name = Name, type = str, explain = []} | T], Protocol, Depth, Functions, Names, List) ->
    Code = io_lib:format("(begin ~sBinary = unicode:characters_to_binary(~s), <<(byte_size(~sBinary)):16, ~sBinary/binary>> end)/binary", [Name, Name, Name, Name]),
    parse_encode_erl_loop(T, Protocol, Depth, Functions, [Name | Names], [Code | List]);

parse_encode_erl_loop([#meta{name = Name, type = bst, explain = []} | T], Protocol, Depth, Functions, Names, List) ->
    Code = io_lib:format("(byte_size(~s)):16, (~s)/binary", [Name, Name]),
    parse_encode_erl_loop(T, Protocol, Depth, Functions, [Name | Names], [Code | List]);

parse_encode_erl_loop([#meta{name = Name, type = rst, explain = []} | T], Protocol, Depth, Functions, Names, List) ->
    Code = io_lib:format("(protocol:text(~s))/binary", [Name]),
    parse_encode_erl_loop(T, Protocol, Depth, Functions, [Name | Names], [Code | List]);

parse_encode_erl_loop([#meta{name = _, type = tuple, explain = Explain} | T], Protocol, Depth, Functions, Names, List) ->
    %% recursive
    {SubFunctions, SubNames, SubCodes} = parse_encode_erl_loop(Explain, Protocol, Depth, [], [], []),

    SubName = lists:concat([
        "{", string:join(SubNames, ", "), "}"
    ]),

    SubCode = string:join(SubCodes, ", "),

    %% stacked
    parse_encode_erl_loop(T, Protocol, Depth, lists:append(SubFunctions, Functions), [SubName | Names], [SubCode | List]);

parse_encode_erl_loop([#meta{name = Name, type = record, explain = Explain, key = KeyNameList} | T], Protocol, Depth, Functions, Names, List) ->
    %% recursive
    {SubFunctions, SubNames, SubCodes} = parse_encode_erl_loop(Explain, Protocol, Depth, [], [], []),

    SubName = lists:concat([
        "#", word:to_snake(Name), "{", string:join([lists:concat([word:to_snake(SubKey), " = ", SubName]) || {SubKey, SubName} <- lists:zip(KeyNameList, SubNames)], ", "), "}"
    ]),

    SubCode = string:join(SubCodes, ", "),

    %% stacked
    parse_encode_erl_loop(T, Protocol, Depth, lists:append(SubFunctions, Functions), [SubName | Names], [SubCode | List]);

parse_encode_erl_loop([#meta{name = Name, type = list, explain = Explain = [#meta{explain = SubExplain}], key = Key} | T], Protocol, Depth, Functions, Names, List) ->
    KeyField = lists:keyfind(Key, #meta.name, SubExplain),
    Key =/= undefined andalso KeyField == undefined andalso erlang:throw(lists:flatten(io_lib:format("Cound not found field ~ts in explain", [Key]))),

    %% recursive
    {SubFunctions, SubNames, SubCodes} = parse_encode_erl_loop(Explain, Protocol, Depth + 1, [], [], []),

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

    parse_encode_erl_loop(T, Protocol, Depth, [SubFunction | Functions], [Name | Names], [Code | List]);

parse_encode_erl_loop([#meta{name = Name, type = ets, explain = Explain = [#meta{explain = SubExplain}], key = Key} | T], Protocol, Depth, Functions, Names, List) ->
    KeyField = lists:keyfind(Key, #meta.name, SubExplain),
    Key =/= undefined andalso KeyField == undefined andalso erlang:throw(lists:flatten(io_lib:format("Cound not found field ~ts in explain", [Key]))),

    %% recursive
    {SubFunctions, SubNames, SubCodes} = parse_encode_erl_loop(Explain, Protocol, Depth + 1, [], [], []),

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

    parse_encode_erl_loop(T, Protocol, Depth, [SubFunction | Functions], [Name | Names], [Code | List]).
