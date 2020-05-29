%%%-------------------------------------------------------------------
%%% @doc
%%% module protocol maker
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_maker).
-export([start/1]).
-export([extract/1, restore/1]).
-include("serialize.hrl").
%% syntax field record
-record(field,    {name = [], meta = [], args = [], procedure = [], packs = []}).
%% ast metadata
-record(meta,     {name = [], type, explain = [], comment = []}).
%% lang code
-record(code,     {default_handler = [], handler = [], text = [], erl = [], lua = [], js = []}).
%%%===================================================================
%%% API functions
%%%===================================================================
start(List) ->
    lists:foreach(fun parse/1, List).

%%%===================================================================
%%% Parse
%%%===================================================================
parse(#protocol{io = IO, includes = Includes, erl = ErlFile, js = JsFile, lua = LuaFile, handler = HandlerFile}) ->
    %% start collect code
    #code{handler = HandlerCode, erl = ErlCode, js = JsCode, lua = LuaCode} = collect_code(IO, [], []),
    %% result error text data file
    %% ResultFileName = list_to_binary(filename:basename(ResultFile, ".erl")),
    %% write default if file not exists
    %% _ = filelib:is_file(maker:prim_script_path() ++ ResultFile) == false andalso file:write_file(maker:prim_script_path() ++ ResultFile, <<"-module(", ResultFileName/binary, ").\n-compile(nowarn_export_all).\n-compile(export_all).get(_, _) ->\n    <<0:16>>.">>) == ok,
    %% read file data success always
    %% {ok, ResultFileBinary} = file:read_file(maker:prim_script_path() ++ ResultFile),
    %% parse the file and extract get text code
    %% [ModuleCode, NoWarmCode, ExportCode, GetCode | _] = string:tokens(binary_to_list(ResultFileBinary), "."),
    %% remove old code and revise format
    %% [DefaultCode | TextCode] = lists:reverse([string:strip(Column, both, $\n) || Column <- string:tokens(string:strip(GetCode, both, $\n), ";"), string:str(Column, lists:concat(["get(", Name])) =:= 0]),
    %% add new code and resort and make new code format
    %% ResultCodeSet = lists:concat(["\n\n\n", string:join(lists:sort(ResultCode ++ TextCode) ++ [DefaultCode], ";\n"), ".", "\n\n"]),
    %% restore other code
    %% ResultFileData = string:join([ModuleCode, NoWarmCode, ExportCode, ResultCodeSet], "."),
    %% write result text file
    %% file:write_file(maker:prim_script_path() ++ ResultFile, ResultFileData),
    %% handler code
    HandlerData = lists:concat(["-module(", filename:basename(HandlerFile, ".erl"), ").\n-export([handle/3]).\n\n", HandlerCode]),
    file:write_file(maker:relative_path(HandlerFile), HandlerData),
    %% names
    ErlName = filename:basename(ErlFile, ".erl"),
    %% erl file
    IncludeCode = [io_lib:format("-include(\"~s\").\n", [Include]) || Include <- Includes],
    ErlData = io_lib:format("-module(~s).\n-export([read/2, write/2]).\n~s~s", [ErlName, IncludeCode, ErlCode]),
    file:write_file(maker:relative_path(ErlFile), ErlData),
    %% js code (file cannot write when parameter not given)
    JsName = word:to_lower_hump(filename:basename(JsFile, ".js")),
    JsData = lists:concat(["const ", JsName, " = ", JsCode, ";"]),
    file:write_file(maker:relative_path(JsFile), JsData),
    %% lua code (file cannot write when parameter not given)
    LuaName = word:to_lower_hump(filename:basename(LuaFile, ".lua")),
    LuaData = lists:concat(["local ", LuaName, " = ", LuaCode]),
    file:write_file(maker:relative_path(LuaFile), LuaData),
    ok.

%% collect code
collect_code([], ReadList, WriteList) ->
    %% handler code
    DefaultHandler = tool:default(lists:reverse(listing:collect(#code.default_handler, ReadList, [])), "handle(Protocol, _, Data) ->\n    {error, Protocol, Data}.\n"),
    Handler = lists:concat([lists:reverse(listing:collect(#code.handler, ReadList, [])), DefaultHandler]),
    %% result text code
    %% Result = lists:append(listing:collect(#code.result, WriteList, [])),
    %% collect all text into protocol file, no text if not set
    Text = case lists:sort(lists:append(listing:collect(#code.text, WriteList, []))) of [] -> []; SortText -> "\n\n" ++ "text(_, ok) ->\n    <<0:16>>;\n" ++ "text(Protocol, Reason) ->\n    text(Protocol, Reason, parameter_data:get(language))." ++ "\n\n" ++ string:join(SortText ++ ["text(_, Reason, _) ->\n    protocol:write_bit_string(type:to_binary(Reason))"], ";\n") ++ ".\n\n" end,
    %% erl code
    ErlRead = lists:reverse(listing:collect(#code.erl, ReadList, [])),
    ErlWrite = lists:reverse(listing:collect(#code.erl, WriteList, [])),
    Erl = lists:concat(["\n\n", ErlRead, "read(Code, Binary) ->\n    {error, Code, Binary}.\n\n", "\n\n", ErlWrite, "write(Code, Content) ->\n    {error, Code, Content}.\n\n", Text]),
    %% js metadata, name test_protocol -> testProtocol
    JsRead = string:join(lists:reverse(listing:collect(#code.js, ReadList, [])), ",\n"),
    JsWrite = string:join(lists:reverse(listing:collect(#code.js, WriteList, [])), ",\n"),
    %% reverse read and write code
    Js = lists:concat(["{\n    \"write\" : {\n", JsRead, "\n    },\n    \"read\" : {\n", JsWrite, "\n    }\n}"]),
    %% lua metadata, name test_protocol -> testProtocol
    LuaRead = string:join(lists:reverse(listing:collect(#code.lua, ReadList, [])), ",\n"),
    LuaWrite = string:join(lists:reverse(listing:collect(#code.lua, WriteList, [])), ",\n"),
    %% reverse read and write code
    Lua = lists:concat(["{\n    [\"write\"] = {\n", LuaWrite, "\n    },\n    [\"read\"] = {\n", LuaRead, "\n    }\n}"]),
    %% return code sets
    #code{erl = Erl, lua = Lua, js = Js, handler = Handler};
collect_code([#io{read = Read, write = Write, handler = Handler, text = Text, translate = Translate, protocol = Protocol} | T], ReadList, WriteList) ->
    ReadCode = parse_read(Protocol, Read, Handler),
    WriteCode = parse_write(Protocol, Write, Text ++ Translate),
    collect_code(T, [ReadCode | ReadList], [WriteCode | WriteList]).

%%%===================================================================
%%% Parse Js Code Part
%%%===================================================================
%% js code
parse_meta_js(Protocol, Meta) ->
    %% start with 3 tabs(4 space) padding
    Result = parse_meta_js_loop(Meta, 3, []),
    %% format a protocol define
    lists:concat(["        \"", Protocol, "\" : [\n", Result, "\n        ]"]).

parse_meta_js_loop([], _, List) ->
    %% construct as a list
    string:join(lists:reverse(List), ",\n");
parse_meta_js_loop([#meta{name = Name, type = binary, explain = Length, comment = Comment} | T], Depth, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    String = lists:flatten(lists:concat([Padding, "{\"name\" : \"", word:to_lower_hump(Name), "\", \"type\" : \"", binary, "\", \"comment\" : \"", encoding:to_list(Comment), "\", \"explain\" : \"", Length, "\"}"])),
    parse_meta_js_loop(T, Depth, [String | List]);
parse_meta_js_loop([#meta{name = Name, type = Type, explain = [], comment = Comment} | T], Depth, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    String = lists:flatten(lists:concat([Padding, "{\"name\" : \"", word:to_lower_hump(Name), "\", \"type\" : \"", Type, "\", \"comment\" : \"", encoding:to_list(Comment), "\", \"explain\" : []}"])),
    parse_meta_js_loop(T, Depth, [String | List]);
parse_meta_js_loop([#meta{name = Name, type = Type, explain = Explain = [_ | _], comment = Comment} | T], Depth, List) ->
    %% recurse
    Result = parse_meta_js_loop(Explain, Depth + 1, []),
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format a field
    String = lists:concat([Padding, "{\"name\" : \"", word:to_lower_hump(Name), "\", \"type\" : \"", Type, "\", \"comment\" : \"", encoding:to_list(Comment), "\", \"explain\" : [\n", Result, "\n", Padding, "]}"]),
    parse_meta_js_loop(T, Depth, [String | List]).

%%%===================================================================
%%% Parse Lua Code Part
%%%===================================================================
%% lua code
parse_meta_lua(Protocol, Meta) ->
    %% start with 3 tabs(4 space) padding
    Result = parse_meta_lua_loop(Meta, 3, []),
    %% format a protocol define
    lists:concat(["        [", Protocol, "] = {\n", Result, "\n        }"]).

parse_meta_lua_loop([], _, List) ->
    %% construct as a list
    string:join(lists:reverse(List), ",\n");
parse_meta_lua_loop([#meta{name = Name, type = binary, explain = Length, comment = Comment} | T], Depth, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format a field
    String = lists:flatten(lists:concat([Padding, "{name = \"", word:to_lower_hump(Name), "\", type = \"", binary, "\", comment = \"", encoding:to_list(Comment), "\", explain = ", Length, "}"])),
    parse_meta_lua_loop(T, Depth, [String | List]);
parse_meta_lua_loop([#meta{name = Name, type = Type, explain = [], comment = Comment} | T], Depth, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format a field
    String = lists:flatten(lists:concat([Padding, "{name = \"", word:to_lower_hump(Name), "\", type = \"", Type, "\", comment = \"", encoding:to_list(Comment), "\", explain = {}}"])),
    parse_meta_lua_loop(T, Depth, [String | List]);
parse_meta_lua_loop([#meta{name = Name, type = Type, explain = Explain = [_ | _], comment = Comment} | T], Depth, List) ->
    %% recurse
    Result = parse_meta_lua_loop(Explain, Depth + 1, []),
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    String = lists:concat([Padding, "{name = \"", word:to_lower_hump(Name), "\", type = \"", Type, "\", comment = \"", encoding:to_list(Comment), "\", explain = {\n", Result, "\n", Padding, "}}"]),
    parse_meta_lua_loop(T, Depth, [String | List]).

%%%===================================================================
%%% Parse Read Part
%%%===================================================================
parse_read(Protocol, SyntaxList, undefined) ->
    %% no handler
    Code = parse_read(Protocol, SyntaxList, #handler{}),
    Code#code{handler = [], default_handler = []};
parse_read(Protocol, SyntaxList, Handler = #handler{protocol = 0}) ->
    %% no handler
    parse_read(Protocol, SyntaxList, Handler#handler{protocol = []});
parse_read(Protocol, SyntaxList, Handler = #handler{protocol = ProtocolArg}) when not is_list(ProtocolArg) ->
    %% no handler
    parse_read(Protocol, SyntaxList, Handler#handler{protocol = integer_to_list(Protocol)});
parse_read(0, _, #handler{module = Module, function = Function, arg = Arg, protocol = ProtocolArg}) ->
    %% default handler code
    HandlerArgs = string:join([word:to_hump(A) || A <-  [Arg, ProtocolArg, "Data"], A =/= []], ", "),
    HandlerCode = lists:concat(["handle(", "_", ", ", tool:default(word:to_hump(Arg), "_"), ", ", "Data", ") ->\n    ", Module, ":", Function, "(", HandlerArgs, ").\n"]),
    #code{erl = [], js = [], lua = [], handler = [], default_handler = HandlerCode};
parse_read(Protocol, [], #handler{module = Module, function = Function, arg = Arg, protocol = ProtocolArg}) ->
    %% erl code
    ErlCode = "read(" ++ integer_to_list(Protocol) ++ ", <<>>) ->\n    {ok, []};\n\n",
    JsCode = lists:concat(["        \"", Protocol, "\" : ", "[]"]),
    LuaCode = lists:concat(["        [", Protocol, "] = ", "{}"]),
    %% handler code
    HandlerArgs = string:join([word:to_hump(A) || A <- [Arg, ProtocolArg], A =/= []], ", "),
    HandlerCode = lists:concat(["handle(", Protocol, ", ", tool:default(word:to_hump(Arg), "_"), ", [", "]) ->\n    ", Module, ":", Function, "(", HandlerArgs, ");\n\n"]),
    #code{erl = ErlCode, js = JsCode, lua = LuaCode, handler = HandlerCode};
parse_read(Protocol, SyntaxList = [_ | _], #handler{module = Module, function = Function, arg = Arg, protocol = ProtocolArg}) ->
    List = [parse_read_unit(Syntax) || Syntax <- SyntaxList],
    %% collect code args
    ArgList = listing:collect_into(#field.args, List, fun(X) -> lists:flatten(X) end),
    %% ArgListCode = string:join(ArgList, ", "),
    %% string type convert binary_to_list args revise
    %% HandlerArgListCode = string:join(HandlerArgList, ", "),
    %% construct erl handler code
    HandlerArgs = string:join([word:to_hump(A) || A <- [Arg, ProtocolArg | ArgList], A =/= []], ", "),
    HandlerCode = lists:concat(["handle(", Protocol, ", ", tool:default(word:to_hump(Arg), "_"), ", ", join(ArgList), ") ->\n    ", Module, ":", Function, "(", HandlerArgs, ");\n\n"]),
    %% construct erl code
    Procedure = ["\n    " ++ Procedure ++ "," || #field{procedure = Procedure} <- List, Procedure =/=[]],
    Packs = string:join(listing:collect(#field.packs, List), ", "),
    ErlCode = lists:concat(["read(", Protocol, ", <<", Packs, ">>) ->", Procedure, "\n    {ok, ", join(ArgList), "};\n\n"]),
    %% collect unit meta
    MetaList = lists:flatten(listing:collect(#field.meta, List)),
    %% construct js/lua code
    JsCode = parse_meta_js(Protocol, MetaList),
    LuaCode = parse_meta_lua(Protocol, MetaList),
    #code{erl = ErlCode, js = JsCode, lua = LuaCode, handler = HandlerCode};
parse_read(_, _, _) ->
    #code{erl = [], js = [], lua = [], handler = []}.

%% parse unit
parse_read_unit(Unit = #binary{name = Name, default = Default, explain = Explain, comment = Comment}) ->
    SourceName = tool:default(Name, Default),
    HumpName = word:to_hump(SourceName),
    PackName = tool:default(Default, Name),
    Args = io_lib:format("~s", [HumpName]),
    Length =  Explain * 8,
    Packs = io_lib:format("~s:~w", [HumpName, Length]),
    #field{name = PackName, meta = #meta{name = SourceName, type = element(1, Unit), explain = Length, comment = Comment}, args = Args, packs = Packs};
parse_read_unit(Unit = #bool{name = Name, default = Default, comment = Comment}) ->
    SourceName = tool:default(Name, Default),
    HumpName = word:to_hump(SourceName),
    PackName = tool:default(Default, Name),
    Args = io_lib:format("type:to_boolean(~s)", [HumpName]),
    Packs = io_lib:format("~s:8", [HumpName]),
    #field{name = PackName, meta = #meta{name = SourceName, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_read_unit(Unit = #u8{name = Name, default = Default, comment = Comment}) ->
    SourceName = tool:default(Name, Default),
    HumpName = word:to_hump(SourceName),
    PackName = tool:default(Default, Name),
    Args = io_lib:format("~s", [HumpName]),
    Packs = io_lib:format("~s:8", [HumpName]),
    #field{name = PackName, meta = #meta{name = SourceName, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_read_unit(Unit = #u16{name = Name, default = Default, comment = Comment}) ->
    SourceName = tool:default(Name, Default),
    HumpName = word:to_hump(SourceName),
    PackName = tool:default(Default, Name),
    Args = io_lib:format("~s", [HumpName]),
    Packs = io_lib:format("~s:16", [HumpName]),
    #field{name = PackName, meta = #meta{name = SourceName, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_read_unit(Unit = #u32{name = Name, default = Default, comment = Comment}) ->
    SourceName = tool:default(Name, Default),
    HumpName = word:to_hump(SourceName),
    PackName = tool:default(Default, Name),
    Args = io_lib:format("~s", [HumpName]),
    Packs = io_lib:format("~s:32", [HumpName]),
    #field{name = PackName, meta = #meta{name = SourceName, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_read_unit(Unit = #u64{name = Name, default = Default, comment = Comment}) ->
    SourceName = tool:default(Name, Default),
    HumpName = word:to_hump(SourceName),
    PackName = tool:default(Default, Name),
    Args = io_lib:format("~s", [HumpName]),
    Packs = io_lib:format("~s:64", [HumpName]),
    #field{name = PackName, meta = #meta{name = SourceName, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_read_unit(Unit = #u128{name = Name, default = Default, comment = Comment}) ->
    SourceName = tool:default(Name, Default),
    HumpName = word:to_hump(SourceName),
    PackName = tool:default(Default, Name),
    Args = io_lib:format("~s", [HumpName]),
    Packs = io_lib:format("~s:128", [HumpName]),
    #field{name = PackName, meta = #meta{name = SourceName, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_read_unit(Unit = #i8{name = Name, default = Default, comment = Comment}) ->
    SourceName = tool:default(Name, Default),
    HumpName = word:to_hump(SourceName),
    PackName = tool:default(Default, Name),
    Args = io_lib:format("~s", [HumpName]),
    Packs = io_lib:format("~s:8/signed", [HumpName]),
    #field{name = PackName, meta = #meta{name = SourceName, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_read_unit(Unit = #i16{name = Name, default = Default, comment = Comment}) ->
    SourceName = tool:default(Name, Default),
    HumpName = word:to_hump(SourceName),
    PackName = tool:default(Default, Name),
    Args = io_lib:format("~s", [HumpName]),
    Packs = io_lib:format("~s:16/signed", [HumpName]),
    #field{name = PackName, meta = #meta{name = SourceName, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_read_unit(Unit = #i32{name = Name, default = Default, comment = Comment}) ->
    SourceName = tool:default(Name, Default),
    HumpName = word:to_hump(SourceName),
    PackName = tool:default(Default, Name),
    Args = io_lib:format("~s", [HumpName]),
    Packs = io_lib:format("~s:32/signed", [HumpName]),
    #field{name = PackName, meta = #meta{name = SourceName, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_read_unit(Unit = #i64{name = Name, default = Default, comment = Comment}) ->
    SourceName = tool:default(Name, Default),
    HumpName = word:to_hump(SourceName),
    PackName = tool:default(Default, Name),
    Args = io_lib:format("~s", [HumpName]),
    Packs = io_lib:format("~s:64/signed", [HumpName]),
    #field{name = PackName, meta = #meta{name = SourceName, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_read_unit(Unit = #i128{name = Name, default = Default, comment = Comment}) ->
    SourceName = tool:default(Name, Default),
    HumpName = word:to_hump(SourceName),
    PackName = tool:default(Default, Name),
    Args = io_lib:format("~s", [HumpName]),
    Packs = io_lib:format("~s:128/signed", [HumpName]),
    #field{name = PackName, meta = #meta{name = SourceName, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_read_unit(Unit = #bst{name = Name, default = Default, comment = Comment}) ->
    SourceName = tool:default(Name, Default),
    HumpName = word:to_hump(SourceName),
    PackName = tool:default(Default, Name),
    Args = io_lib:format("~s", [HumpName]),
    Packs = io_lib:format("~s:16, ~s:~s/binary", [HumpName ++ "Length", HumpName, HumpName ++ "Length"]),
    #field{name = PackName, meta = #meta{name = SourceName, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_read_unit(Unit = #str{name = Name, default = Default, comment = Comment}) ->
    SourceName = tool:default(Name, Default),
    HumpName = word:to_hump(SourceName),
    PackName = tool:default(Default, Name),
    Args = io_lib:format("~sString", [HumpName]),
    Procedure = io_lib:format("~s = binary_to_list(~s)", [Args, HumpName]),
    Packs = io_lib:format("~s:16, ~s:~s/binary", [HumpName ++ "Length", HumpName, HumpName ++ "Length"]),
    #field{name = PackName, procedure = Procedure, meta = #meta{name = SourceName, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};

%% structure unit
parse_read_unit(#tuple{name = Name, default = Default, explain = Explain}) ->
    %% format per unit
    List = [parse_read_unit(Field) || Field <- tuple_to_list(Explain)],
    %% format function match args
    Args = lists:concat(["{", string:join([Args || #field{args = Args} <- List], ", "), "}"]),
    %% format function pack info
    Packs = string:join(listing:collect(#field.packs, List, []), ", "),
    #field{name = tool:default(Name, Default), args = Args, packs = Packs, meta = listing:collect(#field.meta, List, [])};

parse_read_unit(#list{name = Name, default = Default, explain = Explain, comment = Comment}) ->
    %% hump name is unpack bit variable bind
    SourceName = tool:default(Name, Default),
    HumpName = word:to_hump(SourceName),
    %% format subunit
    #field{args = Args, packs = Packs, meta = Meta} = parse_read_unit(Explain),
    %% meta may be list or meta record
    ReviseMeta = lists:flatten([Meta]),
    %% format list pack info
    Procedure = io_lib:format("~s = [~s || <<~s>> <= ~sBinary]", [HumpName, Args, Packs, HumpName]),
    %% read a list cannot contain variable length binary like string/binary
    ListPacks = io_lib:format("~sLength:16, ~sBinary:~sLength/binary-unit:~w", [HumpName, HumpName, HumpName, sum(ReviseMeta)]),
    #field{name = SourceName, args = HumpName, procedure = Procedure, packs = ListPacks, meta = #meta{name = SourceName, type = list, explain = ReviseMeta, comment = Comment}};

parse_read_unit(Record) when is_tuple(Record) andalso is_atom(element(1, Record)) ->
    %% get beam abstract code
    Tag = element(1, Record),
    NameList = beam:find(Tag),
    %% throw error when beam abstract code empty
    NameList =:= [] andalso erlang:error(need_to_update_beam_abstract_code),
    tuple_size(Record) =/= length(NameList) andalso erlang:error(need_to_update_beam_abstract_code),
    %% zip field value and field name
    ZipList = lists:zip(tuple_to_list(Record), NameList),
    %% format per unit
    List = [parse_read_unit(setelement(3, Explain, Name)) || {Explain, Name} <- ZipList, is_unit(Explain)],
    %% format function match param
    Args = lists:concat(["#", Tag, "{", string:join([io_lib:format("~s = ~s", [Names, Args]) || #field{name = Names, args = Args} <- List], ", "), "}"]),
    %% format function pack info
    Packs = string:join(listing:collect(#field.packs, List, []), ", "),
    #field{args = Args, packs = Packs, name = listing:collect(#field.name, List), meta = lists:flatten(listing:collect(#field.meta, List, []))};
parse_read_unit(Tuple) when is_tuple(Tuple) andalso tuple_size(Tuple) > 0 ->
    %% format per unit
    List = [parse_read_unit(Field) || Field <- tuple_to_list(Tuple)],
    %% format function match args
    Args = lists:concat(["{", string:join([Args || #field{args = Args} <- List], ", "), "}"]),
    %% format function pack info
    Packs = string:join(listing:collect(#field.packs, List, []), ", "),
    #field{args = Args, packs = Packs, name = listing:collect(#field.name, List, []), meta = listing:collect(#field.meta, List, [])};
parse_read_unit(_) ->
    #field{}.

%%%===================================================================
%%% Parse Write Part
%%%===================================================================
parse_write(0, _, _) ->
    #code{erl = [], js = [], lua = [], handler = [], text = []};
parse_write(Protocol, [], _) ->
    %% erl code
    ErlCode = "write(" ++ integer_to_list(Protocol) ++ ", []) ->\n    {ok, protocol:pack(" ++ integer_to_list(Protocol) ++ ", <<>>)};\n\n",
    JsCode = lists:concat(["        \"", Protocol, "\" : ", "[]"]),
    LuaCode = lists:concat(["        [", Protocol, "] = ", "{}"]),
    #code{erl = ErlCode, js = JsCode, lua = LuaCode, text = []};
parse_write(Protocol, SyntaxList = [_ | _], TextList) ->
    List = [case Syntax of #rst{} -> parse_write_unit(Syntax#rst{explain = Protocol}); _ -> parse_write_unit(Syntax) end || Syntax <- SyntaxList],
    %% collect code args
    ArgList = listing:collect(#field.args, List),
    %% Args = string:join(ArgList, ", "),
    ReviseText = lists:map(fun({K, V}) -> {K, default_language(), V}; (Other) -> Other end, TextList),
    TextCode = lists:sort([lists:flatten(io_lib:format("text(~w, ~w, ~w) ->\n    <<~w:16, \"~s\"/utf8>>", [Protocol, Reason, Language, element(2, word:byte(String)), encoding:to_list(String)])) || {Reason, Language, String} <- ReviseText]),
    %% construct erl code
    Procedure = ["\n    " ++ Procedure ++ "," || #field{procedure = Procedure} <- List, Procedure =/=[]],
    Packs = string:join(listing:collect(#field.packs, List), ", "),
    ErlCode = lists:concat(["write(", Protocol, ", ", join(ArgList), ") ->", Procedure, "\n    {ok, protocol:pack(", Protocol, ", <<", Packs, ">>)};\n\n"]),
    %% collect unit meta
    MetaList = lists:flatten(listing:collect(#field.meta, List)),
    %% construct js/lua code
    JsCode = parse_meta_js(Protocol, MetaList),
    LuaCode = parse_meta_lua(Protocol, MetaList),
    #code{erl = ErlCode, js = JsCode, lua = LuaCode, text = TextCode};
parse_write(_, _, _) ->
    #code{erl = [], js = [], lua = [], handler = [], text = []}.

%% parse unit
parse_write_unit(#zero{}) ->
    #field{args = '_'};
parse_write_unit(Unit = #binary{name = Name, default = Default, comment = Comment, explain = Explain}) ->
    SourceName = tool:default(Name, Default),
    SourceHumpName = word:to_hump(SourceName),
    PackName = tool:default(Default, Name),
    Length =  Explain * 8,
    Args = io_lib:format("~s", [SourceHumpName]),
    Packs = io_lib:format("~s:~w", [SourceHumpName, Length]),
    #field{name = PackName, meta = #meta{name = SourceName, type = element(1, Unit), explain = Length, comment = Comment}, args = Args, packs = Packs};
parse_write_unit(Unit = #bool{name = Name, default = Default, comment = Comment}) ->
    SourceName = tool:default(Name, Default),
    SourceHumpName = word:to_hump(SourceName),
    PackName = tool:default(Default, Name),
    Args = io_lib:format("~s", [SourceHumpName]),
    Packs = io_lib:format("(type:to_flag(~s)):8", [SourceHumpName]),
    #field{name = PackName, meta = #meta{name = SourceName, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_write_unit(Unit = #u8{name = Name, default = Default, comment = Comment}) ->
    SourceName = tool:default(Name, Default),
    SourceHumpName = word:to_hump(SourceName),
    PackName = tool:default(Default, Name),
    Args = io_lib:format("~s", [SourceHumpName]),
    Packs = io_lib:format("~s:8", [SourceHumpName]),
    #field{name = PackName, meta = #meta{name = SourceName, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_write_unit(Unit = #u16{name = Name, default = Default, comment = Comment}) ->
    SourceName = tool:default(Name, Default),
    SourceHumpName = word:to_hump(SourceName),
    PackName = tool:default(Default, Name),
    Args = io_lib:format("~s", [SourceHumpName]),
    Packs = io_lib:format("~s:16", [SourceHumpName]),
    #field{name = PackName, meta = #meta{name = SourceName, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_write_unit(Unit = #u32{name = Name, default = Default, comment = Comment}) ->
    SourceName = tool:default(Name, Default),
    SourceHumpName = word:to_hump(SourceName),
    PackName = tool:default(Default, Name),
    Args = io_lib:format("~s", [SourceHumpName]),
    Packs = io_lib:format("~s:32", [SourceHumpName]),
    #field{name = PackName, meta = #meta{name = SourceName, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_write_unit(Unit = #u64{name = Name, default = Default, comment = Comment}) ->
    SourceName = tool:default(Name, Default),
    SourceHumpName = word:to_hump(SourceName),
    PackName = tool:default(Default, Name),
    Args = io_lib:format("~s", [SourceHumpName]),
    Packs = io_lib:format("~s:64", [SourceHumpName]),
    #field{name = PackName, meta = #meta{name = SourceName, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_write_unit(Unit = #u128{name = Name, default = Default, comment = Comment}) ->
    SourceName = tool:default(Name, Default),
    SourceHumpName = word:to_hump(SourceName),
    PackName = tool:default(Default, Name),
    Args = io_lib:format("~s", [SourceHumpName]),
    Packs = io_lib:format("~s:128", [SourceHumpName]),
    #field{name = PackName, meta = #meta{name = SourceName, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_write_unit(Unit = #i8{name = Name, default = Default, comment = Comment}) ->
    SourceName = tool:default(Name, Default),
    SourceHumpName = word:to_hump(SourceName),
    PackName = tool:default(Default, Name),
    Args = io_lib:format("~s", [SourceHumpName]),
    Packs = io_lib:format("~s:8/signed", [SourceHumpName]),
    #field{name = PackName, meta = #meta{name = SourceName, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_write_unit(Unit = #i16{name = Name, default = Default, comment = Comment}) ->
    SourceName = tool:default(Name, Default),
    SourceHumpName = word:to_hump(SourceName),
    PackName = tool:default(Default, Name),
    Args = io_lib:format("~s", [SourceHumpName]),
    Packs = io_lib:format("~s:16/signed", [SourceHumpName]),
    #field{name = PackName, meta = #meta{name = SourceName, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_write_unit(Unit = #i32{name = Name, default = Default, comment = Comment}) ->
    SourceName = tool:default(Name, Default),
    SourceHumpName = word:to_hump(SourceName),
    PackName = tool:default(Default, Name),
    Args = io_lib:format("~s", [SourceHumpName]),
    Packs = io_lib:format("~s:32/signed", [SourceHumpName]),
    #field{name = PackName, meta = #meta{name = SourceName, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_write_unit(Unit = #i64{name = Name, default = Default, comment = Comment}) ->
    SourceName = tool:default(Name, Default),
    SourceHumpName = word:to_hump(SourceName),
    PackName = tool:default(Default, Name),
    Args = io_lib:format("~s", [SourceHumpName]),
    Packs = io_lib:format("~s:64/signed", [SourceHumpName]),
    #field{name = PackName, meta = #meta{name = SourceName, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_write_unit(Unit = #i128{name = Name, default = Default, comment = Comment}) ->
    SourceName = tool:default(Name, Default),
    SourceHumpName = word:to_hump(SourceName),
    PackName = tool:default(Default, Name),
    Args = io_lib:format("~s", [SourceHumpName]),
    Packs = io_lib:format("~s:128/signed", [SourceHumpName]),
    #field{name = PackName, meta = #meta{name = SourceName, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_write_unit(Unit = #rst{name = Name, default = Default, comment = Comment, explain = Explain}) ->
    SourceName = tool:default(Name, Default),
    SourceHumpName = word:to_hump(SourceName),
    PackName = tool:default(Default, Name),
    Args = io_lib:format("~s", [SourceHumpName]),
    Packs = io_lib:format("(text(~w, ~s))/binary", [Explain, SourceHumpName]),
    #field{name = PackName, meta = #meta{name = SourceName, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_write_unit(Unit = #bst{name = Name, default = Default, comment = Comment}) ->
    SourceName = tool:default(Name, Default),
    SourceHumpName = word:to_hump(SourceName),
    PackName = tool:default(Default, Name),
    Args = io_lib:format("~s", [SourceHumpName]),
    Packs = io_lib:format("(byte_size(~s)):16, (~s)/binary", [SourceHumpName, SourceHumpName]),
    #field{name = PackName, meta = #meta{name = SourceName, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_write_unit(Unit = #str{name = Name, default = Default, comment = Comment}) ->
    SourceName = tool:default(Name, Default),
    SourceHumpName = word:to_hump(SourceName),
    PackName = tool:default(Default, Name),
    Args = io_lib:format("~s", [SourceHumpName]),
    Packs = io_lib:format("(length(~s)):16, (list_to_binary(~s))/binary", [SourceHumpName, SourceHumpName]),
    #field{name = PackName, meta = #meta{name = SourceName, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};

%% structure unit
parse_write_unit(#record{name = Name, default = Default, explain = Explain}) ->
    %% format per unit
    Field = parse_write_unit(Explain),
    Field#field{name = tool:default(Name, Default)};

parse_write_unit(#tuple{name = Name, default = Default, explain = Explain}) ->
    %% format per unit
    Field = parse_write_unit(Explain),
    Field#field{name = tool:default(Name, Default)};

parse_write_unit(#ets{name = Name, comment = Comment, explain = Explain}) ->
    %% hump name
    HumpName = word:to_hump(Name),
    %% format subunit
    #field{args = Args, packs = Packs, meta = Meta} = parse_write_unit(Explain),
    %% meta may be list or meta record
    ReviseMeta = lists:flatten([Meta]),
    %% format list pack info
    Procedure = io_lib:format("~sBinary = protocol:write_ets(fun([~s]) -> <<~s>> end, ~s)", [HumpName, Args, Packs, HumpName]),
    EtsPacks = io_lib:format("~sBinary/binary", [HumpName]),
    #field{name = Name, args = HumpName, procedure = Procedure, packs = EtsPacks, meta = #meta{name = Name, type = list, explain = ReviseMeta, comment = Comment}};

parse_write_unit(#list{name = Name, default = Default, explain = Explain, comment = Comment}) ->
    %% hump name
    SourceName = tool:default(Name, Default),
    HumpName = word:to_hump(SourceName),
    %% format subunit
    #field{args = Args, packs = Packs, meta = Meta} = parse_write_unit(Explain),
    %% format list pack info
    ListPacks = io_lib:format("(length(~s)):16, <<<<~s>> || ~s <- ~s>>/binary", [HumpName, Packs, Args, HumpName]),
    #field{name = SourceName, args = HumpName, packs = ListPacks, meta = #meta{name = SourceName, type = list, explain = Meta, comment = Comment}};

parse_write_unit(Record) when is_tuple(Record) andalso tuple_size(Record) > 0 andalso is_atom(element(1, Record)) ->
    %% get beam abstract code
    Tag = element(1, Record),
    NameList = beam:find(Tag),
    %% throw error when beam abstract code empty
    NameList =:= [] andalso erlang:error(need_to_update_beam_abstract_code),
    tuple_size(Record) =/= length(NameList) andalso erlang:error(need_to_update_beam_abstract_code),
    %% zip field value and field name
    ZipList = lists:zip(tuple_to_list(Record), NameList),
    %% format per unit
    List = [parse_write_unit(setelement(3, Field, Name)) || {Field, Name} <- ZipList, is_unit(Field)],
    %% format function match args
    Args = lists:concat(["#", Tag, "{", string:join([io_lib:format("~s = ~s", [Names, Args]) || #field{name = Names, args = Args} <- List], ", "), "}"]),
    %% format function pack info
    Packs = string:join(listing:collect(#field.packs, List, []), ", "),
    #field{args = Args, packs = Packs, name = listing:collect(#field.name, List), meta = lists:flatten(listing:collect(#field.meta, List, []))};
parse_write_unit(Tuple) when is_tuple(Tuple) andalso tuple_size(Tuple) > 0 ->
    %% format per unit
    List = [parse_write_unit(Field) || Field <- tuple_to_list(Tuple)],
    %% format function match args
    Args = lists:concat(["{", string:join([Args || #field{args = Args} <- List], ", "), "}"]),
    %% format function pack info
    Packs = string:join(listing:collect(#field.packs, List, []), ", "),
    #field{args = Args, packs = Packs, name = listing:collect(#field.name, List, []), meta = listing:collect(#field.meta, List, [])};
parse_write_unit(_) ->
    #field{}.

%%%===================================================================
%%% Common Tool
%%%===================================================================
%% calc list bit sum
sum(List) ->
    sum(List, 0).
sum([], Sum) ->
    Sum;
sum([#meta{name = Name, type = Type, explain = Explain} | T], Sum) ->
    Number = bit(Type, Explain, Name),
    sum(T, Number + Sum).

%% join string
join([]) ->
    [];
join([H]) ->
    H;
join([H | T]) ->
    "[" ++ H ++ lists:append([", " ++ X || X <- T]) ++ "]".

%% bit type check
bit(u8, _, _)   ->   8;
bit(u16, _, _)  ->  16;
bit(u32, _, _)  ->  32;
bit(u64, _, _)  ->  64;
bit(u128, _, _) -> 128;
bit(i8, _, _)   ->   8;
bit(i16, _, _)  ->  16;
bit(i32, _, _)  ->  32;
bit(i64, _, _)  ->  64;
bit(i128, _, _) -> 128;
bit(binary, N, _) -> N;
bit(Type, _, Name) ->
    erlang:error(list_to_atom(lists:concat(['binary_type_list => ', Name, ":", Type]))).

%% is bit unit
is_unit(#u8{})     -> true;
is_unit(#u16{})    -> true;
is_unit(#u32{})    -> true;
is_unit(#u64{})    -> true;
is_unit(#u128{})   -> true;
is_unit(#i8{})     -> true;
is_unit(#i16{})    -> true;
is_unit(#i32{})    -> true;
is_unit(#i64{})    -> true;
is_unit(#i128{})   -> true;
is_unit(#str{})    -> true;
is_unit(#bst{})    -> true;
is_unit(#binary{}) -> true;
is_unit(#tuple{})  -> true;
is_unit(#record{}) -> true;
is_unit(#list{})   -> true;
is_unit(#ets{})    -> true;
is_unit(_)         -> false.

%%%===================================================================
%%% text extract and restore
%%%===================================================================
%% @doc extract to csv file
extract(File) ->
    List = extract_file_loop(filelib:wildcard(maker:relative_path("script/make/protocol/*.erl")), []),
    %% {file, protocol, key, text list and translate list} | ...
    Result = string:join([io_lib:format(string:join(lists:duplicate(length(X), "~s"), ","), X) || X <- List], "\n"),
    file:write_file(File, Result).

extract_file_loop([], List) ->
    lists:append(lists:reverse(List));
extract_file_loop([File | T], List) ->
    {ok, Binary} = file:read_file(File),
    %% split with io record
    BlockList = re:split(Binary, "#io"),
    case extract_loop(BlockList, File, []) of
        [] ->
            extract_file_loop(T, List);
        Data ->
            extract_file_loop(T, [Data | List])
    end.

extract_loop([], _, List) ->
    lists:append(lists:reverse(List));
extract_loop([Block | T], File, List) ->
    case {extract_protocol(Block), extract_text(Block), extract_translate(Block)} of
        {[], _, _} ->
            extract_loop(T, File, List);
        {_, [], _} ->
            extract_loop(T, File, List);
        {Protocol, Text, Translate} ->
            ReviseText = lists:map(fun({Key, Value}) -> {Key, default_language(), Value}; (Other) -> Other end, Text),
            Rows = merge(File, Protocol, Translate, ReviseText, []),
            extract_loop(T, File, [Rows | List])
    end.

%% file, protocol, key, [{sc, value}, {en, value}, ...]
merge(_, _, _, [], List) ->
    (List);
merge(File, Protocol, Translate, [{Key, Language, Value} | T], List) ->
    TextList = lists:sort(fun sort/2, [{Language, Value} | [{L, V} || {K, L, V} <- Translate, K == Key]]),
    Row = ([File, Protocol, Key | [element(2, listing:key_find(L, 1, TextList, {L, []})) || {_, L} <- language()]]),
    merge(File, Protocol, Translate, T, [Row | List]).

%% the default language
default_language() ->
    parameter_data:get(language).

%% language enumeration
language() ->
    parameter_data:get(language_set).

%% chinese simplified
sort({sc, _}, _) -> true;
sort(_, {sc, _}) -> false;
%% chinese traditional
sort({tc, _}, _) -> true;
sort(_, {tc, _}) -> false;
%% english
sort({en, _}, _) -> true;
sort(_, {en, _}) -> false;
%% korea
sort({kr, _}, _) -> true;
sort(_, {kr, _}) -> false;
%% vietnam
sort({vi, _}, _) -> true;
sort(_, {vi, _}) -> false.

extract_protocol(Block) ->
    case re:run(Block, "(?m)(?s)protocol\\s*=\\s*(\\d+)\\s*", [{capture, first, list}]) of
        {match, [ProtocolCode]} ->
            string:strip(hd(tl(string:tokens(ProtocolCode, "="))));
        _ ->
            []
    end.

extract_text(Block) ->
    case re:run(Block, "(?m)(?s)text\\s*=\\s*\\[.*?\\]", [{capture, first, list}]) of
        {match, [TextCode]} ->
            parser:to_term(string:strip(hd(tl(string:tokens(TextCode, "=")))));
        _ ->
            []
    end.

extract_translate(Block) ->
    case re:run(Block, "(?m)(?s)translate\\s*=\\s*\\[.*?\\]", [{capture, first, list}]) of
        {match, [TranslateCode]} ->
            parser:to_term(string:strip(hd(tl(string:tokens(TranslateCode, "=")))));
        _ ->
            []
    end.

%% @doc restore from csv file
restore(File) ->
    {ok, Binary} = file:read_file(File),
    %% delete \r and split with \n
    RowList = string:tokens(binary_to_list(binary:replace(Binary, <<"\r">>, <<>>, [global])), "\n"),
    List = lists:reverse(build(RowList, [])),
    restore_file_loop(lists:reverse(listing:key_merge(1, List))).

build([], List) ->
    List;
build([Row | T], List) ->
    [File, Protocol, Key | _] = Fields = string:tokens(Row, ","),
    case lists:sort(fun sort/2, [{L, nth(3 + P, Fields)} || {P, L} <- language(), L =/= default_language(), nth(3 + P, Fields) =/= []]) of
        [] ->
            build(T, List);
        Translate ->
            build(T, [{File, Protocol, Key, Translate} | List])
    end.

nth(_, []) ->
    [];
nth(1, [H | _]) ->
    H;
nth(N, [_ | T]) when N > 1 ->
    nth(N - 1, T).

restore_file_loop([]) ->
    ok;
restore_file_loop([{File, List} | T]) ->
    {ok, Binary} = file:read_file(File),
    BlockList = re:split(re:replace(Binary, "(?m)(?s)translate\\s*=\\s*\\[.*?\\],\n?\\s*", [], [{return, binary}]), "#io"),
    %% file, protocol, key, language, value
    GroupList = listing:key_merge(2, List),
    NewBinary = fill_loop(BlockList, GroupList, []),
    file:write_file(File, NewBinary),
    restore_file_loop(T).

fill_loop([], _, Result) ->
    concat(lists:reverse(Result), <<"#io">>, <<>>);
fill_loop([Block | T], GroupList, List) ->
    Protocol = extract_protocol(Block),
    case re:run(Block, "(?m)(?s)text\\s*=\\s*\\[.*?\\],\n?\\s*") of
        {match, [{Start, End}]} ->
            case lists:keyfind(Protocol, 1, GroupList) of
                {_, Translate} ->
                    %% offset is the end of text field assignment sentence
                    Offset = Start + End,
                    %% split block
                    <<Head:Offset/binary-unit:8, Tail/binary-unit:8>> = Block,
                    %% construct code
                    Text = concat([<<"{", (type:to_binary(Key))/binary, ", ", (type:to_binary(Language))/binary, ", \"", (type:to_binary(encoding:to_list(Value)))/binary, "\"}">> || {_, _, Key, LanguageList} <- Translate, {Language, Value} <- LanguageList]),
                    %% @todo alignment, sort, etc...
                    Code = <<Head/binary, "translate = [", Text/binary, "],", "\n", "                ", Tail/binary>>,
                    %% replace code block
                    fill_loop(T, GroupList, [Code | List]);
                _ ->
                    fill_loop(T, GroupList, [Block | List])
            end;
        _ ->
            fill_loop(T, GroupList, [Block | List])
    end.

%% concat binary
concat(List) ->
    concat(List, <<$,>>, <<>>).
concat([], _, Binary) ->
    Binary;
concat([H | T], Separator, <<>>) ->
    concat(T, Separator, <<H/binary>>);
concat([H | T], Separator, Binary) ->
    concat(T, Separator, <<Binary/binary, Separator/binary, H/binary>>).
