%%%------------------------------------------------------------------
%%% @doc
%%% module protocol maker
%%% @end
%%%------------------------------------------------------------------
-module(protocol_maker).
-export([start/1]).
-include("serialize.hrl").
%% syntax field record
-record(field,    {names = [], meta = [], args = [], procedure = [], packs = []}).
%% ast metadata
-record(meta,     {name = [], type, explain = [], comment = []}).
%% lang code
-record(code,     {default_handler = [], handler = [], erl = [], lua = [], json = []}).
%%%==================================================================
%%% API functions
%%%==================================================================
start(List) ->
    lists:foreach(fun parse/1, List).

%%%==================================================================
%%% Parse
%%%==================================================================
parse(#protocol{io = IO, includes = Includes, erl = ErlFile, json = JsonFile, lua = LuaFile, handler = HandlerFile}) ->
    %% start collect code
    #code{handler = HandlerCode, erl = ErlCode, json = JsonCode, lua = LuaCode} = collect_code(IO, [], []),
    %% handler code
    HandlerData = lists:concat(["-module(", filename:basename(HandlerFile, ".erl"), ").\n-export([handle/3]).\n\n", HandlerCode]),
    file:write_file(HandlerFile, HandlerData),
    %% names
    ErlName = filename:basename(ErlFile, ".erl"),
    %% erl file
    IncludeCode = [io_lib:format("-include(\"~s\").\n", [Include]) || Include <- Includes],
    ErlData = io_lib:format("-module(~s).\n-export([read/2, write/2]).\n~s~s", [ErlName, IncludeCode, ErlCode]),
    file:write_file(ErlFile, ErlData),
    %% json code (file cannot write when parameter not given)
    JsonName = maker:lower_hump(filename:basename(JsonFile, ".js")),
    JsonData = lists:concat(["let ", JsonName, " = ", JsonCode, ";"]),
    file:write_file(JsonFile, JsonData),
    %% lua code (file cannot write when parameter not given)
    LuaName = maker:lower_hump(filename:basename(LuaFile, ".lua")),
    LuaData = lists:concat(["local ", LuaName, " = ", LuaCode]),
    file:write_file(LuaFile, LuaData),
    ok.

%% collect code
collect_code([], ReadList, WriteList) ->
    %% handler code
    DefaultHandler = tool:default(lists:reverse(listing:collect(#code.default_handler, ReadList, [])), "handle(Protocol, _, Data) ->\n    {error, Protocol, Data}.\n"),
    Handler = lists:concat([lists:reverse(listing:collect(#code.handler, ReadList, [])), DefaultHandler]),
    %% erl code
    ErlRead = lists:reverse(listing:collect(#code.erl, ReadList, [])),
    ErlWrite = lists:reverse(listing:collect(#code.erl, WriteList, [])),
    Erl = lists:concat(["\n\n", ErlRead, "read(Code, Binary) ->\n    {error, Code, Binary}.\n\n", "\n\n", ErlWrite, "write(Code, Content) ->\n    {error, Code, Content}.\n"]),
    %% json metadata, name test_protocol -> testProtocol
    JsonRead = string:join(lists:reverse(listing:collect(#code.json, ReadList, [])), ",\n"),
    JsonWrite = string:join(lists:reverse(listing:collect(#code.json, WriteList, [])), ",\n"),
    %% reverse read and write code
    Json = lists:concat(["{\n    read: {\n", JsonWrite, "\n    },\n    write: {\n", JsonRead, "\n    }\n}"]),
    %% @todo string type json key here
    %% Json = lists:concat(["{\n    \"read\" : {\n", JsonRead, "\n    },\n    \"write\" : {\n", JsonWrite, "\n    }\n}"]),
    %% lua metadata, name test_protocol -> testProtocol
    LuaRead = string:join(lists:reverse(listing:collect(#code.lua, ReadList, [])), ",\n"),
    LuaWrite = string:join(lists:reverse(listing:collect(#code.lua, WriteList, [])), ",\n"),
    %% reverse read and write code
    Lua = lists:concat(["{\n    [\"read\"] = {\n", LuaWrite, "\n    },\n    [\"write\"] = {\n", LuaRead, "\n    }\n}"]),
    %% return code sets
    #code{erl = Erl, lua = Lua, json = Json, handler = Handler};
collect_code([#io{read = Read, write = Write, handler = Handler, name = Protocol} | T], ReadList, WriteList) ->
    ReadCode = parse_read(Protocol, Read, Handler),
    WriteCode = parse_write(Protocol, Write),
    collect_code(T, [ReadCode | ReadList], [WriteCode | WriteList]).

%%%==================================================================
%%% Parse Json Code Part
%%%==================================================================
%% json code
parse_meta_json(Protocol, Meta) ->
    %% start with 3 tabs(4 space) padding
    Result = parse_meta_json_loop(Meta, 3, []),
    %% format a protocol define
    lists:concat(["        ", Protocol, ": [\n", Result, "\n        ]"]).
    %% @todo string type json key here
    %% lists:concat(["        \"", Protocol, "\" : [\n", Result, "\n        ]"]).

parse_meta_json_loop([], _, List) ->
    %% construct as a list
    string:join(lists:reverse(List), ",\n");
parse_meta_json_loop([#meta{name = Name, type = binary, explain = Length, comment = Comment} | T], Depth, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    String = lists:flatten(lists:concat([Padding, "{name: \"", maker:lower_hump(Name), "\", type: \"", binary, "\", comment: \"", encoding:to_list(Comment), "\", explain: ", Length, "}"])),
    %% @todo string type json key here
    %% String = lists:flatten(lists:concat([Padding, "{\"name\" : \"", maker:lower_hump(Name), "\", \"type\" : \"", binary, "\", \"comment\" : \"", encoding:to_list(Comment), "\", \"explain\" : \"", Length, "\"}"])),
    parse_meta_json_loop(T, Depth, [String | List]);
parse_meta_json_loop([#meta{name = Name, type = Type, explain = [], comment = Comment} | T], Depth, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    String = lists:flatten(lists:concat([Padding, "{name: \"", maker:lower_hump(Name), "\", type: \"", Type, "\", comment: \"", encoding:to_list(Comment), "\", explain: []}"])),
    %% @todo string type json key here
    %% String = lists:flatten(lists:concat([Padding, "{\"name\" : \"", maker:lower_hump(Name), "\", \"type\" : \"", Type, "\", \"comment\" : \"", encoding:to_list(Comment), "\", \"explain\" : []}"])),
    parse_meta_json_loop(T, Depth, [String | List]);
parse_meta_json_loop([#meta{name = Name, type = Type, explain = Explain = [_ | _], comment = Comment} | T], Depth, List) ->
    %% recurse
    Result = parse_meta_json_loop(Explain, Depth + 1, []),
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format a field
    String = lists:concat([Padding, "{name: \"", maker:lower_hump(Name), "\", type: \"", Type, "\", comment: \"", encoding:to_list(Comment), "\", explain: [\n", Result, "\n", Padding, "]}"]),
    %% @todo string type json key here
    %% String = lists:concat([Padding, "{\"name\" : \"", maker:lower_hump(Name), "\", \"type\" : \"", Type, "\", \"comment\" : \"", encoding:to_list(Comment), "\", \"explain\" : [\n", Result, "\n", Padding, "]}"]),
    parse_meta_json_loop(T, Depth, [String | List]).

%%%==================================================================
%%% Parse Lua Code Part
%%%==================================================================
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
    String = lists:flatten(lists:concat([Padding, "{name = \"", maker:lower_hump(Name), "\", type = \"", binary, "\", comment = \"", encoding:to_list(Comment), "\", explain = ", Length, "}"])),
    parse_meta_lua_loop(T, Depth, [String | List]);
parse_meta_lua_loop([#meta{name = Name, type = Type, explain = [], comment = Comment} | T], Depth, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format a field
    String = lists:flatten(lists:concat([Padding, "{name = \"", maker:lower_hump(Name), "\", type = \"", Type, "\", comment = \"", encoding:to_list(Comment), "\", explain = {}}"])),
    parse_meta_lua_loop(T, Depth, [String | List]);
parse_meta_lua_loop([#meta{name = Name, type = Type, explain = Explain = [_ | _], comment = Comment} | T], Depth, List) ->
    %% recurse
    Result = parse_meta_lua_loop(Explain, Depth + 1, []),
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    String = lists:concat([Padding, "{name = \"", maker:lower_hump(Name), "\", type = \"", Type, "\", comment = \"", encoding:to_list(Comment), "\", explain = {\n", Result, "\n", Padding, "}}"]),
    parse_meta_lua_loop(T, Depth, [String | List]).

%%%==================================================================
%%% Parse Read Part
%%%==================================================================
parse_read(Protocol, SyntaxList, undefined) ->
    %% no handler
    Code = parse_read(Protocol, SyntaxList, #handler{}),
    Code#code{handler = [], default_handler = []};
parse_read(0, [], #handler{module = Module, function = Function, arg = Arg}) ->
    %% default handler code
    HandlerArgs = string:join([maker:hump(A) || A <-  [Arg, "Data"], A =/= []], ", "),
    HandlerCode = lists:concat(["handle(", "_", ", ", tool:default(maker:hump(Arg), "_"), ", ", "Data", ") ->\n    ", Module, ":", Function, "(", HandlerArgs, ").\n"]),
    #code{erl = [], json = [], lua = [], handler = [], default_handler = HandlerCode};
parse_read(Protocol, [], #handler{module = Module, function = Function, arg = Arg}) ->
    %% erl code
    ErlCode = "read(" ++ type:to_list(Protocol) ++ ", <<>>) ->\n    {ok, []};\n\n",
    JsonCode = lists:concat(["        ", Protocol, " : ", "[]"]),
    %% @todo string type json key here
    %% JsonCode = lists:concat(["        \"", Protocol, "\" : ", "[]"]),
    LuaCode = lists:concat(["        [", Protocol, "] = ", "{}"]),
    %% handler code
    HandlerArgs = string:join([maker:hump(A) || A <- [Arg], A =/= []], ", "),
    HandlerCode = lists:concat(["handle(", Protocol, ", ", tool:default(maker:hump(Arg), "_"), ", [", "]) ->\n    ", Module, ":", Function, "(", HandlerArgs, ");\n\n"]),
    #code{erl = ErlCode, json = JsonCode, lua = LuaCode, handler = HandlerCode};
parse_read(Protocol, SyntaxList, #handler{module = Module, function = Function, arg = Arg}) ->
    List = [parse_read_unit(Syntax) || Syntax <- SyntaxList],
    %% collect code args
    ArgList = listing:collect(#field.args, List),
    ArgListCode = string:join(ArgList, ", "),
    %% string type convert binary_to_list args revise
    HandlerArgList = [string:strip(string:strip(re:replace(A, "binary_to_list", "", [{return, list}]), left, $(), right, $)) ||  A<- ArgList],
    HandlerArgListCode = string:join(HandlerArgList, ", "),
    %% construct erl handler code
    HandlerArgs = string:join([maker:hump(A) || A <- [Arg | HandlerArgList], A =/= []], ", "),
    HandlerCode = lists:concat(["handle(", Protocol, ", ", tool:default(maker:hump(Arg), "_"), ", [", HandlerArgListCode, "]) ->\n    ", Module, ":", Function, "(", HandlerArgs, ");\n\n"]),
    %% construct erl code
    Procedure = ["\n    " ++ Procedure ++ "," || #field{procedure = Procedure} <- List, Procedure =/=[]],
    Packs = string:join(listing:collect(#field.packs, List), ", "),
    ErlCode = lists:concat(["read(", Protocol, ", <<", Packs, ">>) ->", Procedure, "\n    {ok, [", ArgListCode, "]};\n\n"]),
    %% collect unit meta
    MetaList = lists:flatten(listing:collect(#field.meta, List)),
    %% construct json/lua code
    JsonCode = parse_meta_json(Protocol, MetaList),
    LuaCode = parse_meta_lua(Protocol, MetaList),
    #code{erl = ErlCode, json = JsonCode, lua = LuaCode, handler = HandlerCode}.

%% parse unit
parse_read_unit(Unit = #binary{name = Name, explain = Explain, comment = Comment}) ->
    HumpName = maker:hump(Name),
    Args = io_lib:format("~s", [HumpName]),
    Length =  Explain * 8,
    Packs = io_lib:format("~s:~p", [HumpName, Length]),
    #field{names = Name, meta = #meta{name = Name, type = element(1, Unit), explain = Length, comment = Comment}, args = Args, packs = Packs};
parse_read_unit(Unit = #u8{name = Name, comment = Comment}) ->
    HumpName = maker:hump(Name),
    Args = io_lib:format("~s", [HumpName]),
    Packs = io_lib:format("~s:8", [HumpName]),
    #field{names = Name, meta = #meta{name = Name, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_read_unit(Unit = #u16{name = Name, comment = Comment}) ->
    HumpName = maker:hump(Name),
    Args = io_lib:format("~s", [HumpName]),
    Packs = io_lib:format("~s:16", [HumpName]),
    #field{names = Name, meta = #meta{name = Name, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_read_unit(Unit = #u32{name = Name, comment = Comment}) ->
    HumpName = maker:hump(Name),
    Args = io_lib:format("~s", [HumpName]),
    Packs = io_lib:format("~s:32", [HumpName]),
    #field{names = Name, meta = #meta{name = Name, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_read_unit(Unit = #u64{name = Name, comment = Comment}) ->
    HumpName = maker:hump(Name),
    Args = io_lib:format("~s", [HumpName]),
    Packs = io_lib:format("~s:64", [HumpName]),
    #field{names = Name, meta = #meta{name = Name, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_read_unit(Unit = #u128{name = Name, comment = Comment}) ->
    HumpName = maker:hump(Name),
    Args = io_lib:format("~s", [HumpName]),
    Packs = io_lib:format("~s:128", [HumpName]),
    #field{names = Name, meta = #meta{name = Name, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_read_unit(Unit = #bst{name = Name, comment = Comment}) ->
    HumpName = maker:hump(Name),
    Args = io_lib:format("~s", [HumpName]),
    Packs = io_lib:format("~s:16, ~s:~s/binary", [HumpName ++ "Length", HumpName, HumpName ++ "Length"]),
    #field{names = Name, meta = #meta{name = Name, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_read_unit(Unit = #str{name = Name, comment = Comment}) ->
    HumpName = maker:hump(Name),
    Args = io_lib:format("binary_to_list(~s)", [HumpName]),
    Packs = io_lib:format("~s:16, ~s:~s/binary", [HumpName ++ "Length", HumpName, HumpName ++ "Length"]),
    #field{names = Name, meta = #meta{name = Name, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};

%% structure unit
parse_read_unit(#tuple{name = Name, explain = Explain}) ->
    %% format per unit
    List = [parse_read_unit(Field) || Field <- tuple_to_list(Explain)],
    %% format function match args
    Args = lists:concat(["{", string:join([type:to_list(Args) || #field{args = Args} <- List], ", "), "}"]),
    %% format function pack info
    Packs = string:join(listing:collect(#field.packs, List, []), ", "),
    #field{names = Name, args = Args, packs = Packs, meta = listing:collect(#field.meta, List, [])};
parse_read_unit(#list{name = Name, explain = Explain, comment = Comment}) ->
    %% hump name is unpack bit variable bind
    HumpName = maker:hump(Name),
    %% format subunit
    #field{args = Args, packs = Packs, meta = Meta} = parse_read_unit(Explain),
    %% format list pack info
    Procedure = io_lib:format("~s = [~s || <<~s>> <= ~sBinary]", [HumpName, Args, Packs, HumpName]),
    %% read a list cannot contain variable length binary like string/binary
    ListPacks = io_lib:format("~sBinary/binary-unit:~p", [HumpName, sum(Meta)]),
    #field{names = Name, args = HumpName, procedure = Procedure, packs = ListPacks, meta = #meta{name = Name, type = list, explain = Meta, comment = Comment}};
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
    List = [parse_read_unit(setelement(2, Explain, Name)) || {Explain, Name} <- ZipList, is_unit(Explain)],
    %% format function match param
    Args = lists:concat(["#", Tag, "{", string:join([io_lib:format("~s = ~s", [Names, Args]) || #field{names = Names, args = Args} <- List], ", "), "}"]),
    %% format function pack info
    Packs = string:join(listing:collect(#field.packs, List, []), ", "),
    #field{args = Args, packs = Packs, names = listing:collect(#field.names, List), meta = lists:flatten(listing:collect(#field.meta, List))};
parse_read_unit(Tuple) when is_tuple(Tuple) andalso tuple_size(Tuple) > 0 ->
    %% format per unit
    List = [parse_read_unit(Field) || Field <- tuple_to_list(Tuple)],
    %% format function match args
    Args = lists:concat(["{", string:join([type:to_list(Args) || #field{args = Args} <- List], ", "), "}"]),
    %% format function pack info
    Packs = string:join(listing:collect(#field.packs, List, []), ", "),
    #field{args = Args, packs = Packs, names = listing:collect(#field.names, List, []), meta = listing:collect(#field.meta, List, [])};
parse_read_unit(_) ->
    #field{}.

%%%==================================================================
%%% Parse Write Part
%%%==================================================================
parse_write(0, []) ->
    #code{erl = [], json = [], lua = [], handler = []};
parse_write(Protocol, []) ->
    %% erl code
    ErlCode = "write(" ++ type:to_list(Protocol) ++ ", []) ->\n    {ok, protocol:pack(" ++ type:to_list(Protocol) ++ ", <<>>)};\n\n",
    JsonCode = lists:concat(["        ", Protocol, " : ", "[]"]),
    %% @todo string type json key here
    %% JsonCode = lists:concat(["        \"", Protocol, "\" : ", "[]"]),
    LuaCode = lists:concat(["        [", Protocol, "] = ", "{}"]),
    #code{erl = ErlCode, json = JsonCode, lua = LuaCode};
parse_write(Protocol, SyntaxList) ->
    List = [parse_write_unit(Syntax) || Syntax <- SyntaxList],
    %% collect code args
    ArgList = listing:collect(#field.args, List),
    Args = string:join(ArgList, ", "),
    %% construct erl code
    Procedure = ["\n    " ++ Procedure ++ "," || #field{procedure = Procedure} <- List, Procedure =/=[]],
    Packs = string:join(listing:collect(#field.packs, List), ", "),
    ErlCode = lists:concat(["write(", Protocol, ", [", Args, "]) ->", Procedure, "\n    {ok, protocol:pack(", Protocol, ", <<", Packs, ">>)};\n\n"]),
    %% collect unit meta
    MetaList = lists:flatten(listing:collect(#field.meta, List)),
    %% construct json/lua code
    JsonCode = parse_meta_json(Protocol, MetaList),
    LuaCode = parse_meta_lua(Protocol, MetaList),
    #code{erl = ErlCode, json = JsonCode, lua = LuaCode}.

%% parse unit
parse_write_unit(#zero{}) ->
    #field{args = '_'};
parse_write_unit(Unit = #binary{name = Name, default = Default, comment = Comment, explain = Explain}) ->
    SourceName = tool:default(Name, Default),
    SourceHumpName = maker:hump(SourceName),
    PackName = tool:default(Default, Name),
    Length =  Explain * 8,
    Args = io_lib:format("~s", [SourceHumpName]),
    Packs = io_lib:format("~s:~p", [SourceHumpName, Length]),
    #field{names = PackName, meta = #meta{name = SourceName, type = element(1, Unit), explain = Length, comment = Comment}, args = Args, packs = Packs};
parse_write_unit(Unit = #u8{name = Name, default = Default, comment = Comment}) ->
    SourceName = tool:default(Name, Default),
    SourceHumpName = maker:hump(SourceName),
    PackName = tool:default(Default, Name),
    Args = io_lib:format("~s", [SourceHumpName]),
    Packs = io_lib:format("~s:8", [SourceHumpName]),
    #field{names = PackName, meta = #meta{name = SourceName, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_write_unit(Unit = #u16{name = Name, default = Default, comment = Comment}) ->
    SourceName = tool:default(Name, Default),
    SourceHumpName = maker:hump(SourceName),
    PackName = tool:default(Default, Name),
    Args = io_lib:format("~s", [SourceHumpName]),
    Packs = io_lib:format("~s:16", [SourceHumpName]),
    #field{names = PackName, meta = #meta{name = SourceName, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_write_unit(Unit = #u32{name = Name, default = Default, comment = Comment}) ->
    SourceName = tool:default(Name, Default),
    SourceHumpName = maker:hump(SourceName),
    PackName = tool:default(Default, Name),
    Args = io_lib:format("~s", [SourceHumpName]),
    Packs = io_lib:format("~s:32", [SourceHumpName]),
    #field{names = PackName, meta = #meta{name = SourceName, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_write_unit(Unit = #u64{name = Name, default = Default, comment = Comment}) ->
    SourceName = tool:default(Name, Default),
    SourceHumpName = maker:hump(SourceName),
    PackName = tool:default(Default, Name),
    Args = io_lib:format("~s", [SourceHumpName]),
    Packs = io_lib:format("~s:64", [SourceHumpName]),
    #field{names = PackName, meta = #meta{name = SourceName, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_write_unit(Unit = #u128{name = Name, default = Default, comment = Comment}) ->
    SourceName = tool:default(Name, Default),
    SourceHumpName = maker:hump(SourceName),
    PackName = tool:default(Default, Name),
    Args = io_lib:format("~s", [SourceHumpName]),
    Packs = io_lib:format("~s:128", [SourceHumpName]),
    #field{names = PackName, meta = #meta{name = SourceName, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_write_unit(Unit = #bst{name = Name, default = Default, comment = Comment}) ->
    SourceName = tool:default(Name, Default),
    SourceHumpName = maker:hump(SourceName),
    PackName = tool:default(Default, Name),
    Args = io_lib:format("~s", [SourceHumpName]),
    Packs = io_lib:format("(byte_size(~s)):16, (~s)/binary", [SourceHumpName, SourceHumpName]),
    #field{names = PackName, meta = #meta{name = SourceName, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_write_unit(Unit = #str{name = Name, default = Default, comment = Comment}) ->
    SourceName = tool:default(Name, Default),
    SourceHumpName = maker:hump(SourceName),
    PackName = tool:default(Default, Name),
    Args = io_lib:format("~s", [SourceHumpName]),
    Packs = io_lib:format("(byte_size(~s)):16, (list_to_binary(~s))/binary", [SourceHumpName, SourceHumpName]),
    #field{names = PackName, meta = #meta{name = SourceName, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};

%% structure unit
parse_write_unit(#tuple{name = Name, default = Default, explain = Explain}) ->
    %% format per unit
    Field = parse_write_unit(Explain),
    Field#field{names = tool:default(Name, Default)};
parse_write_unit(#record{name = Name, default = Default, explain = Explain}) ->
    %% format per unit
    Field = parse_write_unit(Explain),
    Field#field{names = tool:default(Name, Default)};

parse_write_unit(#ets{name = Name, comment = Comment, explain = Explain}) ->
    %% hump name
    HumpName = maker:hump(Name),
    %% format subunit
    #field{args = Args, packs = Packs, meta = Meta} = parse_write_unit(Explain),
    %% format list pack info
    Procedure = io_lib:format("~sBinary = protocol:write_ets(fun([~s]) -> <<~s>> end, ~s)", [HumpName, Args, Packs, HumpName]),
    EtsPacks = io_lib:format("~sBinary/binary", [HumpName]),
    #field{names = Name, args = HumpName, procedure = Procedure, packs = EtsPacks, meta = #meta{name = Name, type = list, explain = Meta, comment = Comment}};

parse_write_unit(#list{name = Name, explain = Explain, comment = Comment}) ->
    %% hump name
    HumpName = maker:hump(Name),
    %% format subunit
    #field{args = Args, packs = Packs, meta = Meta} = parse_write_unit(Explain),
    %% format list pack info
    ListPacks = io_lib:format("(length(~s)):16, <<<<~s>> || ~s <- ~s>>/binary", [HumpName, Packs, Args, HumpName]),
    #field{names = Name, args = HumpName, packs = ListPacks, meta = #meta{name = Name, type = list, explain = Meta, comment = Comment}};
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
    Args = lists:concat(["#", Tag, "{", string:join([io_lib:format("~s = ~s", [Names, Args]) || #field{names = Names, args = Args} <- List], ", "), "}"]),
    %% format function pack info
    Packs = string:join(listing:collect(#field.packs, List, []), ", "),
    #field{args = Args, packs = Packs, names = listing:collect(#field.names, List), meta = lists:flatten(listing:collect(#field.meta, List))};
parse_write_unit(Tuple) when is_tuple(Tuple) andalso tuple_size(Tuple) > 0 ->
    %% format per unit
    List = [parse_write_unit(Field) || Field <- tuple_to_list(Tuple)],
    %% format function match args
    Args = lists:concat(["{", string:join([lists:concat([Args]) || #field{args = Args} <- List], ", "), "}"]),
    %% format function pack info
    Packs = string:join(listing:collect(#field.packs, List, []), ", "),
    #field{args = Args, packs = Packs, names = listing:collect(#field.names, List, []), meta = listing:collect(#field.meta, List, [])};
parse_write_unit(_) ->
    #field{}.

%%%==================================================================
%%% Common Tool
%%%==================================================================
%% calc list bit sum
sum(List) ->
    sum(List, 0).
sum([], Sum) ->
    Sum;
sum([#meta{name = Name, type = Type, explain = Explain} | T], Sum) ->
    Number = bit(Type, Explain, Name),
    sum(T, Number + Sum).

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
