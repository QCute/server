%%%-------------------------------------------------------------------
%%% @doc
%%% module protocol maker
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_maker).
%% API
-export([start/1]).
%% Includes
-include("serialize.hrl").
%% Records
%% syntax field record
-record(field,    {names = [], meta = [], args = [], procedure = [], packs = []}).
%% ast metadata
-record(meta,     {name = [], type, explain = [], comment = []}).
%% lang code
-record(code,     {erl = [], lua = [], json = []}).
%%%===================================================================
%%% API
%%%===================================================================
start(List) ->
    maker:start(fun parse/2, List).

%%%===================================================================
%%% parse
%%%===================================================================
parse(_, {_, #protocol{io = IO, includes = Includes, erl = ErlFile, json = JsonFile, lua = LuaFile}}) ->
    Module = filename:basename(ErlFile, ".erl"),
    Include = [format("-include(\"~s\").\n", [Include]) || Include <- Includes],
    %% test_protocol -> testProtocol
    Name = lower_hump(Module),
    #code{erl = ErlCode, json = JsonCode, lua = LuaCode} = collect_code(IO, Name, [], []),
    %% json code (file cannot write when parameter not given)
    file:write_file(JsonFile, JsonCode),
    %% lua code (file cannot write when parameter not given)
    file:write_file(LuaFile, LuaCode),
    %% erl code
    Head = format("-module(~s).\n-compile(nowarn_export_all).\n-compile(export_all).\n", [Module]),
    [{"(?s).*", Head ++ Include ++ ErlCode}].

%% collect code
collect_code([], Name, ReadList, WriteList) ->
    %% json metadata
    Json = lists:flatten(lists:concat(["let ", Name, " = {\n    \"read\" : ", lists:reverse(take(#code.json, ReadList)), ",\n    \"write\" : ", lists:reverse(take(#code.json, WriteList)), "\n};"])),
    %% lua metadata
    Lua = lists:concat(["local ", Name, " = {\n    [\"read\"] = ", lists:reverse(take(#code.lua, ReadList)), ",\n    [\"write\"] = ", lists:reverse(take(#code.lua, WriteList)), "\n}"]),
    %% erl code
    Erl = lists:concat(["\n\n", lists:reverse(take(#code.erl, ReadList)), "read(Code, Binary) ->\n    {error, Code, Binary}.\n\n", "\n\n", lists:reverse(take(#code.erl, WriteList)), "write(Code, Content) ->\n    {error, Code, Content}.\n"]),
    #code{erl = Erl, lua = Lua, json = Json};
collect_code([#io{read = Read, write = Write, name = Protocol} | T], Name, ReadList, WriteList) ->
    ReadCode = parse_read(Protocol, Read),
    WriteCode = parse_write(Protocol, Write),
    collect_code(T, Name, [ReadCode | ReadList], [WriteCode | WriteList]).

%%====================================================================
%% parse json code part
%%====================================================================
%% json code
parse_meta_json(Protocol, Meta) ->
    %% start with 3 tabs(4 space) padding
    Result = parse_meta_json_loop(Meta, 3, []),
    %% format a protocol define
    lists:concat(["{\n        \"", Protocol, "\" : [\n", Result, "\n        ]\n    }"]).

parse_meta_json_loop([], _, List) ->
    %% construct as a list
    string:join(lists:reverse(List), ",\n");
parse_meta_json_loop([#meta{name = Name, type = binary, explain = Length, comment = Comment} | T], Depth, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    String = lists:flatten(lists:concat([Padding, "{\"name\" : \"", lower_hump(Name), "\", \"type\" : \"", binary, "\", \"comment\" : \"", Comment, "\", \"explain\" : \"", Length, "\"}"])),
    parse_meta_json_loop(T, Depth, [String | List]);
parse_meta_json_loop([#meta{name = Name, type = Type, explain = [], comment = Comment} | T], Depth, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    String = lists:flatten(lists:concat([Padding, "{\"name\" : \"", lower_hump(Name), "\", \"type\" : \"", Type, "\", \"comment\" : \"", Comment, "\", \"explain\" : []}"])),
    parse_meta_json_loop(T, Depth, [String | List]);
parse_meta_json_loop([#meta{name = Name, type = Type, explain = Explain = [_ | _], comment = Comment} | T], Depth, List) ->
    %% recurse
    Result = parse_meta_json_loop(Explain, Depth + 1, []),
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format a field
    String = lists:concat([Padding, "{\"name\" : \"", lower_hump(Name), "\", \"type\" : \"", Type, "\", \"comment\" : \"", Comment, "\", \"explain\" : [\n", Result, "\n", Padding, "]}"]),
    parse_meta_json_loop(T, Depth, [String | List]).

%%====================================================================
%% parse lua code part
%%====================================================================
%% lua code
parse_meta_lua(Protocol, Meta) ->
    %% start with 3 tabs(4 space) padding
    Result = parse_meta_lua_loop(Meta, 3, []),
    %% format a protocol define
    lists:concat(["{\n        [", Protocol, "] = {\n", Result, "\n        }\n    }"]).

parse_meta_lua_loop([], _, List) ->
    %% construct as a list
    string:join(lists:reverse(List), ",\n");
parse_meta_lua_loop([#meta{name = Name, type = binary, explain = Length, comment = Comment} | T], Depth, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format a field
    String = lists:flatten(lists:concat([Padding, "{name = \"", lower_hump(Name), "\", type = \"", binary, "\", comment = \"", Comment, "\", explain = ", Length, "}"])),
    parse_meta_lua_loop(T, Depth, [String | List]);
parse_meta_lua_loop([#meta{name = Name, type = Type, explain = [], comment = Comment} | T], Depth, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format a field
    String = lists:flatten(lists:concat([Padding, "{name = \"", lower_hump(Name), "\", type = \"", Type, "\", comment = \"", Comment, "\", explain = {}}"])),
    parse_meta_lua_loop(T, Depth, [String | List]);
parse_meta_lua_loop([#meta{name = Name, type = Type, explain = Explain = [_ | _], comment = Comment} | T], Depth, List) ->
    %% recurse
    Result = parse_meta_lua_loop(Explain, Depth + 1, []),
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    String = lists:concat([Padding, "{name = \"", lower_hump(Name), "\", type = \"", Type, "\", comment = \"", Comment, "\", explain = {\n", Result, "\n", Padding, "}}"]),
    parse_meta_lua_loop(T, Depth, [String | List]).

%%====================================================================
%% parse read part
%%====================================================================
parse_read(Protocol, []) ->
    {"read(" ++ type:to_list(Protocol) ++ ", <<>>) ->\n    {ok, []};\n\n", [], []};
parse_read(Protocol, SyntaxList) ->
    List = [parse_read_unit(Syntax) || Syntax <- SyntaxList],
    %% construct erl code
    Args = string:join(take(#field.args, List), ", "),
    Procedure = ["\n    " ++ Procedure ++ "," || #field{procedure = Procedure} <- List, Procedure =/=[]],
    Packs = string:join(take(#field.packs, List), ", "),
    ErlCode = lists:concat(["read(", Protocol, ", <<", Packs, ">>) ->", Procedure, "\n    {ok, [", Args, "]};\n\n"]),
    %% construct json/lua code
    Meta = lists:flatten(take(#field.meta, List)),
    JsonCode = parse_meta_json(Protocol, Meta),
    LuaCode = parse_meta_lua(Protocol, Meta),
    #code{erl = ErlCode, json = JsonCode, lua = LuaCode}.

%% parse unit
parse_read_unit(Unit = #binary{name = Name, explain = Explain, comment = Comment}) ->
    HumpName = maker:hump(Name),
    Args = (format("~s", [HumpName])),
    Length =  Explain * 8,
    %Type = list_to_atom(lists:concat([b, Length])),
    Packs = format("~s:~p", [HumpName, Length]),
    #field{names = Name, meta = #meta{name = Name, type = element(1, Unit), explain = Length, comment = Comment}, args = Args, packs = Packs};
parse_read_unit(Unit = #u8{name = Name, comment = Comment}) ->
    HumpName = maker:hump(Name),
    Args = format("~s", [HumpName]),
    Packs = format("~s:8", [HumpName]),
    #field{names = Name, meta = #meta{name = Name, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_read_unit(Unit = #u16{name = Name, comment = Comment}) ->
    HumpName = maker:hump(Name),
    Args = format("~s", [HumpName]),
    Packs = format("~s:16", [HumpName]),
    #field{names = Name, meta = #meta{name = Name, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_read_unit(Unit = #u32{name = Name, comment = Comment}) ->
    HumpName = maker:hump(Name),
    Args = format("~s", [HumpName]),
    Packs = format("~s:32", [HumpName]),
    #field{names = Name, meta = #meta{name = Name, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_read_unit(Unit = #u64{name = Name, comment = Comment}) ->
    HumpName = maker:hump(Name),
    Args = format("~s", [HumpName]),
    Packs = format("~s:64", [HumpName]),
    #field{names = Name, meta = #meta{name = Name, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_read_unit(Unit = #u128{name = Name, comment = Comment}) ->
    HumpName = maker:hump(Name),
    Args = format("~s", [HumpName]),
    Packs = format("~s:128", [HumpName]),
    #field{names = Name, meta = #meta{name = Name, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_read_unit(Unit = #bst{name = Name, comment = Comment}) ->
    HumpName = maker:hump(Name),
    Args = format("~s", [HumpName]),
    Packs = format("~s:16, ~s:~s/binary", [HumpName ++ "Length", HumpName, HumpName ++ "Length"]),
    #field{names = Name, meta = #meta{name = Name, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_read_unit(Unit = #str{name = Name, comment = Comment}) ->
    HumpName = maker:hump(Name),
    Args = format("binary_to_list(~s)", [HumpName]),
    Packs = format("~s:16, ~s:~s/binary", [HumpName ++ "Length", HumpName, HumpName ++ "Length"]),
    #field{names = Name, meta = #meta{name = Name, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};

%% structure unit
parse_read_unit(#list{name = Name, explain = Explain, comment = Comment}) ->
    %% hump name is unpack bit variable bind
    HumpName = maker:hump(Name),
    %% format subunit
    #field{args = Args, packs = Packs, meta = Meta} = parse_read_unit(Explain),
    %% format list pack info
    Procedure = format("~s = [~s || <<~s>> <= ~sBinary]", [HumpName, Args, Packs, HumpName]),
    %% read a list cannot contain variable length binary like string/binary
    ListPacks = format("~sBinary/binary-unit:~p", [HumpName, sum(Meta)]),
    #field{names = Name, args = HumpName, procedure = Procedure, packs = ListPacks, meta = #meta{name = Name, type = list, explain = Meta, comment = Comment}};
parse_read_unit(Record) when is_tuple(Record) andalso is_atom(element(1, Record)) ->
    %% get beam abstract code
    Tag = element(1, Record),
    NameList = beam:get(Tag),
    %% zip field value and field name
    ZipList = lists:zip(tuple_to_list(Record), NameList),
    %% format per unit
    List = [parse_read_unit(setelement(2, Explain, Name)) || {Explain, Name} <- ZipList, is_unit(Explain)],
    %% format function match param
    Args = lists:concat(["#", Tag, "{", string:join([format("~s = ~s", [Names, Args]) || #field{names = Names, args = Args} <- List], ", "), "}"]),
    %% format function pack info
    Packs = string:join(take(#field.packs, List, []), ", "),
    #field{args = Args, packs = Packs, names = take(#field.names, List), meta = take(#field.meta, List)};
parse_read_unit(Tuple) when is_tuple(Tuple) andalso tuple_size(Tuple) > 0 ->
    %% format per unit
    List = [parse_read_unit(Field) || Field <- tuple_to_list(Tuple)],
    %% format function match args
    Args = lists:concat(["{", string:join([type:to_list(Args) || #field{args = Args} <- List], ", "), "}"]),
    %% format function pack info
    Packs = string:join(take(#field.packs, List, []), ", "),
    #field{args = Args, packs = Packs, names = take(#field.names, List, []), meta = take(#field.meta, List, [])};
parse_read_unit(_) ->
    #field{}.

%%====================================================================
%% parse write part
%%====================================================================
parse_write(Protocol, []) ->
    "write(" ++ type:to_list(Protocol) ++ ", []) ->\n    {ok, protocol:pack(" ++ type:to_list(Protocol) ++ ", <<>>)};\n\n";
parse_write(Protocol, SyntaxList) ->
    List = [parse_write_unit(Syntax) || Syntax <- SyntaxList],
    %% construct erl code
    Args = string:join(take(#field.args, List), ", "),
    Procedure = ["\n    " ++ Procedure ++ "," || #field{procedure = Procedure} <- List, Procedure =/=[]],
    Packs = string:join(take(#field.packs, List), ", "),
    ErlCode = lists:concat(["write(", Protocol, ", [", Args, "]) ->", Procedure, "\n    {ok, protocol:pack(", Protocol, ", <<", Packs, ">>)};\n\n"]),
    %% construct json/lua code
    Meta = lists:flatten(take(#field.meta, List)),
    JsonCode = parse_meta_json(Protocol, Meta),
    LuaCode = parse_meta_lua(Protocol, Meta),
    #code{erl = ErlCode, json = JsonCode, lua = LuaCode}.

%% parse unit
parse_write_unit(#zero{}) ->
    #field{args = '_'};
parse_write_unit(Unit = #binary{name = Name, explain = Explain, comment = Comment}) ->
    HumpName = maker:hump(Name),
    Args = (format("~s", [HumpName])),
    Length =  Explain * 8,
    %Type = list_to_atom(lists:concat([b, Length])),
    Packs = format("~s:~p", [HumpName, Length]),
    #field{names = HumpName, meta = #meta{name = Name, type = element(1, Unit), explain = Length, comment = Comment}, args = Args, packs = Packs};
parse_write_unit(Unit = #u8{name = Name, comment = Comment}) ->
    HumpName = maker:hump(Name),
    Args = (format("~s", [HumpName])),
    Packs = format("~s:8", [HumpName]),
    #field{names = Name, meta = #meta{name = Name, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_write_unit(Unit = #u16{name = Name, comment = Comment}) ->
    HumpName = maker:hump(Name),
    Args = (format("~s", [HumpName])),
    Packs = format("~s:8", [HumpName]),
    #field{names = Name, meta = #meta{name = Name, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_write_unit(Unit = #u32{name = Name, comment = Comment}) ->
    HumpName = maker:hump(Name),
    Args = (format("~s", [HumpName])),
    Packs = format("~s:8", [HumpName]),
    #field{names = Name, meta = #meta{name = Name, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_write_unit(Unit = #u64{name = Name, comment = Comment}) ->
    HumpName = maker:hump(Name),
    Args = (format("~s", [HumpName])),
    Packs = format("~s:8", [HumpName]),
    #field{names = Name, meta = #meta{name = Name, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_write_unit(Unit = #u128{name = Name, comment = Comment}) ->
    HumpName = maker:hump(Name),
    Args = (format("~s", [HumpName])),
    Packs = format("~s:8", [HumpName]),
    #field{names = Name, meta = #meta{name = Name, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_write_unit(Unit = #bst{name = Name, comment = Comment}) ->
    HumpName = maker:hump(Name),
    Args = (format("~s", [HumpName])),
    Packs = format("(byte_size(~s)):16, (~s)/binary", [HumpName, HumpName]),
    #field{names = Name, meta = #meta{name = Name, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_write_unit(Unit = #str{name = Name, comment = Comment}) ->
    HumpName = maker:hump(Name),
    Args = (format("~s", [HumpName])),
    Packs = format("(byte_size(~s)):16, (~s)/binary", [HumpName, HumpName]),
    #field{names = Name, meta = #meta{name = Name, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};

%% structure unit
parse_write_unit(#ets{name = Name, explain = Explain, comment = Comment}) ->
    %% hump name
    HumpName = maker:hump(Name),
    %% format subunit
    #field{args = Args, packs = Packs, meta = Meta} = parse_write_unit(Explain),
    %% format list pack info
    Procedure = format("~sBinary = protocol:write_ets(fun([~s]) -> <<~s>> end, ~s)", [HumpName, Args, Packs, HumpName]),
    EtsPacks = format("~sBinary/binary", [HumpName]),
    #field{names = Name, args = HumpName, procedure = Procedure, packs = EtsPacks, meta = #meta{name = Name, type = list, explain = Meta, comment = Comment}};
parse_write_unit(#list{name = Name, explain = Explain, comment = Comment}) ->
    %% hump name
    HumpName = maker:hump(Name),
    %% format subunit
    #field{args = Args, packs = Packs, meta = Meta} = parse_write_unit(Explain),
    %% format list pack info
    ListPacks = format("(length(~s)):16, <<<<~s>> || ~s <- ~s>>/binary", [HumpName, Packs, Args, HumpName]),
    #field{names = Name, args = HumpName, packs = ListPacks, meta = #meta{name = Name, type = list, explain = Meta, comment = Comment}};
parse_write_unit(Record) when is_tuple(Record) andalso tuple_size(Record) > 0 andalso is_atom(element(1, Record)) ->
    %% get beam abstract code
    Tag = element(1, Record),
    NameList = beam:get(Tag),
    %% throw error when beam abstract code empty
    NameList =:= [] andalso erlang:error(need_to_update_beam_abstract_code),
    %% zip field value and field name
    ZipList = lists:zip(tuple_to_list(Record), NameList),
    %% format per unit
    List = [parse_write_unit(setelement(2, Field, Name)) || {Field, Name} <- ZipList, is_unit(Field)],
    %% format function match args
    Args = lists:concat(["#", Tag, "{", string:join([format("~s = ~s", [Names, Args]) || #field{names = Names, args = Args} <- List], ", "), "}"]),
    %% format function pack info
    Packs = string:join(take(#field.packs, List, []), ", "),
    #field{args = Args, packs = Packs, names = take(#field.names, List), meta = take(#field.meta, List)};
parse_write_unit(Tuple) when is_tuple(Tuple) andalso tuple_size(Tuple) > 0 ->
    %% format per unit
    List = [parse_write_unit(Field) || Field <- tuple_to_list(Tuple)],
    %% format function match args
    Args = lists:concat(["{", string:join([lists:concat([Args]) || #field{args = Args} <- List], ", "), "}"]),
    %% format function pack info
    Packs = string:join(take(#field.packs, List, []), ", "),
    #field{args = Args, packs = Packs, names = take(#field.names, List, []), meta = take(#field.meta, List, [])};
parse_write_unit(_) ->
    #field{}.

%%====================================================================
%% common tool
%%====================================================================
%% format
format(F, A) ->
    binary_to_list(list_to_binary(io_lib:format(F, A))).

%% lower_hump -> lowerHump
lower_hump(Name) ->
    [string:to_lower(hd(lists:concat([Name]))) | tl(maker:hump(lists:concat([Name])))].

%% take element from list
take(N, List) ->
    [element(N, T) || T <- List].
%% take element from list except value
take(N, List, E) ->
    [element(N, T) || T <- List, element(N, T) =/= E].

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
is_unit(#list{})   -> true;
is_unit(#ets{})    -> true;
is_unit(_)         -> false.
