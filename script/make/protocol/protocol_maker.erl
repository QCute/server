%%%-------------------------------------------------------------------
%%% @doc
%%% make protocol define to erl/js/lua io/metadata code
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_maker).
-export([start/1]).
-include("../../../include/serialize.hrl").
%% ast metadata
-record(meta, {name = [], type, comment = [], explain = [], key}).
%% lang code
-record(code, {protocol = 0, erl = [], handler = [], html = [], lua_code = [], lua_meta = [], js_code = [], js_meta = [], cs_code = [], cs_meta = []}).
%%%===================================================================
%%% API functions
%%%===================================================================
%% start parse
start(#protocol{comment = ProtocolComment, handler = HandlerFile, includes = Includes, io = IO, erl = ErlFile, html = HtmlFile, lua = LuaFile, js = JsFile, cs = CsFile}) ->
    %% names
    ErlName = filename:basename(ErlFile, ".erl"),
    HandlerName = filename:basename(HandlerFile, ".erl"),
    HtmlName = filename:basename(HtmlFile, ".html"),
    LuaName = filename:basename(LuaFile, ".lua"),
    JsName = filename:basename(JsFile, ".js"),
    CsName = filename:basename(CsFile, ".cs"),
    %% start collect code
    #code{erl = ErlCode, handler = HandlerCode, html = HtmlCode, lua_code = LuaCode, lua_meta = LuaMeta, js_code = JsCode, js_meta = JsMeta, cs_code = CsCode, cs_meta = CsMeta} = collect_code(lists:reverse(IO), ProtocolComment, HtmlName, LuaName, JsName, CsName, []),

    %% handler file (file could not write when parameter not given)
    HandlerCodeData = lists:concat([
        "-module(", HandlerName, ").", "\n",
        "-export([handle/3]).", "\n",
        "\n",
        HandlerCode
    ]),
    HandlerCodeFile = maker:relative_path(HandlerFile),
    filelib:ensure_dir(HandlerCodeFile),
    HandlerFile =/= [] andalso file:write_file(HandlerCodeFile, HandlerCodeData),

    %% text code
    %% TextData = lists:concat(["-module(", filename:basename(TextFile, ".erl"), ").\n-export([text/3]).\n\n", TextCode]),
    %% file:write_file(maker:relative_path(TextFile), unicode:characters_to_binary(TextData)),

    %% erl file (file could not write when parameter not given)
    IncludeCode = [io_lib:format("-include(\"~s\").\n", [Include]) || Include <- Includes],
    ErlCodeData = lists:concat([
        "-module(", ErlName, ").", "\n",
        "-export([decode/2, encode/2]).", "\n",
        IncludeCode, ErlCode
    ]),
    ErlCodeFile = maker:relative_path(ErlFile),
    filelib:ensure_dir(ErlCodeFile),
    ErlFile =/= [] andalso file:write_file(ErlCodeFile, unicode:characters_to_binary(ErlCodeData)),

    %% html code (file could not write when parameter not given)
    HtmlCodeFile = maker:relative_path(HtmlFile),
    filelib:ensure_dir(HtmlCodeFile),
    HtmlFile =/= [] andalso file:write_file(HtmlCodeFile, unicode:characters_to_binary(HtmlCode)),

    %% lua file (file could not write when parameter not given)
    %% meta
    LuaMetaData = lists:concat(["", word:to_lower_hump(LuaName), " = ", LuaMeta]),
    LuaMetaFile = maker:relative_path(lists:concat([filename:dirname(LuaFile), "/meta/", LuaName, ".lua"])),
    filelib:ensure_dir(LuaMetaFile),
    LuaFile =/= [] andalso file:write_file(LuaMetaFile, unicode:characters_to_binary(LuaMetaData)),
    %% code
    LuaCodeFile = maker:relative_path(LuaFile),
    filelib:ensure_dir(LuaCodeFile),
    LuaFile =/= [] andalso file:write_file(LuaCodeFile, unicode:characters_to_binary(LuaCode)),

    %% js file (file could not write when parameter not given)
    %% meta
    JsMetaData = lists:concat(["export default ", JsMeta, ";"]),
    JsMetaFile = maker:relative_path(lists:concat([filename:dirname(JsFile), "/meta/", JsName, ".js"])),
    filelib:ensure_dir(JsMetaFile),
    JsFile =/= [] andalso file:write_file(JsMetaFile, unicode:characters_to_binary(JsMetaData)),
    %% code
    JsCodeFile = maker:relative_path(JsFile),
    filelib:ensure_dir(JsCodeFile),
    JsFile =/= [] andalso file:write_file(JsCodeFile, unicode:characters_to_binary(JsCode)),

    %% cs file (file could not write when parameter not given)
    %% meta
    CsMetaFile = maker:relative_path(lists:concat([filename:dirname(CsFile), "/meta/", CsName, ".cs"])),
    filelib:ensure_dir(CsMetaFile),
    CsFile =/= [] andalso file:write_file(CsMetaFile, unicode:characters_to_binary(CsMeta)),
    %% code
    CsCodeFile = maker:relative_path(CsFile),
    filelib:ensure_dir(CsCodeFile),
    CsFile =/= [] andalso file:write_file(CsCodeFile, unicode:characters_to_binary(CsCode)),

    ok.

%% collect code
collect_code([], ProtocolComment, HtmlName, LuaName, JsName, CsName, List) ->
    DecodeList = [Code || {_, _, Code, _} <- List],
    EncodeList = [Code || {_, _, _, Code} <- List],

    %% erl server side
    ErlCode = protocol_maker_erl:format_code(DecodeList, EncodeList),
    HandlerCode = protocol_maker_erl:format_handler(DecodeList),

    %% html
    HtmlCode = protocol_maker_html:format(ProtocolComment, HtmlName, List),

    %% lua client side
    LuaCode = protocol_maker_lua:format_code(LuaName, DecodeList, EncodeList),
    LuaMeta = protocol_maker_lua:format_meta(LuaName, List),

    %% js client side
    JsCode = protocol_maker_js:format_code(JsName, DecodeList, EncodeList),
    JsMeta = protocol_maker_js:format_meta(JsName, List),

    %% cs client side
    CsCode = protocol_maker_cs:format_code(CsName, DecodeList, EncodeList),
    CsMeta = protocol_maker_cs:format_meta(CsName, List),

    %% return code sets
    #code{erl = ErlCode, handler = HandlerCode, html = HtmlCode, lua_code = LuaCode, lua_meta = LuaMeta, js_code = JsCode, js_meta = JsMeta, cs_code = CsCode, cs_meta = CsMeta};

collect_code([#io{protocol = Protocol, comment = Comment, handler = Handler, read = Read, write = Write} | T], ProtocolComment, HtmlName, LuaName, JsName, CsName, List) ->
    DecodeCode = parse_decode(Protocol, Read, Handler),
    EncodeCode = parse_encode(Protocol, Write),
    collect_code(T, ProtocolComment, HtmlName, LuaName, JsName, CsName, [{Protocol, Comment, DecodeCode, EncodeCode} | List]).

%%%===================================================================
%%% Parse Decode Part
%%%===================================================================
parse_decode(Protocol, SyntaxList, undefined) ->
    %% no handler
    parse_decode(Protocol, SyntaxList, #handler{});

parse_decode(0, undefined, Handler = #handler{}) ->
    HandlerCode = protocol_maker_erl:parse_handler_erl(0, [], Handler),
    #code{protocol = 0, erl = [], handler = HandlerCode, lua_meta = "{}", js_meta = "[]", cs_meta = "{}"};

parse_decode(Protocol, undefined, _) ->
    #code{protocol = Protocol, erl = [], lua_meta = "{}", js_meta = "[]", cs_meta = "{}"};

parse_decode(Protocol, SyntaxList, Handler) when is_list(SyntaxList) ->
    MetaList = [parse_decode_unit(Syntax) || Syntax <- SyntaxList],
    %% erl decode
    ErlCode = protocol_maker_erl:parse_decode_erl(Protocol, MetaList),
    HandlerCode = protocol_maker_erl:parse_handler_erl(Protocol, MetaList, Handler),
    %% html
    HtmlCode = protocol_maker_html:parse_code_html(Protocol, MetaList),
    %% lua
    LuaCode = protocol_maker_lua:parse_encode_lua(Protocol, MetaList),
    LuaMeta = protocol_maker_lua:parse_meta_lua(Protocol, MetaList),
    %% js
    JsCode = protocol_maker_js:parse_encode_js(Protocol, MetaList),
    JsMeta = protocol_maker_js:parse_meta_js(Protocol, MetaList),
    %% cs
    CsCode = protocol_maker_cs:parse_encode_cs(Protocol, MetaList),
    CsMeta = protocol_maker_cs:parse_meta_cs(Protocol, MetaList),
    %% code set
    #code{protocol = Protocol, erl = ErlCode, handler = HandlerCode, html = HtmlCode, lua_code = LuaCode, lua_meta = LuaMeta, js_code = JsCode, js_meta = JsMeta, cs_code = CsCode, cs_meta = CsMeta};

parse_decode(Protocol, _, _) ->
    #code{protocol = Protocol}.

%% parse unit
parse_decode_unit(Unit = #binary{name = Name, comment = Comment, explain = Explain}) ->
    HumpName = word:to_hump(Name),
    #meta{name = HumpName, type = element(1, Unit), comment = Comment, explain = Explain};

parse_decode_unit(Unit = #bool{name = Name, comment = Comment}) ->
    HumpName = word:to_hump(Name),
    #meta{name = HumpName, type = element(1, Unit), comment = Comment, explain = []};

parse_decode_unit(Unit = #u8{name = Name, comment = Comment}) ->
    HumpName = word:to_hump(Name),
    #meta{name = HumpName, type = element(1, Unit), comment = Comment, explain = []};

parse_decode_unit(Unit = #u16{name = Name, comment = Comment}) ->
    HumpName = word:to_hump(Name),
    #meta{name = HumpName, type = element(1, Unit), comment = Comment, explain = []};

parse_decode_unit(Unit = #u32{name = Name, comment = Comment}) ->
    HumpName = word:to_hump(Name),
    #meta{name = HumpName, type = element(1, Unit), comment = Comment, explain = []};

parse_decode_unit(Unit = #u64{name = Name, comment = Comment}) ->
    HumpName = word:to_hump(Name),
    #meta{name = HumpName, type = element(1, Unit), comment = Comment, explain = []};

parse_decode_unit(Unit = #u128{name = Name, comment = Comment}) ->
    HumpName = word:to_hump(Name),
    #meta{name = HumpName, type = element(1, Unit), comment = Comment, explain = []};

parse_decode_unit(Unit = #i8{name = Name, comment = Comment}) ->
    HumpName = word:to_hump(Name),
    #meta{name = HumpName, type = element(1, Unit), comment = Comment, explain = []};

parse_decode_unit(Unit = #i16{name = Name, comment = Comment}) ->
    HumpName = word:to_hump(Name),
    #meta{name = HumpName, type = element(1, Unit), comment = Comment, explain = []};

parse_decode_unit(Unit = #i32{name = Name, comment = Comment}) ->
    HumpName = word:to_hump(Name),
    #meta{name = HumpName, type = element(1, Unit), comment = Comment, explain = []};

parse_decode_unit(Unit = #i64{name = Name, comment = Comment}) ->
    HumpName = word:to_hump(Name),
    #meta{name = HumpName, type = element(1, Unit), comment = Comment, explain = []};

parse_decode_unit(Unit = #i128{name = Name, comment = Comment}) ->
    HumpName = word:to_hump(Name),
    #meta{name = HumpName, type = element(1, Unit), comment = Comment, explain = []};

parse_decode_unit(Unit = #f32{name = Name, comment = Comment}) ->
    HumpName = word:to_hump(Name),
    #meta{name = HumpName, type = element(1, Unit), comment = Comment, explain = []};

parse_decode_unit(Unit = #f64{name = Name, comment = Comment}) ->
    HumpName = word:to_hump(Name),
    #meta{name = HumpName, type = element(1, Unit), comment = Comment, explain = []};

parse_decode_unit(Unit = #bst{name = Name, comment = Comment}) ->
    HumpName = word:to_hump(Name),
    #meta{name = HumpName, type = element(1, Unit), comment = Comment, explain = []};

parse_decode_unit(Unit = #str{name = Name, comment = Comment}) ->
    HumpName = word:to_hump(Name),
    #meta{name = HumpName, type = element(1, Unit), comment = Comment, explain = []};

parse_decode_unit(Unit = #list{name = Name, comment = Comment, explain = Explain, key = Key}) ->
    %% hump name
    HumpName = word:to_hump(Name),
    %% format subunit
    SubMeta = parse_decode_unit(Explain),
    #meta{name = HumpName, type = element(1, Unit), comment = Comment, explain = lists:flatten([SubMeta]), key = Key};

%% structure unit
parse_decode_unit(Unit = #tuple{name = Name, comment = Comment, explain = Explain}) ->
    %% format per unit
    List = [parse_decode_unit(Field) || Field <- tuple_to_list(Explain)],
    #meta{name = Name, type = element(1, Unit), comment = Comment, explain = List};

parse_decode_unit(Record) when is_tuple(Record) andalso is_atom(element(1, Record)) ->

    %% get beam abstract code
    Tag = element(1, Record),
    NameList = beam:find(Tag),
    %% error when beam abstract code empty
    NameList == [] andalso erlang:throw(need_to_update_beam_abstract_code),
    tuple_size(Record) =/= length(NameList) andalso erlang:throw(need_to_update_beam_abstract_code),

    %% hump name
    HumpName = word:to_hump(Tag),

    %% zip field value and field name
    ZipList = lists:zip(tuple_to_list(Record), NameList),

    %% format per unit
    List = [parse_decode_unit(case element(2, Field) of [] -> setelement(2, Field, Name); _ -> Field end) || {Field, Name} <- ZipList, is_unit(Field)],
    KeyNameList = [Name || {Field, Name} <- ZipList, is_unit(Field)],
    #meta{name = HumpName, type = record, comment = HumpName, explain = List, key = KeyNameList};

parse_decode_unit(Tuple) when is_tuple(Tuple) andalso tuple_size(Tuple) > 0 ->
    %% format per unit
    List = [parse_decode_unit(Field) || Field <- tuple_to_list(Tuple)],
    #meta{name = "", type = tuple, comment = "", explain = List};

parse_decode_unit(Unit) ->
    erlang:throw(lists:flatten(io_lib:format("Unsupported decode unit: ~tp", [Unit]))).

%%%===================================================================
%%% Parse Encode Part
%%%===================================================================
parse_encode(0, _) ->
    #code{protocol = 0};

parse_encode(Protocol, undefined) ->
    #code{protocol = Protocol, erl = [], lua_meta = "{}", js_meta = "[]", cs_meta = "{}"};

parse_encode(Protocol, SyntaxList) when is_list(SyntaxList) ->
    MetaList = [parse_encode_unit(Syntax) || Syntax <- SyntaxList],
    %% erl encode
    ErlCode = protocol_maker_erl:parse_encode_erl(Protocol, MetaList),
    %% html
    HtmlCode = protocol_maker_html:parse_code_html(Protocol, MetaList),
    %% lua
    LuaCode = protocol_maker_lua:parse_decode_lua(Protocol, MetaList),
    LuaMeta = protocol_maker_lua:parse_meta_lua(Protocol, MetaList),
    %% js
    JsCode = protocol_maker_js:parse_decode_js(Protocol, MetaList),
    JsMeta = protocol_maker_js:parse_meta_js(Protocol, MetaList),
    %% cs
    CsCode = protocol_maker_cs:parse_decode_cs(Protocol, MetaList),
    CsMeta = protocol_maker_cs:parse_meta_cs(Protocol, MetaList),
    %% code set
    #code{protocol = Protocol, erl = ErlCode, handler = [], html = HtmlCode, lua_code = LuaCode, lua_meta = LuaMeta, js_code = JsCode, js_meta = JsMeta, cs_code = CsCode, cs_meta = CsMeta};

parse_encode(Protocol, _) ->
    #code{protocol = Protocol}.

%% parse unit
parse_encode_unit(Unit = #zero{}) ->
    #meta{name = "", type = element(1, Unit), comment = "", explain = []};

parse_encode_unit(Unit = #binary{name = Name, comment = Comment, explain = Explain}) ->
    HumpName = word:to_hump(Name),
    #meta{name = HumpName, type = element(1, Unit), comment = Comment, explain = Explain};

parse_encode_unit(Unit = #bool{name = Name, comment = Comment}) ->
    HumpName = word:to_hump(Name),
    #meta{name = HumpName, type = element(1, Unit), comment = Comment, explain = []};

parse_encode_unit(Unit = #u8{name = Name, comment = Comment}) ->
    HumpName = word:to_hump(Name),
    #meta{name = HumpName, type = element(1, Unit), comment = Comment, explain = []};

parse_encode_unit(Unit = #u16{name = Name, comment = Comment}) ->
    HumpName = word:to_hump(Name),
    #meta{name = HumpName, type = element(1, Unit), comment = Comment, explain = []};

parse_encode_unit(Unit = #u32{name = Name, comment = Comment}) ->
    HumpName = word:to_hump(Name),
    #meta{name = HumpName, type = element(1, Unit), comment = Comment, explain = []};

parse_encode_unit(Unit = #u64{name = Name, comment = Comment}) ->
    HumpName = word:to_hump(Name),
    #meta{name = HumpName, type = element(1, Unit), comment = Comment, explain = []};

parse_encode_unit(Unit = #u128{name = Name, comment = Comment}) ->
    HumpName = word:to_hump(Name),
    #meta{name = HumpName, type = element(1, Unit), comment = Comment, explain = []};

parse_encode_unit(Unit = #i8{name = Name, comment = Comment}) ->
    HumpName = word:to_hump(Name),
    #meta{name = HumpName, type = element(1, Unit), comment = Comment, explain = []};

parse_encode_unit(Unit = #i16{name = Name, comment = Comment}) ->
    HumpName = word:to_hump(Name),
    #meta{name = HumpName, type = element(1, Unit), comment = Comment, explain = []};

parse_encode_unit(Unit = #i32{name = Name, comment = Comment}) ->
    HumpName = word:to_hump(Name),
    #meta{name = HumpName, type = element(1, Unit), comment = Comment, explain = []};

parse_encode_unit(Unit = #i64{name = Name, comment = Comment}) ->
    HumpName = word:to_hump(Name),
    #meta{name = HumpName, type = element(1, Unit), comment = Comment, explain = []};

parse_encode_unit(Unit = #i128{name = Name, comment = Comment}) ->
    HumpName = word:to_hump(Name),
    #meta{name = HumpName, type = element(1, Unit), comment = Comment, explain = []};

parse_encode_unit(Unit = #f32{name = Name, comment = Comment}) ->
    HumpName = word:to_hump(Name),
    #meta{name = HumpName, type = element(1, Unit), comment = Comment, explain = []};

parse_encode_unit(Unit = #f64{name = Name, comment = Comment}) ->
    HumpName = word:to_hump(Name),
    #meta{name = HumpName, type = element(1, Unit), comment = Comment, explain = []};

parse_encode_unit(Unit = #rst{name = Name, comment = Comment}) ->
    HumpName = word:to_hump(Name),
    #meta{name = HumpName, type = element(1, Unit), comment = Comment, explain = []};

parse_encode_unit(Unit = #bst{name = Name, comment = Comment}) ->
    HumpName = word:to_hump(Name),
    #meta{name = HumpName, type = element(1, Unit), comment = Comment, explain = []};

parse_encode_unit(Unit = #str{name = Name, comment = Comment}) ->
    HumpName = word:to_hump(Name),
    #meta{name = HumpName, type = element(1, Unit), comment = Comment, explain = []};

parse_encode_unit(Unit = #list{name = Name, comment = Comment, explain = Explain, key = Key}) ->
    %% hump name
    HumpName = word:to_hump(Name),
    %% format subunit
    SubMeta = parse_encode_unit(Explain),
    #meta{name = HumpName, type = element(1, Unit), comment = Comment, explain = lists:flatten([SubMeta]), key = Key};

parse_encode_unit(Unit = #ets{name = Name, comment = Comment, explain = Explain, key = Key}) ->
    %% hump name
    HumpName = word:to_hump(Name),
    %% format subunit
    SubMeta = parse_encode_unit(Explain),
    #meta{name = HumpName, type = element(1, Unit), comment = Comment, explain = lists:flatten([SubMeta]), key = Key};

%% structure unit
parse_encode_unit(#record{explain = Explain}) ->
    %% format per unit
    parse_encode_unit(Explain);

%% structure unit
parse_encode_unit(Unit = #tuple{name = Name, comment = Comment, explain = Explain}) ->
    HumpName = word:to_hump(Name),
    %% format per unit
    List = [parse_encode_unit(Field) || Field <- tuple_to_list(Explain)],
    #meta{name = HumpName, type = element(1, Unit), comment = Comment, explain = List};

parse_encode_unit(Record) when is_tuple(Record) andalso tuple_size(Record) > 0 andalso is_atom(element(1, Record)) ->
    %% get beam abstract code
    Tag = element(1, Record),
    NameList = beam:find(Tag),
    %% throw error when beam abstract code empty
    NameList == [] andalso erlang:throw(need_to_update_beam_abstract_code),
    tuple_size(Record) =/= length(NameList) andalso erlang:throw(need_to_update_beam_abstract_code),

    %% hump name
    HumpName = word:to_hump(Tag),

    %% zip field value and field name
    ZipList = lists:zip(tuple_to_list(Record), NameList),
    %% format per unit
    List = [parse_encode_unit(case element(2, Field) of [] -> setelement(2, Field, Name); _ -> Field end) || {Field, Name} <- ZipList, is_unit(Field)],
    KeyNameList = [Name || {Field, Name} <- ZipList, is_unit(Field)],
    #meta{name = HumpName, type = record, comment = HumpName, explain = List, key = KeyNameList};

parse_encode_unit(Tuple) when is_tuple(Tuple) andalso tuple_size(Tuple) > 0 ->
    %% format per unit
    List = [parse_encode_unit(Field) || Field <- tuple_to_list(Tuple)],
    #meta{name = "", type = tuple, comment = "", explain = List};

parse_encode_unit(Unit) ->
    erlang:throw(lists:flatten(io_lib:format("Unsupported encode unit: ~tp", [Unit]))).

%%%===================================================================
%%% Common Tool
%%%===================================================================

%% is bit unit
is_unit(#u8{}) -> true;
is_unit(#u16{}) -> true;
is_unit(#u32{}) -> true;
is_unit(#u64{}) -> true;
is_unit(#u128{}) -> true;
is_unit(#i8{}) -> true;
is_unit(#i16{}) -> true;
is_unit(#i32{}) -> true;
is_unit(#i64{}) -> true;
is_unit(#i128{}) -> true;
is_unit(#rst{}) -> true;
is_unit(#bst{}) -> true;
is_unit(#str{}) -> true;
is_unit(#binary{}) -> true;
is_unit(#list{}) -> true;
is_unit(#ets{}) -> true;
is_unit(#record{}) -> true;
is_unit(#tuple{}) -> true;
is_unit(_) -> false.
