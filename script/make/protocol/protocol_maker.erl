%%%-------------------------------------------------------------------
%%% @doc
%%% make protocol define to erl/lua/js/cs io/metadata code
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_maker).
-export([start/1]).
-include("../../../include/serialize.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% start parse
start(#protocol{comment = ProtocolComment, io = IO, erl = ErlFile, lua = LuaFile, js = JsFile, cs = CsFile, html = HtmlFile}) ->
    %% names
    ErlName = filename:basename(ErlFile, ".erl"),
    HtmlName = filename:basename(HtmlFile, ".html"),
    LuaName = filename:basename(LuaFile, ".lua"),
    JsName = filename:basename(JsFile, ".js"),
    CsName = filename:basename(CsFile, ".cs"),
    %% start collect code
    #data{erl = Erl, html = Html, lua = Lua, js = Js, cs = Cs} = collect_code(lists:reverse(IO), ProtocolComment, ErlName, LuaName, JsName, CsName, HtmlName, []),

    %% text code
    %% TextData = lists:concat(["-module(", filename:basename(TextFile, ".erl"), ").\n-export([text/3]).\n\n", TextCode]),
    %% file:write_file(maker:relative_path(TextFile), unicode:characters_to_binary(TextData)),

    #set{code = ErlCode, handler = ErlHandler} = Erl,

    %% erl file (file could not write when parameter not given)
    %% IncludeCode = [io_lib:format("-include(\"~s\").\n", [Include]) || Include <- Includes],
    ErlCodeFile = maker:relative_path(ErlFile),
    filelib:ensure_dir(ErlCodeFile),
    ErlFile =/= [] andalso file:write_file(ErlCodeFile, unicode:characters_to_binary(ErlCode)),
    %% handler
    ErlHandlerFile = maker:relative_path(lists:concat([filename:dirname(ErlFile), "/handler/", string:replace(ErlName, "_protocol", "_handler"), ".erl"])),
    filelib:ensure_dir(ErlHandlerFile),
    ErlHandlerFile =/= [] andalso file:write_file(ErlHandlerFile, unicode:characters_to_binary(ErlHandler)),

    %% lua file (file could not write when parameter not given)
    #set{code = LuaCode, meta = LuaMeta} = Lua,
    %% meta
    LuaMetaFile = maker:relative_path(lists:concat([filename:dirname(LuaFile), "/meta/", LuaName, ".lua"])),
    filelib:ensure_dir(LuaMetaFile),
    LuaFile =/= [] andalso file:write_file(LuaMetaFile, unicode:characters_to_binary(LuaMeta)),
    %% code
    LuaCodeFile = maker:relative_path(LuaFile),
    filelib:ensure_dir(LuaCodeFile),
    LuaFile =/= [] andalso file:write_file(LuaCodeFile, unicode:characters_to_binary(LuaCode)),

    %% js file (file could not write when parameter not given)
    #set{code = JsCode, meta = JsMeta} = Js,
    %% meta
    JsMetaFile = maker:relative_path(lists:concat([filename:dirname(JsFile), "/meta/", JsName, ".js"])),
    filelib:ensure_dir(JsMetaFile),
    JsFile =/= [] andalso file:write_file(JsMetaFile, unicode:characters_to_binary(JsMeta)),
    %% code
    JsCodeFile = maker:relative_path(JsFile),
    filelib:ensure_dir(JsCodeFile),
    JsFile =/= [] andalso file:write_file(JsCodeFile, unicode:characters_to_binary(JsCode)),

    %% cs file (file could not write when parameter not given)
    #set{code = CsCode, meta = CsMeta} = Cs,
    %% meta
    CsMetaFile = maker:relative_path(lists:concat([filename:dirname(CsFile), "/meta/", CsName, ".cs"])),
    filelib:ensure_dir(CsMetaFile),
    CsFile =/= [] andalso file:write_file(CsMetaFile, unicode:characters_to_binary(CsMeta)),
    %% code
    CsCodeFile = maker:relative_path(CsFile),
    filelib:ensure_dir(CsCodeFile),
    CsFile =/= [] andalso file:write_file(CsCodeFile, unicode:characters_to_binary(CsCode)),

    %% html code (file could not write when parameter not given)
    #set{meta = HtmlCode} = Html,
    HtmlCodeFile = maker:relative_path(HtmlFile),
    filelib:ensure_dir(HtmlCodeFile),
    HtmlFile =/= [] andalso file:write_file(HtmlCodeFile, unicode:characters_to_binary(HtmlCode)),

    ok.

%% collect code
collect_code([], ProtocolComment, ErlName, LuaName, JsName, CsName, HtmlName, List) ->
    DecodeList = [Code || {_, _, Code, _} <- List],
    EncodeList = [Code || {_, _, _, Code} <- List],

    %% erl server side
    ErlCode = protocol_maker_erl:format_code(ErlName, DecodeList, EncodeList),
    ErlHandlerCode = protocol_maker_erl:format_handler(ErlName, DecodeList, EncodeList),
    Erl = #set{code = ErlCode, handler = ErlHandlerCode},

    %% lua client side
    LuaCode = protocol_maker_lua:format_code(LuaName, DecodeList, EncodeList),
    LuaMeta = protocol_maker_lua:format_meta(LuaName, List),
    Lua = #set{code = LuaCode, meta = LuaMeta},

    %% js client side
    JsCode = protocol_maker_js:format_code(JsName, DecodeList, EncodeList),
    JsMeta = protocol_maker_js:format_meta(JsName, List),
    Js = #set{code = JsCode, meta = JsMeta},

    %% cs client side
    CsCode = protocol_maker_cs:format_code(CsName, DecodeList, EncodeList),
    CsMeta = protocol_maker_cs:format_meta(CsName, List),
    Cs = #set{code = CsCode, meta = CsMeta},

    %% html
    HtmlCode = protocol_maker_html:format(ProtocolComment, HtmlName, List),
    Html = #set{meta = HtmlCode},

    %% return code sets
    #data{erl = Erl, lua = Lua, js = Js, cs = Cs, html = Html};

collect_code([#io{number = Protocol, comment = Comment, handler = Handler, decode = Decode, encode = Encode} | T], ProtocolComment, ErlName, LuaName, JsName, CsName, HtmlName, List) ->
    DecodeCode = parse_decode(Protocol, Decode, Handler, ErlName),
    EncodeCode = parse_encode(Protocol, Encode, Handler, ErlName),
    collect_code(T, ProtocolComment, ErlName, LuaName, JsName, CsName, HtmlName, [{Protocol, Comment, DecodeCode, EncodeCode} | List]).

%%%===================================================================
%%% Parse Decode Part
%%%===================================================================
parse_decode(Protocol, Form, undefined, Name) ->
    %% no handler
    parse_decode(Protocol, Form, #handler{}, Name);

parse_decode(0, undefined, Handler = #handler{}, _) ->
    ErlRequestCode = protocol_maker_erl:parse_request_erl(0, [], Handler),
    Erl = #set{handler = ErlRequestCode},
    Lua = #set{meta = "{}"},
    Js = #set{meta = "[]"},
    Cs = #set{meta = "{}"},
    Html = #set{},
    #data{protocol = 0, erl = Erl, lua = Lua, js = Js, cs = Cs, html = Html};

parse_decode(Protocol, undefined, _, _) ->
    Erl = #set{},
    Lua = #set{meta = "{}"},
    Js = #set{meta = "[]"},
    Cs = #set{meta = "{}"},
    Html = #set{},
    #data{protocol = Protocol, erl = Erl, lua = Lua, js = Js, cs = Cs, html = Html};

parse_decode(Protocol, Meta, Handler, Name)->
    %% erl decode
    ErlCode = protocol_maker_erl:parse_decode_erl(Protocol, Meta),
    ErlRequestCode = protocol_maker_erl:parse_request_erl(Protocol, Meta, Handler),
    Erl = #set{code = ErlCode, handler = ErlRequestCode},
    %% lua
    #file{import = LuaImport} = protocol_maker_lua:parse_encode_class(Protocol, Meta, Handler, Name),
    LuaCode = protocol_maker_lua:parse_encode(Protocol, Meta),
    LuaMeta = protocol_maker_lua:parse_meta(Protocol, Meta),
    Lua = #set{code = LuaCode#file{import = LuaImport}, meta = LuaMeta},
    %% js
    #file{import = JsImport} = protocol_maker_js:parse_encode_class(Protocol, Meta, Handler, Name),
    JsCode = protocol_maker_js:parse_encode(Protocol, Meta),
    JsMeta = protocol_maker_js:parse_meta(Protocol, Meta),
    Js = #set{code = JsCode#file{import = JsImport}, meta = JsMeta},
    %% cs
    #file{import = CsImport} = protocol_maker_cs:parse_encode_class(Protocol, Meta, Handler, Name),
    CsCode = protocol_maker_cs:parse_encode(Protocol, Meta, Handler, Name),
    CsMeta = protocol_maker_cs:parse_meta(Protocol, Meta),
    Cs = #set{code = CsCode#file{import = CsImport}, meta = CsMeta},
    %% html
    HtmlCode = protocol_maker_html:parse_code_html(Protocol, Meta),
    Html = #set{meta = HtmlCode},
    %% code set
    #data{protocol = Protocol, erl = Erl, lua = Lua, js = Js, cs = Cs, html = Html}.


%%%===================================================================
%%% Parse Encode Part
%%%===================================================================
parse_encode(0, _, _, _) ->
    #data{protocol = 0};

parse_encode(Protocol, undefined, _, _) ->
    Erl = #set{},
    Lua = #set{meta = "{}"},
    Js = #set{meta = "[]"},
    Cs = #set{meta = "{}"},
    Html = #set{},
    #data{protocol = Protocol, erl = Erl, lua = Lua, js = Js, cs = Cs, html = Html};

parse_encode(Protocol, Meta, Handler, Name) ->
    %% erl encode
    ErlCode = protocol_maker_erl:parse_encode_erl(Protocol, Meta),
    ErlResponseCode = protocol_maker_erl:parse_response_erl(Protocol, Meta, Handler, Name),
    Erl = #set{code = ErlCode, handler = ErlResponseCode},
    %% lua
    #file{export = LuaExport} = protocol_maker_lua:parse_decode_class(Protocol, Meta, Handler, Name),
    LuaCode = protocol_maker_lua:parse_decode(Protocol, Meta),
    LuaMeta = protocol_maker_lua:parse_meta(Protocol, Meta),
    Lua = #set{code = LuaCode#file{export = LuaExport}, meta = LuaMeta},
    %% js
    #file{export = JsExport} = protocol_maker_js:parse_decode_class(Protocol, Meta, Handler, Name),
    JsCode = protocol_maker_js:parse_decode(Protocol, Meta),
    JsMeta = protocol_maker_js:parse_meta(Protocol, Meta),
    Js = #set{code = JsCode#file{export = JsExport}, meta = JsMeta},
    %% cs
    #file{export = CsExport} = protocol_maker_cs:parse_decode_class(Protocol, Meta, Handler, Name),
    CsCode = protocol_maker_cs:parse_decode(Protocol, Meta, Handler, Name),
    CsMeta = protocol_maker_cs:parse_meta(Protocol, Meta),
    Cs = #set{code = CsCode#file{export = CsExport}, meta = CsMeta},
    %% html
    HtmlCode = protocol_maker_html:parse_code_html(Protocol, Meta),
    Html = #set{meta = HtmlCode},
    %% code set
    #data{protocol = Protocol, erl = Erl, lua = Lua, js = Js, cs = Cs, html = Html}.
