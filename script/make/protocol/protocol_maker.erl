%%%-------------------------------------------------------------------
%%% @doc
%%% make protocol define to erl/js/lua io/metadata code
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_maker).
-export([start/1]).
-include("../../../include/serialize.hrl").
%% syntax field record
-record(field, {name = [], default = [], meta = [], args = [], procedures = [], packs = []}).
%% ast metadata
-record(meta, {name = [], type, comment = [], explain = [], key}).
%% lang code
-record(code, {handler = [], erl = [], html = [], lua_code = [], lua_meta = [], js_code = [], js_meta = [], cs_code = [], cs_meta = []}).
%%%===================================================================
%%% API functions
%%%===================================================================
%% start parse
start(#protocol{comment = ProtocolComment, handler = HandlerFile, includes = Includes, io = IO, erl = ErlFile, html = HtmlFile, lua = LuaFile, js = JsFile, cs = CsFile}) ->
    %% put default
    Default = listing:key_find(0, #io.protocol, IO, #io{}),
    IOList = [Default | lists:reverse(lists:keydelete(0, #io.protocol, IO))],
    %% names
    HandlerName = filename:basename(HandlerFile, ".erl"),
    ErlName = filename:basename(ErlFile, ".erl"),
    HtmlName = filename:basename(HtmlFile, ".html"),
    LuaName = filename:basename(LuaFile, ".lua"),
    JsName = filename:basename(JsFile, ".js"),
    CsName = filename:basename(CsFile, ".cs"),
    %% start collect code
    #code{handler = HandlerCode, erl = ErlCode, html = HtmlCode, lua_code = LuaCode, lua_meta = LuaMeta, js_code = JsCode, js_meta = JsMeta, cs_code = CsCode, cs_meta = CsMeta} = collect_code(IOList, ProtocolComment, HtmlName, LuaName, JsName, CsName, []),

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
        "-export([read/2, write/2]).", "\n",
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
    ReadList = [ReadCode || {_, _, ReadCode, _} <- List],
    WriteList = [WriteCode || {_, _, _, WriteCode} <- List],
    %% handler code
    %% DefaultHandler = case listing:collect(#code.default_handler, ReadList, []) of [] -> "handle(_, Protocol, Data) ->\n    {error, Protocol, Data}.\n"; HandlerList -> HandlerList end,
    Handler = lists:concat([listing:collect(#code.handler, ReadList, [])]),
    %% @todo handler spec
    %% text code
    %% Text = lists:concat(["text(_, ok, _) ->\n    <<0:16>>;\n", listing:collect(#code.text, WriteList, []), "text(_, Key, _) ->\n    protocol:write_binary(type:to_binary(Key)).\n"]),
    %% result text code
    %% Result = lists:append(listing:collect(#code.result, WriteList, [])),
    %% collect all text into protocol file, no text if not set
    %% erl code
    ErlRead = listing:collect(#code.erl, ReadList, []),
    ErlWrite = listing:collect(#code.erl, WriteList, []),
    Erl = lists:concat([
        "\n\n",
        "-spec read(Protocol :: non_neg_integer(), Binary :: binary()) -> {ok, [integer() | binary() | list()]} | {error, Protocol :: non_neg_integer(), Binary :: binary()}.",
        "\n",
        ErlRead,
        "read(Protocol, Binary) ->\n    {error, Protocol, Binary}.\n",
        "\n\n",
        "-spec write(Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()) -> {ok, binary()} | {error, Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()}.",
        "\n",
        ErlWrite,
        "write(Protocol, Data) ->\n    {error, Protocol, Data}.\n",
        "\n\n"
    ]),
    %% html metadata
    %% HtmlRead = listing:collect(#code.html, ReadList, []),
    %% HtmlWrite = listing:collect(#code.html, WriteList, []),
    Html = lists:concat(["<!DOCTYPE html>
<html>
<head>
    <meta charset='UTF-8'>
    <link rel='icon' href='data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiPz48c3ZnIHdpZHRoPSIyNCIgaGVpZ2h0PSIyNCIgdmlld0JveD0iMCAwIDQ4IDQ4IiBmaWxsPSJub25lIiB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciPjxyZWN0IHg9IjgiIHk9IjQiIHdpZHRoPSIzMiIgaGVpZ2h0PSI0MCIgcng9IjIiIHN0cm9rZT0iIzMzMyIgc3Ryb2tlLXdpZHRoPSI0IiBzdHJva2UtbGluZWNhcD0icm91bmQiIHN0cm9rZS1saW5lam9pbj0icm91bmQiLz48cGF0aCBkPSJNMTYgNEgyNVYyMEwyMC41IDE2TDE2IDIwVjRaIiBmaWxsPSJub25lIiBzdHJva2U9IiMzMzMiIHN0cm9rZS13aWR0aD0iNCIgc3Ryb2tlLWxpbmVjYXA9InJvdW5kIiBzdHJva2UtbGluZWpvaW49InJvdW5kIi8+PHBhdGggZD0iTTE2IDI4SDI2IiBzdHJva2U9IiMzMzMiIHN0cm9rZS13aWR0aD0iNCIgc3Ryb2tlLWxpbmVjYXA9InJvdW5kIi8+PHBhdGggZD0iTTE2IDM0SDMyIiBzdHJva2U9IiMzMzMiIHN0cm9rZS13aWR0aD0iNCIgc3Ryb2tlLWxpbmVjYXA9InJvdW5kIi8+PC9zdmc+' type='image/x-icon' />
    <title>", HtmlName, "</title>
    <style>
        html, body { margin: 0; width: 100vw; height: 100vh; display: flex; }
        body { opacity: 0; animation: fade-in 1s forwards; }
        @keyframes fade-in { 0% { opacity: 0; } 100% { opacity: 1; } }
        div { display: flex; }
        a { color: #fff; text-decoration: none; }

        .left {
            width: 250px;
            height: 100vh;
            overflow: auto;
            flex-direction: column;
            color: #000;
            background-color: #f4f5f7;
        }

        .left > .protocol {
            width: 100%;
            padding: 8px 0px 8px 0px;
            margin-bottom: 2%;
            flex-shrink: 0;
            display: flex;
            justify-content: center;
            align-items: center;
            font-size: 0.88em;
        }

        .left > .protocol:hover {
            background-color: #eaecf0;
            cursor: pointer;
        }

        .left > .protocol > .inner {
            width: 80%;
            height: 100%;
            display: flex;
            align-items: center;
            font-size: 0.88em;
        }

        .right {
            width: calc(100vw - 250px);
            height: 100vh;
            overflow: auto;
            display: flex;
            flex-direction: column;
            align-items: center;
            background-color: #fff;
        }

        .right > .header {
            margin-bottom: 48px;
            font-size: 2em;
            font-weight: bold;
        }

        .right > .title {
            width: 90%;
            margin-bottom: 24px;
            font-size: 0.88em;
            font-weight: bold;
        }

        .right > .protocol {
            width: 80%;
            margin-bottom: 48px;
            display: flex;
            flex-direction: column;
            align-items: center;
        }

        .right > .protocol > .title {
            width: 100%;
            margin-bottom: 24px;
            display: flex;
            flex-direction: column;
            align-items: flex-start;
            font-size: 0.82em;
            font-weight: 500;
        }

        .right > .protocol > .read, 
        .right > .protocol > .write {
            width: 100%;
            margin-bottom: 24px;
            display: flex;
            flex-direction: column;
            align-items: flex-end;
        }

        .right > .protocol > .read  .field, 
        .right > .protocol > .write  .field {
            width: 100%;
            display: flex;
            flex-direction: column;
            align-items: flex-end;
        }

        .right > .protocol > .read .digest, 
        .right > .protocol > .write .digest {
            width: 100%;
            margin-bottom: 4px;
            padding: 4px 0px 4px 0px;
            display: flex;
            justify-content: space-between;
            border-radius: 4px;
            background-color: #fafbfc;
            font-size: 0.76em;
        }

        .right > .protocol > .read .digest .name, 
        .right > .protocol > .write .digest .name {
            width: 60%;
            position: relative;
            cursor: pointer;
        }

        .right > .protocol > .read .digest .type, 
        .right > .protocol > .write .digest .type, 
        .right > .protocol > .read .digest .comment,
        .right > .protocol > .write .digest .comment {
            width: 20%;
            position: relative;
            cursor: pointer;
        }

        .right > .protocol > .read .sub .field .digest  .name .pad > .top, 
        .right > .protocol > .write .sub .field .digest .name .pad > .top {
            border-left: 2px solid #323232;
            border-bottom: 1px solid #323232;
        }

        .right > .protocol > .read .sub .field .digest  .name .pad > .bottom, 
        .right > .protocol > .write .sub .field .digest .name .pad > .bottom {
            border-left: 2px solid #323232;
            border-top: 1px solid #323232;
        }

        .right > .protocol > .read .sub .field:first-child .digest .name .pad > .top, 
        .right > .protocol > .write .sub .field:first-child .digest .name .pad > .top {
            border-left: 2px solid #323232;
            border-bottom: 1px solid #323232;
        }

        .right > .protocol > .read .sub .field:first-child .digest .name .pad > .bottom, 
        .right > .protocol > .write .sub .field:first-child .digest .name .pad > .bottom {
            border-left: 2px solid #323232;
            border-top: 1px solid #323232;
        }

        .right > .protocol > .read .sub .field:last-child .digest .name .pad > .top, 
        .right > .protocol > .write .sub .field:last-child .digest .name .pad > .top {
            border-left: 2px solid #323232;
            border-bottom: 1px solid #323232;
        }

        .right > .protocol > .read .sub .field:last-child .digest .name .pad > .bottom, 
        .right > .protocol > .write .sub .field:last-child .digest .name .pad > .bottom {
            border-left: 2px solid #323232;
            border-top: 1px solid #323232;
        }

        .right > .protocol > .write .digest .inner .tips, 
        .right > .protocol > .read .digest .inner .tips {
            position: fixed;
            padding: 4px 8px 4px 8px;
            border-radius: 4px;
            visibility: hidden;
            background-color: #323232;
            color: #fff;
        }

        .right > .protocol > .read .sub, 
        .right > .protocol > .write .sub {
            width: 100%;
            display: flex;
        }
        
        .right > .protocol > .read .explain, 
        .right > .protocol > .write .explain {
            width: 100%;
            display: flex;
            flex-direction: column;
        }

        .right > .protocol > .read .digest .type .list, 
        .right > .protocol > .write .digest .type .list {
            color: #ed0d0c;
        }

        .right > .protocol > .read .digest .type .map, 
        .right > .protocol > .write .digest .type .map {
            color: #ed0d0c;
        }
        
        .right > .protocol > .read .digest .type .bool, 
        .right > .protocol > .write .digest .type .bool {
            color: #9c57b6;
        }

        .right > .protocol > .read .digest .type .binary, 
        .right > .protocol > .write .digest .type .binary {
            color: #1a9f29;
        }

        .right > .protocol > .read .digest .type .rst, 
        .right > .protocol > .write .digest .type .rst, 
        .right > .protocol > .read .digest .type .bst, 
        .right > .protocol > .write .digest .type .bst, 
        .right > .protocol > .read .digest .type .str,
        .right > .protocol > .write .digest .type .str {
            color: #1a9f29;
        }

        .right > .protocol > .read .digest .type .i8, 
        .right > .protocol > .write .digest .type .i8, 
        .right > .protocol > .read .digest .type .i16, 
        .right > .protocol > .write .digest .type .i16, 
        .right > .protocol > .read .digest .type .i32, 
        .right > .protocol > .write .digest .type .i32, 
        .right > .protocol > .read .digest .type .i64,
        .right > .protocol > .write .digest .type .i64 {
            color: #275fe4;
        } 

        .right > .protocol > .read .digest .type .u8,
        .right > .protocol > .write .digest .type .u8,
        .right > .protocol > .read .digest .type .u16,
        .right > .protocol > .write .digest .type .u16,
        .right > .protocol > .read .digest .type .u32,
        .right > .protocol > .write .digest .type .u32,
        .right > .protocol > .read .digest .type .u64,
        .right > .protocol > .write .digest .type .u64 {
            color: #275fe4;
        }

        .right > .protocol > .read .digest .type .f32, 
        .right > .protocol > .write .digest .type .f32, 
        .right > .protocol > .read .digest .type .f64, 
        .right > .protocol > .write .digest .type .f64 {
            color: #a71b76;
        }
    </style>
    <script>
        function load(event) {
            const value = location.search.replace(/^\\?/, '').split('&').map(i => i.split('=')).filter(([k, v]) => k == 'style').flat()[1];
            if(!value) return;
            const json = JSON.parse(decodeURIComponent(value));
            let styles = '';
            for(const selector in json) {
                const attributes = json[selector];
                let style = '';
                for(attribute in attributes) {
                    style += `${attribute}: ${ attributes[attribute] };`;
                }
                styles += `${selector} { ${style} }`;
            }
            const node = new DOMParser().parseFromString(`<style>${styles}</style>`, 'text/html');
            document.body.appendChild(node.head.firstChild)
        }

        function copy(event, target) {
            const inputValue = document.createElement('input');
            document.body. appendChild(inputValue);
            inputValue.value = target.innerText.split(':')[1];
            inputValue.select();
            document.execCommand('copy');
            document.body.removeChild(inputValue);
            // tips
            const { width, height } = target.parentElement.lastChild.getBoundingClientRect();
            target.parentElement.lastChild.style.top = `${event.y - height - 8}px`;
            target.parentElement.lastChild.style.left = `${event.x - (width / 2)}px`;
            target.parentElement.lastChild.style.visibility = 'visible';
            setTimeout(() => { target.parentElement.lastChild.style.visibility = 'hidden'; }, 1000);
        }
    </script>
</head>
<body onload='load(event)'>
    <div id='nav' class='left'>
",
        string:join([
            lists:concat([
            "    " "    ", "<div class='protocol' onclick=\"javascript:location.href='#protocol-", Protocol, "'\"><div class='inner'>", Protocol, " - ", Comment, "</div></div>"
            ]) || {Protocol, Comment, _, _} <- List, Protocol =/= 0
        ], "\n")
        ,"
    </div>
    <div id='pannel' class='right'>
        <div class='header'>", ProtocolComment, " - ", HtmlName, "</div>", "\n",  
        [
            lists:concat([
            "    ", "    ", "<div class='title' id='protocol-", Protocol, "'>", Protocol, " - ", Comment, "</div>", "\n",
            "    ", "    ", "<div class='protocol'>", "\n",
            "    ", "    ", "    ", "<div class='title'>发送</div>", "\n",
            "    ", "    ", "    ", "<div class='read'>", "\n",
            lists:flatten(ReadCode#code.html), 
            "    ", "    ", "    ", "</div>", "\n",
            "    ", "    ", "    ", "<div class='title'>接收</div>", "\n",
            "    ", "    ", "    ", "<div class='write'>", "\n",
            lists:flatten(WriteCode#code.html), "\n", 
            "    ", "    ", "    ", "</div>", "\n",
            "    ", "    ", "</div>", "\n"
            ]) || {Protocol, Comment, ReadCode, WriteCode} <- List, Protocol =/= 0
        ], "
    </div>
</body>
</html>"]),
    %% lua metadata, name test_protocol -> testProtocol
    %% LuaRead = string:join(lists:reverse(listing:collect(#code.lua, ReadList, [])), ",\n"),
    %% LuaWrite = string:join(lists:reverse(listing:collect(#code.lua, WriteList, [])), ",\n"),
    %% reverse read and write code
    %% Lua = lists:concat(["{\n    [\"write\"] = {\n", LuaWrite, "\n    },\n    [\"read\"] = {\n", LuaRead, "\n    }\n}"]),
    LuaRead = string:join(listing:collect(#code.lua_code, ReadList, []), ",\n"),
    LuaWrite = string:join(listing:collect(#code.lua_code, WriteList, []), ",\n"),
    LuaCode = lists:concat([
        "function encode", LuaName, "(offset, protocol, data)", "\n",
        "    ", "local switch = {", "\n",
        LuaRead, "\n",
        "    ", "}", "\n",
        "    ", "local method = switch[protocol]", "\n",
        "    ", "if method then", "\n",
        "    ", "    ", "return method()", "\n",
        "    ", "else", "\n",
        "    ", "    ", "error(string.format('unknown protocol define: %d', protocol))", "\n",
        "    ", "end", "\n",
        "end", "\n\n",
        "function decode", LuaName, "(offset, protocol, data)", "\n",
        "    ", "local switch = {", "\n",
        LuaWrite, "\n",
        "    ", "}", "\n",
        "    ", "local method = switch[protocol]", "\n",
        "    ", "if method then", "\n",
        "    ", "    ", "return method()", "\n",
        "    ", "else", "\n",
        "    ", "    ", "error(string.format('unknown protocol define: %d', protocol))", "\n",
        "    ", "end", "\n",
        "end"
    ]),
    LuaMetaInner = string:join([lists:concat([
        "    ", "[", Protocol, "] = {", "\n",
        "    ", "    ", "[\"comment\"] = \"", Comment, "\",", "\n",
        "    ", "    ", "[\"write\"] = ", ReadCode#code.lua_meta, ",", "\n",
        "    ", "    ", "[\"read\"] = ", WriteCode#code.lua_meta, "\n",
        "    ", "}"
    ]) || {Protocol, Comment, ReadCode, WriteCode} <- List, Protocol =/= 0], ",\n"),
    LuaMeta = lists:concat(["{\n", LuaMetaInner, "\n}"]),
    %% js metadata, name test_protocol -> testProtocol
    %% JsRead = string:join(lists:reverse(listing:collect(#code.js, ReadList, [])), ",\n"),
    %% JsWrite = string:join(lists:reverse(listing:collect(#code.js, WriteList, [])), ",\n"),
    %% reverse read and write code
    JsRead = string:join(listing:collect(#code.js_code, ReadList, []), "\n"),
    JsWrite = string:join(listing:collect(#code.js_code, WriteList, []), "\n"),
    JsCode = lists:concat([
        "export function encode", JsName, "(textEncoder, view, offset, protocol, data) {", "\n",
        "    ", "switch (protocol) {", "\n",
        JsRead, "\n",
        "    ", "    ", "default:throw(\"unknown protocol define: \" + protocol)", "\n",
        "    ", "}", "\n",
        "}", "\n\n",
        "export function decode", JsName, "(textDecoder, view, offset, protocol) {", "\n",
        "    ", "switch (protocol) {", "\n",
        JsWrite, "\n",
        "    ", "    ", "default:throw(\"unknown protocol define: \" + protocol)", "\n",
        "    ", "}", "\n",
        "}"
    ]),
    JsMetaInner = string:join([lists:concat([
        "    ", "\"", Protocol, "\" : {", "\n",
        "    ", "    ", "\"comment\" : \"", Comment, "\",", "\n",
        "    ", "    ", "\"write\" : ", ReadCode#code.js_meta, ",", "\n",
        "    ", "    ", "\"read\" : ", WriteCode#code.js_meta, "\n",
        "    ", "}"
    ]) || {Protocol, Comment, ReadCode, WriteCode} <- List, Protocol =/= 0], ",\n"),
    JsMeta = lists:concat(["{\n", JsMetaInner, "\n}"]),
    %% Js = lists:concat(["{\n    \"write\" : {\n", JsRead, "\n    },\n    \"read\" : {\n", JsWrite, "\n    }\n}"]),

    %% cs metadata, name test_protocol -> testProtocol
    %% CsRead = string:join(lists:reverse(listing:collect(#code.cs, ReadList, [])), ",\n"),
    %% CsWrite = string:join(lists:reverse(listing:collect(#code.cs, WriteList, [])), ",\n"),
    %% reverse read and write code
    CsRead = string:join(listing:collect(#code.cs_code, ReadList, []), "\n"),
    CsWrite = string:join(listing:collect(#code.cs_code, WriteList, []), "\n"),
    CsCode = lists:concat([
        "public static class ", CsName, "\n"
        "{", "\n",
        "    ", "public static void Encode(System.Text.Encoding encoding, System.IO.BinaryWriter writer, System.UInt16 protocol, System.Collections.Generic.Dictionary<System.String, System.Object> data) ", "\n",
        "    ", "{", "\n",
        "    ", "    ", "switch (protocol) ", "\n",
        "    ", "    ", "{", "\n",
        CsRead, "\n",
        "    ", "    ", "    ", "default:throw new System.ArgumentException(System.String.Format(\"unknown protocol define: {0}\", protocol));", "\n",
        "    ", "    ", "}", "\n",
        "    ", "}", "\n\n",
        "    ", "public static System.Collections.Generic.Dictionary<System.String, System.Object> Decode(System.Text.Encoding encoding, System.IO.BinaryReader reader, System.UInt16 protocol) ", "\n",
        "    ", "{", "\n",
        "    ", "    ", "switch (protocol) ", "\n",
        "    ", "    ", "{", "\n",
        CsWrite, "\n",
        "    ", "    ", "    ", "default:throw new System.ArgumentException(System.String.Format(\"unknown protocol define: {0}\", protocol));", "\n",
        "    ", "    ", "}", "\n",
        "    ", "}", "\n",
        "}"
    ]),
    CsMetaInner = string:join([lists:concat([
        "    ", "    ", "    ", "{\"", Protocol, "\", new Map() {\n",
        "    ", "    ", "    ", "    ", "{\"comment\", \"", Comment, "\"},", "\n",
        "    ", "    ", "    ", "    ", "{\"write\", new List() ", ReadCode#code.cs_meta, "},", "\n",
        "    ", "    ", "    ", "    ", "{\"read\", new List() ", WriteCode#code.cs_meta, "}", "\n",
        "    ", "    ", "    ", "}}"
    ]) || {Protocol, Comment, ReadCode, WriteCode} <- List, Protocol =/= 0], ",\n"),
    CsMeta = lists:concat([
        "using List = System.Collections.ArrayList;", "\n",
        "using Map = System.Collections.Generic.Dictionary<System.String, System.Object>;", "\n",
        "\n",
        "public static class ", CsName, "\n", "{", "\n",
        "    ", "public static Map GetMeta()", "\n",
        "    ", "{", "\n",
        "    ", "    ", "return new Map()", "\n",
        "    ", "    ", "{\n",
        CsMetaInner, "\n",
        "    ", "    ", "};", "\n",
        "    ", "}", "\n",
        "}"
    ]),
    %% Cs = lists:concat(["{\n    {\"write\", {\n", CsRead, "\n    }},\n    {\"read\", {\n", CsWrite, "\n    }\n}}"]),
    %% return code sets
    #code{handler = Handler, erl = Erl, html = Html, lua_code = LuaCode, lua_meta = LuaMeta, js_code = JsCode, js_meta = JsMeta, cs_code = CsCode, cs_meta = CsMeta};
collect_code([#io{protocol = Protocol, comment = Comment, handler = Handler, read = Read, write = Write} | T], ProtocolComment, HtmlName, LuaName, JsName, CsName, List) ->
    ReadCode = parse_read(Protocol, Read, Handler),
    WriteCode = parse_write(Protocol, Write),
    collect_code(T, ProtocolComment, HtmlName, LuaName, JsName, CsName, [{Protocol, Comment, ReadCode, WriteCode} | List]).

%%%===================================================================
%%% Parse HTML code Part
%%%===================================================================
%% html code
parse_code_html(_, Meta) ->
    %% start with 3 tabs(4 space) padding
    Padding = lists:duplicate(3, "    "),
    Result = parse_code_html_loop(Meta, 4, []),
    %% format a protocol define
    %% lists:concat(["        \"", Protocol, "\" : [\n", Result, "\n        ]"]).
    %% lists:concat(["[\n", Result, "\n", Padding, "]"]).
    lists:concat([
        %% Padding, Padding, "<div class='title' id='protocol-", Protocol, "'>●", Meta#meta.name, "(", Protocol, ")</div>", "\n",
        Padding, Padding, "\n", 
        Result, "\n", "\n"
    ]).

parse_code_html_loop([], _, List) ->
    %% construct as a list
    string:join(lists:reverse(List), "\n");
parse_code_html_loop([#meta{name = Name, type = binary, explain = Length, comment = Comment} | T], Depth, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    %% String = lists:flatten(io_lib:format("~s{\"name\" : \"~s\", \"type\" : \"~s\", \"comment\" : \"~ts\", \"explain\" : ~w}", [Padding, word:to_lower_hump(Name), binary, Comment, Length])),
    String = lists:flatten(io_lib:format(lists:concat([
    Padding, "<div class='field'>", "\n",
    Padding, "    ", "<div class='digest'>", "\n",
    Padding, "    ", "    ", "<div class='inner name'><div class='pad' style='width: ~tppx;position: relative;'><div class='top' style='width: calc(~tp% - 4px);position: absolute;height: 50%;top: 0; right: 4px;'></div><div class='bottom' style='width: calc(~tp% - 4px);height: 50%; position: absolute;bottom: 0; right: 4px;'></div></div><div class='align' style='width: calc(100% - ~tppx);'><span onclick='copy(event, this)'>名称: ~ts</span><div class='tips'>已复制</div></div></div>", "\n",
    Padding, "    ", "    ", "<div class='inner type'><span onclick='copy(event, this)'>类型: <span class='~ts'>~ts(~w)</span></span><div class='tips'>已复制</div></div>", "\n",
    Padding, "    ", "    ", "<div class='inner comment'><span onclick='copy(event, this)'>注释: ~ts</span><div class='tips'>已复制</div></div>", "\n",
    Padding, "    ", "</div>", "\n",
    Padding, "</div>"]), [(Depth - 3) * 16, 100 / (Depth - 3), 100 / (Depth - 3), (Depth - 3) * 16, word:to_lower_hump(Name), binary, binary, Length, Comment])),
    parse_code_html_loop(T, Depth, [String | List]);
parse_code_html_loop([#meta{name = Name, type = Type, explain = [], comment = Comment} | T], Depth, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    %% String = lists:flatten(io_lib:format("~s{\"name\" : \"~s\", \"type\" : \"~s\", \"comment\" : \"~ts\", \"explain\" : ~w}", [Padding, word:to_lower_hump(Name), Type, Comment, []])),
    %% String = lists:flatten(io_lib:format("~s<div class='field'><div class='digest'><div class='inner comment' onclick='copy(event, this)'>注释: ~ts</div><div class='inner type' onclick='copy(event, this)'>类型: ~ts</div><div class='inner name' onclick='copy(event, this)'>名称: ~ts</div></div></div>", [Padding, Comment, Type, word:to_lower_hump(Name)])),
    String = lists:flatten(io_lib:format(lists:concat([
        Padding, "<div class='field'>", "\n",
        Padding, "    ", "<div class='digest'>", "\n",
        Padding, "    ", "    ", "<div class='inner name'><div class='pad' style='width: ~tppx;position: relative;'><div class='top' style='width: calc(~tp% - 4px);height: 50%; position: absolute;top: 0; right: 4px;'></div><div class='bottom' style='width: calc(~tp% - 4px);height: 50%; position: absolute;bottom: 0; right: 4px;'></div></div><div class='align' style='width: calc(100% - ~tppx);'><span onclick='copy(event, this)'>名称: ~ts</span><div class='tips'>已复制</div></div></div>", "\n",
        Padding, "    ", "    ", "<div class='inner type'><span onclick='copy(event, this)'>类型: <span class='~ts'>~ts</span></span><div class='tips'>已复制</div></div>", "\n",
        Padding, "    ", "    ", "<div class='inner comment'><span onclick='copy(event, this)'>注释: ~ts</span><div class='tips'>已复制</div></div>", "\n",
        Padding, "    ", "</div>", "\n",
        Padding, "</div>"]), [(Depth - 3) * 16, 100 / (Depth - 3), 100 / (Depth - 3), (Depth - 3) * 16, word:to_lower_hump(Name), Type, Type, Comment])),
    parse_code_html_loop(T, Depth, [String | List]);
parse_code_html_loop([#meta{name = Name, type = list, explain = Explain = [_ | _], comment = Comment, key = undefined} | T], Depth, List) ->
    %% recurse
    Result = parse_code_html_loop(Explain, Depth + 2, []),
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format a field
    %% String = lists:flatten(io_lib:format("~s{\"name\" : \"~s\", \"type\" : \"~s\", \"comment\" : \"~ts\", \"explain\" : [\n~ts\n~s]}", [Padding, word:to_lower_hump(Name), Type, Comment, Result, Padding])),
    %% String = lists:flatten(io_lib:format("~s<div class='field'><div class='digest'><div class='inner comment' onclick='copy(event, this)'>注释: ~ts</div><div class='inner type' onclick='copy(event, this)'>类型: ~ts</div><div class='inner name' onclick='copy(event, this)'>名称: ~ts</div></div><div class='sub'><div class='align'></div><div class='explain'>~ts</div></div></div>", [Padding, Comment, Type, word:to_lower_hump(Name), Result])),
    String = lists:flatten(io_lib:format(lists:concat([
        Padding, "<div class='field'>", "\n",
        Padding, "    ", "<div class='digest'>", "\n",
        Padding, "    ", "    ", "<div class='inner name'><div class='pad' style='width: ~tppx;position: relative;'><div class='top' style='width: calc(~tp% - 4px);position: absolute;height: 50%;top: 0; right: 4px;'></div><div class='bottom' style='width: calc(~tp% - 4px);height: 50%; position: absolute;bottom: 0; right: 4px;'></div></div><div class='align' style='width: calc(100% - ~tppx);'><span onclick='copy(event, this)'>名称: ~ts</span><div class='tips'>已复制</div></div></div>", "\n",
        Padding, "    ", "    ", "<div class='inner type'><span onclick='copy(event, this)'>类型: <span class='~ts'>~ts</span></span><div class='tips'>已复制</div></div>", "\n",
        Padding, "    ", "    ", "<div class='inner comment'><span onclick='copy(event, this)'>注释: ~ts</span><div class='tips'>已复制</div></div>", "\n",
        Padding, "    ", "</div>", "\n",
        Padding, "    ", "<div class='sub'>", "\n",
        %% Padding, "    ", "    ", "<div class='align'></div>", "\n",
        Padding, "    ", "    ", "<div class='explain'>", "\n",
        "~ts", "\n",
        Padding, "    ", "    ", "</div>", "\n",
        Padding, "    ", "</div>", "\n",
        Padding, "</div>"]), [(Depth - 3) * 16, 100 / (Depth - 3), 100 / (Depth - 3), (Depth - 3) * 16, word:to_lower_hump(Name), list, list, Comment, Result])),
    parse_code_html_loop(T, Depth, [String | List]);
parse_code_html_loop([#meta{name = Name, type = list, explain = Explain = [_ | _], comment = Comment, key = Key} | T], Depth, List) ->
    %% recurse
    Result = parse_code_html_loop(Explain, Depth + 2, []),
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format a field
    %% String = lists:flatten(io_lib:format("~s{\"name\" : \"~s\", \"type\" : \"~s\", \"comment\" : \"~ts\", \"explain\" : [\n~ts\n~s]}", [Padding, word:to_lower_hump(Name), Type, Comment, Result, Padding])),
    %% String = lists:flatten(io_lib:format("~s<div class='field'><div class='digest'><div class='inner comment' onclick='copy(event, this)'>注释: ~ts</div><div class='inner type' onclick='copy(event, this)'>类型: ~ts</div><div class='inner name' onclick='copy(event, this)'>名称: ~ts</div></div><div class='sub'><div class='align'></div><div class='explain'>~ts</div></div></div>", [Padding, Comment, Type, word:to_lower_hump(Name), Result])),
    String = lists:flatten(io_lib:format(lists:concat([
        Padding, "<div class='field'>", "\n",
        Padding, "    ", "<div class='digest'>", "\n",
        Padding, "    ", "    ", "<div class='inner name'><div class='pad' style='width: ~tppx;position: relative;'><div class='top' style='width: calc(~tp% - 4px);height: 50%;position: absolute;top: 0; right: 4px;'></div><div class='bottom' style='width: calc(~tp% - 4px);height: 50%; position: absolute;bottom: 0; right: 4px;'></div></div><div class='align' style='width: calc(100% - ~tppx);'><span onclick='copy(event, this)'>名称: ~ts</span><div class='tips'>已复制</div></div></div>", "\n",
        Padding, "    ", "    ", "<div class='inner type'><span onclick='copy(event, this)'>类型: <span class='~ts'>~ts</span></span> / <span onclick='copy(event, this)'>键: ~ts</span><div class='tips'>已复制</div></div>", "\n",
        Padding, "    ", "    ", "<div class='inner comment'><span onclick='copy(event, this)'>注释: ~ts</span><div class='tips'>已复制</div></div>", "\n",
        Padding, "    ", "</div>", "\n",
        Padding, "    ", "<div class='sub'>", "\n",
        %% Padding, "    ", "    ", "<div class='align'></div>", "\n",
        Padding, "    ", "    ", "<div class='explain'>", "\n",
        "~ts", "\n",
        Padding, "    ", "    ", "</div>", "\n",
        Padding, "    ", "</div>", "\n",
        Padding, "</div>"]), [(Depth - 3) * 16, 100 / (Depth - 3), 100 / (Depth - 3), (Depth - 3) * 16, word:to_lower_hump(Name), map, map, word:to_lower_hump(Key), Comment, Result])),
    parse_code_html_loop(T, Depth, [String | List]).

%%%===================================================================
%%% Parse Lua Code Part
%%%===================================================================
%% lua meta
parse_meta_lua(_, Meta) ->
    %% start with 3 tabs(4 space) padding
    Padding = lists:duplicate(2, "    "),
    Result = parse_meta_lua_loop(Meta, 3, []),
    %% format a protocol define
    %% lists:concat(["        [", Protocol, "] = {\n", Result, "\n        }"]).
    lists:concat(["{\n", Result, "\n", Padding, "}"]).

parse_meta_lua_loop([], _, List) ->
    %% construct as a list
    string:join(lists:reverse(List), ",\n");
parse_meta_lua_loop([#meta{name = Name, type = binary, explain = Length, comment = Comment} | T], Depth, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format a field
    String = lists:flatten(io_lib:format("~s{name = \"~s\", type = \"~s\", comment = \"~ts\", explain = ~w}", [Padding, word:to_lower_hump(Name), binary, Comment, Length])),
    parse_meta_lua_loop(T, Depth, [String | List]);
parse_meta_lua_loop([#meta{name = Name, type = Type, explain = [], comment = Comment} | T], Depth, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format a field
    String = lists:flatten(io_lib:format("~s{name = \"~s\", type = \"~s\", comment = \"~ts\", explain = ~w}", [Padding, word:to_lower_hump(Name), Type, Comment, {}])),
    parse_meta_lua_loop(T, Depth, [String | List]);
parse_meta_lua_loop([#meta{name = Name, type = list, comment = Comment, explain = Explain = [_ | _], key = undefined} | T], Depth, List) ->
    %% recurse
    Result = parse_meta_lua_loop(Explain, Depth + 1, []),
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    String = lists:flatten(io_lib:format("~s{name = \"~s\", type = \"~s\", comment = \"~ts\", explain = {\n~ts\n~s}}", [Padding, word:to_lower_hump(Name), list, Comment, Result, Padding])),
    parse_meta_lua_loop(T, Depth, [String | List]);
parse_meta_lua_loop([#meta{name = Name, type = list, comment = Comment, explain = Explain = [_ | _], key = Key} | T], Depth, List) ->
    %% recurse
    Result = parse_meta_lua_loop(Explain, Depth + 1, []),
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    String = lists:flatten(io_lib:format("~s{name = \"~s\", type = \"~s\", comment = \"~ts\", key = \"~ts\", explain = {\n~ts\n~s}}", [Padding, word:to_lower_hump(Name), map, Comment, word:to_lower_hump(Key), Result, Padding])),
    parse_meta_lua_loop(T, Depth, [String | List]).

%% lua code
parse_encode_lua(Protocol, Meta) ->
    %% start with 3 tabs(4 space) padding
    Padding = lists:duplicate(2, "    "),
    {_, Result} = parse_encode_lua_loop(Meta, 3, "data", [], []),
    %% format a protocol define
    String = [
        Padding, "[", Protocol, "] = function()", "\n",
        Padding, "    ", "local offset = offset", "\n",
        Padding, "    ", "local table = {}", "\n",
        Result, "\n",
        Padding, "    ", "return table", "\n",
        Padding, "end"
    ],
    lists:concat(String).

parse_encode_lua_loop([], _, _, Fields, List) ->
    %% construct as a list
    {string:join(lists:reverse(Fields), ", "), string:join(lists:reverse(List), "\n")};
parse_encode_lua_loop([#meta{name = Name, type = binary, comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = ", ScopeArgs, "[\"", HumpName, "\"]", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_encode_lua_loop(T, Depth, ScopeArgs, [HumpName | Fields], [String | List]);
parse_encode_lua_loop([#meta{name = Name, type = bool, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = string.pack(\">I1\", ", ScopeArgs, "[\"", HumpName, "\"] ~= 0 and 1 or 0)", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_encode_lua_loop(T, Depth, ScopeArgs, [HumpName | Fields], [String | List]);
parse_encode_lua_loop([#meta{name = Name, type = u8, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = string.pack(\">I1\", ", ScopeArgs, "[\"", HumpName, "\"])", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_encode_lua_loop(T, Depth, ScopeArgs, [HumpName | Fields], [String | List]);
parse_encode_lua_loop([#meta{name = Name, type = u16, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = string.pack(\">I2\", ", ScopeArgs, "[\"", HumpName, "\"])", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_encode_lua_loop(T, Depth, ScopeArgs, [HumpName | Fields], [String | List]);
parse_encode_lua_loop([#meta{name = Name, type = u32, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = string.pack(\">I4\", ", ScopeArgs, "[\"", HumpName, "\"])", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_encode_lua_loop(T, Depth, ScopeArgs, [HumpName | Fields], [String | List]);
parse_encode_lua_loop([#meta{name = Name, type = u64, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = string.pack(\">I8\", ", ScopeArgs, "[\"", HumpName, "\"])", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_encode_lua_loop(T, Depth, ScopeArgs, [HumpName | Fields], [String | List]);
parse_encode_lua_loop([#meta{name = Name, type = i8, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = string.pack(\">i1\", ", ScopeArgs, "[\"", HumpName, "\"])", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_encode_lua_loop(T, Depth, ScopeArgs, [HumpName | Fields], [String | List]);
parse_encode_lua_loop([#meta{name = Name, type = i16, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = string.pack(\">i2\", ", ScopeArgs, "[\"", HumpName, "\"])", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_encode_lua_loop(T, Depth, ScopeArgs, [HumpName | Fields], [String | List]);
parse_encode_lua_loop([#meta{name = Name, type = i32, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = string.pack(\">i4\", ", ScopeArgs, "[\"", HumpName, "\"])", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_encode_lua_loop(T, Depth, ScopeArgs, [HumpName | Fields], [String | List]);
parse_encode_lua_loop([#meta{name = Name, type = i64, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = string.pack(\">i8\", ", ScopeArgs, "[\"", HumpName, "\"])", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_encode_lua_loop(T, Depth, ScopeArgs, [HumpName | Fields], [String | List]);
parse_encode_lua_loop([#meta{name = Name, type = f32, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = string.pack(\">f\", ", ScopeArgs, "[\"", HumpName, "\"])", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_encode_lua_loop(T, Depth, ScopeArgs, [HumpName | Fields], [String | List]);
parse_encode_lua_loop([#meta{name = Name, type = f64, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = string.pack(\">d\", ", ScopeArgs, "[\"", HumpName, "\"])", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_encode_lua_loop(T, Depth, ScopeArgs, [HumpName | Fields], [String | List]);
parse_encode_lua_loop([#meta{name = Name, type = str, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = string.pack(\">s2\", ", ScopeArgs, "[\"", HumpName, "\"])", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_encode_lua_loop(T, Depth, ScopeArgs, [HumpName | Fields], [String | List]);
parse_encode_lua_loop([#meta{name = Name, type = bst, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = string.pack(\">s2\", ", ScopeArgs, "[\"", HumpName, "\"])", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_encode_lua_loop(T, Depth, ScopeArgs, [HumpName | Fields], [String | List]);
parse_encode_lua_loop([#meta{name = Name, type = rst, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "-- ", Comment, "\n",
        Padding, "table[offset] = string.pack(\">s2\", ", ScopeArgs, "[\"", HumpName, "\"])", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_encode_lua_loop(T, Depth, ScopeArgs, [HumpName | Fields], [String | List]);
parse_encode_lua_loop([#meta{name = Name, type = list, explain = Explain = [_ | _], comment = Comment, key = undefined} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    %% recurse
    {_, Result} = parse_encode_lua_loop(Explain, Depth + 1, lists:concat([HumpName, "Table[", HumpName, "Index]"]), [], []),
    String = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", HumpName, "Table = ", ScopeArgs, "[\"", HumpName, "\"]", "\n",
        Padding, "table[offset] = string.pack(\">I2\", #", HumpName, "Table)", "\n",
        Padding, "offset = offset + 1", "\n",
        Padding, "for ", HumpName, "Index = 1, #", HumpName, "Table do", "\n",
        Result, "\n",
        Padding, "end"
    ],
    parse_encode_lua_loop(T, Depth, ScopeArgs, [HumpName | Fields], [String | List]);
parse_encode_lua_loop([#meta{name = Name, type = list, explain = Explain = [_ | _], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    %% recurse
    {_, Result} = parse_encode_lua_loop(Explain, Depth + 1, lists:concat([HumpName, "ItemData"]), [], []),
    String = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", HumpName, "Table = ", ScopeArgs, "[\"", HumpName, "\"]", "\n",
        Padding, "table[offset] = string.pack(\">I2\", #", HumpName, "Table)", "\n",
        Padding, "offset = offset + 1", "\n",
        Padding, "for _, ", HumpName, "ItemData in pairs(", HumpName, "Table) do", "\n",
        Result, "\n",
        Padding, "end"
    ],
    parse_encode_lua_loop(T, Depth, ScopeArgs, [HumpName | Fields], [String | List]).

%% lua code
parse_decode_lua(Protocol, Meta) ->
    %% start with 3 tabs(4 space) padding
    Padding = lists:duplicate(2, "    "),
    {Fields, Result} = parse_decode_lua_loop(Meta, 3, "data", [], []),
    %% format a protocol define
    String = [
        Padding, "[", Protocol, "] = function()", "\n",
        Padding, "    ", "local offset = offset", "\n",
        Result, "\n",
        Padding, "    ", "return {", Fields, "}", "\n",
        Padding, "end"
    ],
    lists:concat(String).

parse_decode_lua_loop([], _, _, Fields, List) ->
    %% construct as a list
    {string:join([[Name, " = ", Name] || Name <- lists:reverse(Fields)], ", "), string:join(lists:reverse(List), "\n")};
parse_decode_lua_loop([#meta{name = Name, type = binary, explain = Explain, comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", HumpName, " = string.unpack(\"c", integer_to_list(Explain), "\", data, offset)", "\n",
        Padding, "offset = offset + ", integer_to_list(Explain)
    ],
    parse_decode_lua_loop(T, Depth, ScopeArgs, [HumpName | Fields], [String | List]);
parse_decode_lua_loop([#meta{name = Name, type = bool, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", HumpName, " = string.unpack(\">I1\", data, offset) ~= 0", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_decode_lua_loop(T, Depth, ScopeArgs, [HumpName | Fields], [String | List]);
parse_decode_lua_loop([#meta{name = Name, type = u8, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", HumpName, " = string.unpack(\">I1\", data, offset)", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_decode_lua_loop(T, Depth, ScopeArgs, [HumpName | Fields], [String | List]);
parse_decode_lua_loop([#meta{name = Name, type = u16, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", HumpName, " = string.unpack(\">I2\", data, offset)", "\n",
        Padding, "offset = offset + 2"
    ],
    parse_decode_lua_loop(T, Depth, ScopeArgs, [HumpName | Fields], [String | List]);
parse_decode_lua_loop([#meta{name = Name, type = u32, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", HumpName, " = string.unpack(\">I4\", data, offset)", "\n",
        Padding, "offset = offset + 4"
    ],
    parse_decode_lua_loop(T, Depth, ScopeArgs, [HumpName | Fields], [String | List]);
parse_decode_lua_loop([#meta{name = Name, type = u64, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", HumpName, " = string.unpack(\">I8\", data, offset)", "\n",
        Padding, "offset = offset + 8"
    ],
    parse_decode_lua_loop(T, Depth, ScopeArgs, [HumpName | Fields], [String | List]);
parse_decode_lua_loop([#meta{name = Name, type = i8, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", HumpName, " = string.unpack(\">i1\", data, offset)", "\n",
        Padding, "offset = offset + 1"
    ],
    parse_decode_lua_loop(T, Depth, ScopeArgs, [HumpName | Fields], [String | List]);
parse_decode_lua_loop([#meta{name = Name, type = i16, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", HumpName, " = string.unpack(\">i2\", data, offset)", "\n",
        Padding, "offset = offset + 2"
    ],
    parse_decode_lua_loop(T, Depth, ScopeArgs, [HumpName | Fields], [String | List]);
parse_decode_lua_loop([#meta{name = Name, type = i32, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", HumpName, " = string.unpack(\">i4\", data, offset)", "\n",
        Padding, "offset = offset + 4"
    ],
    parse_decode_lua_loop(T, Depth, ScopeArgs, [HumpName | Fields], [String | List]);
parse_decode_lua_loop([#meta{name = Name, type = i64, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", HumpName, " = string.unpack(\">i8\", data, offset)", "\n",
        Padding, "offset = offset + 8"
    ],
    parse_decode_lua_loop(T, Depth, ScopeArgs, [HumpName | Fields], [String | List]);
parse_decode_lua_loop([#meta{name = Name, type = f32, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", HumpName, " = string.unpack(\">f\", data, offset)", "\n",
        Padding, "offset = offset + 4"
    ],
    parse_decode_lua_loop(T, Depth, ScopeArgs, [HumpName | Fields], [String | List]);
parse_decode_lua_loop([#meta{name = Name, type = f64, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", HumpName, " = string.unpack(\">d\", data, offset)", "\n",
        Padding, "offset = offset + 8"
    ],
    parse_decode_lua_loop(T, Depth, ScopeArgs, [HumpName | Fields], [String | List]);
parse_decode_lua_loop([#meta{name = Name, type = str, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", HumpName, " = string.unpack(\">s2\", data, offset)", "\n",
        Padding, "offset = offset + 2 + string.len(", HumpName, ")"
    ],
    parse_decode_lua_loop(T, Depth, ScopeArgs, [HumpName | Fields], [String | List]);
parse_decode_lua_loop([#meta{name = Name, type = bst, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", HumpName, " = string.unpack(\">s2\", data, offset)", "\n",
        Padding, "offset = offset + 2 + string.len(", HumpName, ")"
    ],
    parse_decode_lua_loop(T, Depth, ScopeArgs, [HumpName | Fields], [String | List]);
parse_decode_lua_loop([#meta{name = Name, type = rst, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", HumpName, " = string.unpack(\">s2\", data, offset)", "\n",
        Padding, "offset = offset + 2 + string.len(", HumpName, ")"
    ],
    parse_decode_lua_loop(T, Depth, ScopeArgs, [HumpName | Fields], [String | List]);
parse_decode_lua_loop([#meta{name = Name, type = list, explain = Explain = [_ | _], comment = Comment, key = undefined} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    %% recurse
    {SubFields, Result} = parse_decode_lua_loop(Explain, Depth + 1, HumpName, [], []),
    String = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", HumpName, " = {}", "\n",
        Padding, "local ", HumpName, "Length = string.unpack(\">I2\", data, offset)", "\n",
        Padding, "offset = offset + 2", "\n",
        Padding, "for ", HumpName, "Index = 1, ", HumpName, "Length do", "\n",
        Result, "\n",
        Padding, "    ", HumpName, "[", HumpName, "Index] = {", SubFields, "}", "\n",
        Padding, "end"
    ],
    parse_decode_lua_loop(T, Depth, ScopeArgs, [HumpName | Fields], [String | List]);
parse_decode_lua_loop([#meta{name = Name, type = list, explain = Explain = [_ | _], comment = Comment, key = Key} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    %% recurse
    {SubFields, Result} = parse_decode_lua_loop(Explain, Depth + 1, HumpName, [], []),
    String = [
        Padding, "-- ", Comment, "\n",
        Padding, "local ", HumpName, " = {}", "\n",
        Padding, "local ", HumpName, "Length = string.unpack(\">I2\", data, offset)", "\n",
        Padding, "offset = offset + 2", "\n",
        Padding, "for ", HumpName, "Index = 1, ", HumpName, "Length do", "\n",
        Result, "\n",
        Padding, "    ", HumpName, "[", word:to_lower_hump(Key), "] = {", SubFields, "}", "\n",
        Padding, "end"
    ],
    parse_decode_lua_loop(T, Depth, ScopeArgs, [HumpName | Fields], [String | List]).

%%%===================================================================
%%% Parse Js Code Part
%%%===================================================================
%% js meta
parse_meta_js(_, Meta) ->
    %% start with 3 tabs(4 space) padding
    Padding = lists:duplicate(2, "    "),
    Result = parse_meta_js_loop(Meta, 3, []),
    %% format a protocol define
    %% lists:concat(["        \"", Protocol, "\" : [\n", Result, "\n        ]"]).
    lists:concat(["[\n", Result, "\n", Padding, "]"]).

parse_meta_js_loop([], _, List) ->
    %% construct as a list
    string:join(lists:reverse(List), ",\n");
parse_meta_js_loop([#meta{name = Name, type = binary, explain = Length, comment = Comment} | T], Depth, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    String = lists:flatten(io_lib:format("~s{\"name\" : \"~s\", \"type\" : \"~s\", \"comment\" : \"~ts\", \"explain\" : ~w}", [Padding, word:to_lower_hump(Name), binary, Comment, Length])),
    parse_meta_js_loop(T, Depth, [String | List]);
parse_meta_js_loop([#meta{name = Name, type = Type, explain = [], comment = Comment} | T], Depth, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    String = lists:flatten(io_lib:format("~s{\"name\" : \"~s\", \"type\" : \"~s\", \"comment\" : \"~ts\", \"explain\" : ~w}", [Padding, word:to_lower_hump(Name), Type, Comment, []])),
    parse_meta_js_loop(T, Depth, [String | List]);
parse_meta_js_loop([#meta{name = Name, type = list, comment = Comment, explain = Explain = [_ | _], key = undefined} | T], Depth, List) ->
    %% recurse
    Result = parse_meta_js_loop(Explain, Depth + 1, []),
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format a field
    String = lists:flatten(io_lib:format("~s{\"name\" : \"~s\", \"type\" : \"~s\", \"comment\" : \"~ts\", \"explain\" : [\n~ts\n~s]}", [Padding, word:to_lower_hump(Name), list, Comment, Result, Padding])),
    parse_meta_js_loop(T, Depth, [String | List]);
parse_meta_js_loop([#meta{name = Name, type = list, comment = Comment, explain = Explain = [_ | _], key = Key} | T], Depth, List) ->
    %% recurse
    Result = parse_meta_js_loop(Explain, Depth + 1, []),
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format a field
    String = lists:flatten(io_lib:format("~s{\"name\" : \"~s\", \"type\" : \"~s\", \"comment\" : \"~ts\", \"key\": \"~ts\", \"explain\" : [\n~ts\n~s]}", [Padding, word:to_lower_hump(Name), map, Comment, word:to_lower_hump(Key), Result, Padding])),
    parse_meta_js_loop(T, Depth, [String | List]).

%% js code
parse_encode_js(Protocol, Meta) ->
    %% start with 3 tabs(4 space) padding
    Padding = lists:duplicate(2, "    "),
    {_, Result} = parse_encode_js_loop(Meta, 3, "data", [], []),
    %% format a protocol define
    String = [
        Padding, "case ", Protocol, ": {", "\n",
        Result, "\n",
        Padding, "    ", "return new DataView(view.buffer.slice(0, offset));", "\n",
        Padding, "}"
    ],
    lists:concat(String).

parse_encode_js_loop([], _, _, Fields, List) ->
    %% construct as a list
    {string:join(lists:reverse(Fields), ", "), string:join(lists:reverse(List), "\n")};
parse_encode_js_loop([#meta{name = Name, type = binary, explain = Length, comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    %% format one field
    String = [
        Padding, "// extend", "\n",
        Padding, "while (view.byteLength < offset + ", integer_to_list(Length), ") {", "\n",
        Padding, "    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));", "\n",
        Padding, "    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));", "\n",
        Padding, "    view = extendView;", "\n",
        Padding, "}", "\n",
        Padding, "// ", Comment, "\n",
        Padding, "const ", HumpName, " = ", ScopeArgs, "[\"", HumpName, "\"];", "\n",
        Padding, "(new Uint8Array(view.buffer, offset)).set(new Uint8Array(", HumpName, "));", "\n",
        Padding, "offset = offset + ", HumpName, ".byteLength;"
    ],
    parse_encode_js_loop(T, Depth, ScopeArgs, [HumpName | Fields], [String | List]);
parse_encode_js_loop([#meta{name = Name, type = bool, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "// extend", "\n",
        Padding, "while (view.byteLength < offset + 1) {", "\n",
        Padding, "    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));", "\n",
        Padding, "    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));", "\n",
        Padding, "    view = extendView;", "\n",
        Padding, "}", "\n",
        Padding, "// ", Comment, "\n",
        Padding, "view.setUint8(offset, ", ScopeArgs, "[\"", HumpName, "\"] ? 1 : 0, false);", "\n",
        Padding, "offset = offset + 1;"
    ],
    parse_encode_js_loop(T, Depth, ScopeArgs, [HumpName | Fields], [String | List]);
parse_encode_js_loop([#meta{name = Name, type = u8, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "// extend", "\n",
        Padding, "while (view.byteLength < offset + 1) {", "\n",
        Padding, "    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));", "\n",
        Padding, "    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));", "\n",
        Padding, "    view = extendView;", "\n",
        Padding, "}", "\n",
        Padding, "// ", Comment, "\n",
        Padding, "view.setUint8(offset, ", ScopeArgs, "[\"", HumpName, "\"], false);", "\n",
        Padding, "offset = offset + 1;"
    ],
    parse_encode_js_loop(T, Depth, ScopeArgs, [HumpName | Fields], [String | List]);
parse_encode_js_loop([#meta{name = Name, type = u16, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "// extend", "\n",
        Padding, "while (view.byteLength < offset + 2) {", "\n",
        Padding, "    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));", "\n",
        Padding, "    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));", "\n",
        Padding, "    view = extendView;", "\n",
        Padding, "}", "\n",
        Padding, "// ", Comment, "\n",
        Padding, "view.setUint16(offset, ", ScopeArgs, "[\"", HumpName, "\"], false);", "\n",
        Padding, "offset = offset + 2;"
    ],
    parse_encode_js_loop(T, Depth, ScopeArgs, [HumpName | Fields], [String | List]);
parse_encode_js_loop([#meta{name = Name, type = u32, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "// extend", "\n",
        Padding, "while (view.byteLength < offset + 4) {", "\n",
        Padding, "    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));", "\n",
        Padding, "    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));", "\n",
        Padding, "    view = extendView;", "\n",
        Padding, "}", "\n",
        Padding, "// ", Comment, "\n",
        Padding, "view.setUint32(offset, ", ScopeArgs, "[\"", HumpName, "\"], false);", "\n",
        Padding, "offset = offset + 4;"
    ],
    parse_encode_js_loop(T, Depth, ScopeArgs, [HumpName | Fields], [String | List]);
parse_encode_js_loop([#meta{name = Name, type = u64, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "// extend", "\n",
        Padding, "while (view.byteLength < offset + 8) {", "\n",
        Padding, "    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));", "\n",
        Padding, "    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));", "\n",
        Padding, "    view = extendView;", "\n",
        Padding, "}", "\n",
        Padding, "// ", Comment, "\n",
        Padding, "view.setBigUint64(offset, ", ScopeArgs, "[\"", HumpName, "\"], false);", "\n",
        Padding, "offset = offset + 8;"
    ],
    parse_encode_js_loop(T, Depth, ScopeArgs, [HumpName | Fields], [String | List]);
parse_encode_js_loop([#meta{name = Name, type = i8, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "// extend", "\n",
        Padding, "while (view.byteLength < offset + 1) {", "\n",
        Padding, "    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));", "\n",
        Padding, "    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));", "\n",
        Padding, "    view = extendView;", "\n",
        Padding, "}", "\n",
        Padding, "// ", Comment, "\n",
        Padding, "view.setInt8(offset, ", ScopeArgs, "[\"", HumpName, "\"], false);", "\n",
        Padding, "offset = offset + 1;"
    ],
    parse_encode_js_loop(T, Depth, ScopeArgs, [HumpName | Fields], [String | List]);
parse_encode_js_loop([#meta{name = Name, type = i16, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "// extend", "\n",
        Padding, "while (view.byteLength < offset + 2) {", "\n",
        Padding, "    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));", "\n",
        Padding, "    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));", "\n",
        Padding, "    view = extendView;", "\n",
        Padding, "}", "\n",
        Padding, "// ", Comment, "\n",
        Padding, "view.setInt16(offset, ", ScopeArgs, "[\"", HumpName, "\"], false);", "\n",
        Padding, "offset = offset + 2;"
    ],
    parse_encode_js_loop(T, Depth, ScopeArgs, [HumpName | Fields], [String | List]);
parse_encode_js_loop([#meta{name = Name, type = i32, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "// extend", "\n",
        Padding, "while (view.byteLength < offset + 4) {", "\n",
        Padding, "    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));", "\n",
        Padding, "    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));", "\n",
        Padding, "    view = extendView;", "\n",
        Padding, "}", "\n",
        Padding, "// ", Comment, "\n",
        Padding, "view.setInt32(offset, ", ScopeArgs, "[\"", HumpName, "\"], false);", "\n",
        Padding, "offset = offset + 4;"
    ],
    parse_encode_js_loop(T, Depth, ScopeArgs, [HumpName | Fields], [String | List]);
parse_encode_js_loop([#meta{name = Name, type = i64, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "// extend", "\n",
        Padding, "while (view.byteLength < offset + 8) {", "\n",
        Padding, "    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));", "\n",
        Padding, "    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));", "\n",
        Padding, "    view = extendView;", "\n",
        Padding, "}", "\n",
        Padding, "// ", Comment, "\n",
        Padding, "view.setBigInt64(offset, ", ScopeArgs, "[\"", HumpName, "\"], false);", "\n",
        Padding, "offset = offset + 8;"
    ],
    parse_encode_js_loop(T, Depth, ScopeArgs, [HumpName | Fields], [String | List]);
parse_encode_js_loop([#meta{name = Name, type = f32, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "// extend", "\n",
        Padding, "while (view.byteLength < offset + 4) {", "\n",
        Padding, "    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));", "\n",
        Padding, "    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));", "\n",
        Padding, "    view = extendView;", "\n",
        Padding, "}", "\n",
        Padding, "// ", Comment, "\n",
        Padding, "view.setFloat32(offset, ", ScopeArgs, "[\"", HumpName, "\"], false);", "\n",
        Padding, "offset = offset + 4;"
    ],
    parse_encode_js_loop(T, Depth, ScopeArgs, [HumpName | Fields], [String | List]);
parse_encode_js_loop([#meta{name = Name, type = f64, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "// extend", "\n",
        Padding, "while (view.byteLength < offset + 8) {", "\n",
        Padding, "    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));", "\n",
        Padding, "    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));", "\n",
        Padding, "    view = extendView;", "\n",
        Padding, "}", "\n",
        Padding, "// ", Comment, "\n",
        Padding, "view.setFloat64(offset, ", ScopeArgs, "[\"", HumpName, "\"], false);", "\n",
        Padding, "offset = offset + 8;"
    ],
    parse_encode_js_loop(T, Depth, ScopeArgs, [HumpName | Fields], [String | List]);
parse_encode_js_loop([#meta{name = Name, type = str, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "// extend", "\n",
        Padding, "while (view.byteLength < offset + 2) {", "\n",
        Padding, "    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));", "\n",
        Padding, "    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));", "\n",
        Padding, "    view = extendView;", "\n",
        Padding, "}", "\n",
        Padding, "// ", Comment, "\n",
        Padding, "const ", HumpName, "Array = textEncoder.encode(", ScopeArgs, "[\"", HumpName, "\"]);", "\n",
        Padding, "view.setUint16(offset, ", HumpName, "Array.length, false);", "\n",
        Padding, "offset = offset + 2;", "\n",
        Padding, "// extend", "\n",
        Padding, "while (view.byteLength < offset + ", HumpName, "Array.length) {", "\n",
        Padding, "    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));", "\n",
        Padding, "    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));", "\n",
        Padding, "    view = extendView;", "\n",
        Padding, "}", "\n",
        Padding, "(new Uint8Array(view.buffer, offset)).set(", HumpName, "Array);", "\n",
        Padding, "offset = offset + ", HumpName, "Array.length;"
    ],
    parse_encode_js_loop(T, Depth, ScopeArgs, [HumpName | Fields], [String | List]);
parse_encode_js_loop([#meta{name = Name, type = bst, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "// extend", "\n",
        Padding, "while (view.byteLength < offset + 2) {", "\n",
        Padding, "    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));", "\n",
        Padding, "    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));", "\n",
        Padding, "    view = extendView;", "\n",
        Padding, "}", "\n",
        Padding, "// ", Comment, "\n",
        Padding, "const ", HumpName, "Array = textEncoder.encode(", ScopeArgs, "[\"", HumpName, "\"]);", "\n",
        Padding, "view.setUint16(offset, ", HumpName, "Array.length, false);", "\n",
        Padding, "offset = offset + 2;", "\n",
        Padding, "// extend", "\n",
        Padding, "while (view.byteLength < offset + ", HumpName, "Array.length) {", "\n",
        Padding, "    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));", "\n",
        Padding, "    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));", "\n",
        Padding, "    view = extendView;", "\n",
        Padding, "}", "\n",
        Padding, "(new Uint8Array(view.buffer, offset)).set(", HumpName, "Array);", "\n",
        Padding, "offset = offset + ", HumpName, "Array.length;"
    ],
    parse_encode_js_loop(T, Depth, ScopeArgs, [HumpName | Fields], [String | List]);
parse_encode_js_loop([#meta{name = Name, type = rst, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "// extend", "\n",
        Padding, "while (view.byteLength < offset + 2) {", "\n",
        Padding, "    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));", "\n",
        Padding, "    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));", "\n",
        Padding, "    view = extendView;", "\n",
        Padding, "}", "\n",
        Padding, "// ", Comment, "\n",
        Padding, "const ", HumpName, "Array = textEncoder.encode(", ScopeArgs, "[\"", HumpName, "\"]);", "\n",
        Padding, "view.setUint16(offset, ", HumpName, "Array.length, false);", "\n",
        Padding, "offset = offset + 2;", "\n",
        Padding, "// extend", "\n",
        Padding, "while (view.byteLength < offset + ", HumpName, "Array.length) {", "\n",
        Padding, "    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));", "\n",
        Padding, "    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));", "\n",
        Padding, "    view = extendView;", "\n",
        Padding, "}", "\n",
        Padding, "(new Uint8Array(view.buffer, offset)).set(", HumpName, "Array);", "\n",
        Padding, "offset = offset + ", HumpName, "Array.length;"
    ],
    parse_encode_js_loop(T, Depth, ScopeArgs, [HumpName | Fields], [String | List]);
parse_encode_js_loop([#meta{name = Name, type = list, explain = Explain = [_ | _], comment = Comment, key = undefined} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    %% recurse
    {_, Result} = parse_encode_js_loop(Explain, Depth + 1, lists:concat([HumpName, "DataItem"]), [], []),
    String = [
        Padding, "// extend", "\n",
        Padding, "while (view.byteLength < offset + 2) {", "\n",
        Padding, "    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));", "\n",
        Padding, "    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));", "\n",
        Padding, "    view = extendView;", "\n",
        Padding, "}", "\n",
        Padding, "// ", Comment, "\n",
        Padding, "const ", HumpName, "Data = ", ScopeArgs, "[\"", HumpName, "\"];", "\n",
        Padding, "view.setUint16(offset, ", HumpName, "Data.length, false);", "\n",
        Padding, "offset = offset + 2;", "\n",
        Padding, "for (const ", HumpName, "DataItem of ", HumpName, "Data) {", "\n",
        Result, "\n",
        Padding, "}"
    ],
    parse_encode_js_loop(T, Depth, ScopeArgs, [HumpName, lists:concat([HumpName, "Length"]) | Fields], [String | List]);
parse_encode_js_loop([#meta{name = Name, type = list, explain = Explain = [_ | _], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    %% recurse
    {_, Result} = parse_encode_js_loop(Explain, Depth + 1, lists:concat([HumpName, "Data", "[", HumpName, "DataKey", "]"]), [], []),
    String = [
        Padding, "// extend", "\n",
        Padding, "while (view.byteLength < offset + 2) {", "\n",
        Padding, "    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));", "\n",
        Padding, "    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));", "\n",
        Padding, "    view = extendView;", "\n",
        Padding, "}", "\n",
        Padding, "// ", Comment, "\n",
        Padding, "const ", HumpName, "Data = ", ScopeArgs, "[\"", HumpName, "\"];", "\n",
        Padding, "view.setUint16(offset, Object.keys(", HumpName, "Data).length, false);", "\n",
        Padding, "offset = offset + 2;", "\n",
        Padding, "for (const ", HumpName, "DataKey in ", HumpName, "Data) {", "\n",
        Result, "\n",
        Padding, "}"
    ],
    parse_encode_js_loop(T, Depth, ScopeArgs, [HumpName, lists:concat([HumpName, "Length"]) | Fields], [String | List]).

%% js code
parse_decode_js(Protocol, Meta) ->
    %% start with 3 tabs(4 space) padding
    Padding = lists:duplicate(2, "    "),
    {Fields, Result} = parse_decode_js_loop(Meta, 3, [], []),
    %% format a protocol define
    String = [
        Padding, "case ", Protocol, ": {", "\n",
        Result, "\n",
        Padding, "    ", "return {", Fields, "};", "\n",
        Padding, "}"
    ],
    lists:concat(String).

parse_decode_js_loop([], _, Fields, List) ->
    %% construct as a list
    {string:join(lists:reverse(Fields), ", "), string:join(lists:reverse(List), "\n")};
parse_decode_js_loop([#meta{name = Name, type = binary, explain = Length, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    %% format one field
    String = [
        Padding, "// ", Comment, "\n",
        Padding, "const ", HumpName, " = view.buffer.slice(offset, offset + ", integer_to_list(Length), ");", "\n",
        Padding, "offset = offset + ", integer_to_list(Length), ";"
    ],
    parse_decode_js_loop(T, Depth, [HumpName | Fields], [String | List]);
parse_decode_js_loop([#meta{name = Name, type = bool, explain = [], comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "// ", Comment, "\n",
        Padding, "const ", HumpName, " = view.getUint8(offset, false) !== 0;", "\n",
        Padding, "offset = offset + 1;"
    ],
    parse_decode_js_loop(T, Depth, [HumpName | Fields], [String | List]);
parse_decode_js_loop([#meta{name = Name, type = u8, explain = [], comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "// ", Comment, "\n",
        Padding, "const ", HumpName, " = view.getUint8(offset, false);", "\n",
        Padding, "offset = offset + 1;"
    ],
    parse_decode_js_loop(T, Depth, [HumpName | Fields], [String | List]);
parse_decode_js_loop([#meta{name = Name, type = u16, explain = [], comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "// ", Comment, "\n",
        Padding, "const ", HumpName, " = view.getUint16(offset, false);", "\n",
        Padding, "offset = offset + 2;"
    ],
    parse_decode_js_loop(T, Depth, [HumpName | Fields], [String | List]);
parse_decode_js_loop([#meta{name = Name, type = u32, explain = [], comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "// ", Comment, "\n",
        Padding, "const ", HumpName, " = view.getUint32(offset, false);", "\n",
        Padding, "offset = offset + 4;"
    ],
    parse_decode_js_loop(T, Depth, [HumpName | Fields], [String | List]);
parse_decode_js_loop([#meta{name = Name, type = u64, explain = [], comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "// ", Comment, "\n",
        Padding, "const ", HumpName, " = view.getBigUint64(offset, false);", "\n",
        Padding, "offset = offset + 8;"
    ],
    parse_decode_js_loop(T, Depth, [HumpName | Fields], [String | List]);
parse_decode_js_loop([#meta{name = Name, type = i8, explain = [], comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "// ", Comment, "\n",
        Padding, "const ", HumpName, " = view.getInt8(offset, false);", "\n",
        Padding, "offset = offset + 1;"
    ],
    parse_decode_js_loop(T, Depth, [HumpName | Fields], [String | List]);
parse_decode_js_loop([#meta{name = Name, type = i16, explain = [], comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "// ", Comment, "\n",
        Padding, "const ", HumpName, " = view.getInt16(offset, false);", "\n",
        Padding, "offset = offset + 2;"
    ],
    parse_decode_js_loop(T, Depth, [HumpName | Fields], [String | List]);
parse_decode_js_loop([#meta{name = Name, type = i32, explain = [], comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "// ", Comment, "\n",
        Padding, "const ", HumpName, " = view.getInt32(offset, false);", "\n",
        Padding, "offset = offset + 4;"
    ],
    parse_decode_js_loop(T, Depth, [HumpName | Fields], [String | List]);
parse_decode_js_loop([#meta{name = Name, type = i64, explain = [], comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "// ", Comment, "\n",
        Padding, "const ", HumpName, " = view.getBigInt64(offset, false);", "\n",
        Padding, "offset = offset + 8;"
    ],
    parse_decode_js_loop(T, Depth, [HumpName | Fields], [String | List]);
parse_decode_js_loop([#meta{name = Name, type = f32, explain = [], comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "// ", Comment, "\n",
        Padding, "const ", HumpName, " = view.getFloat32(offset, false);", "\n",
        Padding, "offset = offset + 4;"
    ],
    parse_decode_js_loop(T, Depth, [HumpName | Fields], [String | List]);
parse_decode_js_loop([#meta{name = Name, type = f64, explain = [], comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "// ", Comment, "\n",
        Padding, "const ", HumpName, " = view.getFloat64(offset, false);", "\n",
        Padding, "offset = offset + 8;"
    ],
    parse_decode_js_loop(T, Depth, [HumpName | Fields], [String | List]);
parse_decode_js_loop([#meta{name = Name, type = str, explain = [], comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "// ", Comment, "\n",
        Padding, "const ", HumpName, "Length = view.getUint16(offset, false);", "\n",
        Padding, "offset = offset + 2;", "\n",
        Padding, "const ", HumpName, "Array = new Uint8Array(view.buffer.slice(offset, offset + ", HumpName, "Length));", "\n",
        Padding, "const ", HumpName, " = textDecoder.decode(", HumpName, "Array);", "\n",
        Padding, "offset = offset + ", HumpName, "Length;"
    ],
    parse_decode_js_loop(T, Depth, [HumpName | Fields], [String | List]);
parse_decode_js_loop([#meta{name = Name, type = bst, explain = [], comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "// ", Comment, "\n",
        Padding, "const ", HumpName, "Length = view.getUint16(offset, false);", "\n",
        Padding, "offset = offset + 2;", "\n",
        Padding, "const ", HumpName, "Array = new Uint8Array(view.buffer.slice(offset, offset + ", HumpName, "Length));", "\n",
        Padding, "const ", HumpName, " = textDecoder.decode(", HumpName, "Array);", "\n",
        Padding, "offset = offset + ", HumpName, "Length;"
    ],
    parse_decode_js_loop(T, Depth, [HumpName | Fields], [String | List]);
parse_decode_js_loop([#meta{name = Name, type = rst, explain = [], comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "// ", Comment, "\n",
        Padding, "const ", HumpName, "Length = view.getUint16(offset, false);", "\n",
        Padding, "offset = offset + 2;", "\n",
        Padding, "const ", HumpName, "Array = new Uint8Array(view.buffer.slice(offset, offset + ", HumpName, "Length));", "\n",
        Padding, "const ", HumpName, " = textDecoder.decode(", HumpName, "Array);", "\n",
        Padding, "offset = offset + ", HumpName, "Length;"
    ],
    parse_decode_js_loop(T, Depth, [HumpName | Fields], [String | List]);
parse_decode_js_loop([#meta{name = Name, type = list, explain = Explain = [_ | _], comment = Comment, key = undefined} | T], Depth, Fields, List)  ->
    %% recurse
    {SubFields, Result} = parse_decode_js_loop(Explain, Depth + 1, [], []),
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "// ", Comment, "\n",
        Padding, "const ", HumpName, " = [];", "\n",
        Padding, "let ", HumpName, "Length = view.getUint16(offset, false);", "\n",
        Padding, "offset = offset + 2;", "\n",
        Padding, "while (--", HumpName, "Length >= 0) {", "\n",
        Result, "\n",
        Padding, "    // add", "\n",
        Padding, "    ", HumpName, ".push({", SubFields, "});", "\n",
        Padding, "}"
    ],
    parse_decode_js_loop(T, Depth, [HumpName | Fields], [String | List]);
parse_decode_js_loop([#meta{name = Name, type = list, explain = Explain = [_ | _], comment = Comment, key = Key} | T], Depth, Fields, List) ->
    %% recurse
    {SubFields, Result} = parse_decode_js_loop(Explain, Depth + 1, [], []),
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "// ", Comment, "\n",
        Padding, "const ", HumpName, " = {};", "\n",
        Padding, "let ", HumpName, "Length = view.getUint16(offset, false);", "\n",
        Padding, "offset = offset + 2;", "\n",
        Padding, "while (--", HumpName, "Length >= 0) {", "\n",
        Result, "\n",
        Padding, "    // add", "\n",
        Padding, "    ", HumpName, "[", word:to_lower_hump(Key), "] = {", SubFields, "};", "\n",
        Padding, "}"
    ],
    parse_decode_js_loop(T, Depth, [HumpName | Fields], [String | List]).


%%%===================================================================
%%% Parse Cs Code Part
%%%===================================================================
%% cs meta
parse_meta_cs(_, Meta) ->
    %% start with 3 tabs(4 space) padding
    Padding = lists:duplicate(4, "    "),
    Result = parse_meta_cs_loop(Meta, 5, []),
    %% format a protocol define
    %% lists:concat(["        \"", Protocol, "\" : [\n", Result, "\n        ]"]).
    lists:concat(["{\n", Result, "\n", Padding, "}"]).

parse_meta_cs_loop([], _, List) ->
    %% construct as a list
    string:join(lists:reverse(List), ",\n");
parse_meta_cs_loop([#meta{name = Name, type = binary, explain = Length, comment = Comment} | T], Depth, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    String = lists:flatten(io_lib:format("~snew Map() { {\"name\", \"~s\"}, {\"type\", \"~s\"}, {\"comment\", \"~ts\"}, {\"explain\", ~w} }", [Padding, word:to_lower_hump(Name), binary, Comment, Length])),
    parse_meta_cs_loop(T, Depth, [String | List]);
parse_meta_cs_loop([#meta{name = Name, type = Type, explain = [], comment = Comment} | T], Depth, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    String = lists:flatten(io_lib:format("~snew Map() { {\"name\", \"~s\"}, {\"type\", \"~s\"}, {\"comment\", \"~ts\"}, {\"explain\", new List()} }", [Padding, word:to_lower_hump(Name), Type, Comment])),
    parse_meta_cs_loop(T, Depth, [String | List]);
parse_meta_cs_loop([#meta{name = Name, type = list, comment = Comment, explain = Explain = [_ | _], key = undefined} | T], Depth, List) ->
    %% recurse
    Result = parse_meta_cs_loop(Explain, Depth + 1, []),
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format a field
    String = lists:flatten(io_lib:format("~snew Map() { {\"name\", \"~s\"}, {\"type\", \"~s\"}, {\"comment\", \"~ts\"}, {\"explain\", new List() {\n~ts\n~s}}}", [Padding, word:to_lower_hump(Name), list, Comment, Result, Padding])),
    parse_meta_cs_loop(T, Depth, [String | List]);
parse_meta_cs_loop([#meta{name = Name, type = list, comment = Comment, explain = Explain = [_ | _], key = Key} | T], Depth, List) ->
    %% recurse
    Result = parse_meta_cs_loop(Explain, Depth + 1, []),
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format a field
    String = lists:flatten(io_lib:format("~snew Map() { {\"name\", \"~s\"}, {\"type\", \"~s\"}, {\"comment\", \"~ts\"}, {\"key\", \"~ts\"}, {\"explain\", new List() {\n~ts\n~s}}}", [Padding, word:to_lower_hump(Name), map, Comment, word:to_lower_hump(Key), Result, Padding])),
    parse_meta_cs_loop(T, Depth, [String | List]).

%% cs code
parse_encode_cs(Protocol, Meta) ->
    %% start with 3 tabs(4 space) padding
    Padding = lists:duplicate(3, "    "),
    {_, Result} = parse_encode_cs_loop(Meta, 4, "data", [], []),
    %% format a protocol define
    String = [
        Padding, "case ", Protocol, ":", "\n",
        Padding, "{", "\n",
        Result, "\n",
        Padding, "    ", "return;", "\n",
        Padding, "}"
    ],
    lists:concat(String).

parse_encode_cs_loop([], _, _, Fields, List) ->
    %% construct as a list
    {string:join(lists:reverse(Fields), ", "), string:join(lists:reverse(List), "\n")};
parse_encode_cs_loop([#meta{name = Name, type = binary, explain = _, comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    %% format one field
    String = [
        Padding, "// ", Comment, "\n",
        Padding, "writer.Write((System.Byte[])", ScopeArgs, "[\"", HumpName, "\"]);"
    ],
    parse_encode_cs_loop(T, Depth, ScopeArgs, [HumpName | Fields], [String | List]);
parse_encode_cs_loop([#meta{name = Name, type = bool, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "// ", Comment, "\n",
        Padding, "writer.Write((System.Byte)((System.Boolean)", ScopeArgs, "[\"", HumpName, "\"] ? 1 : 0));"
    ],
    parse_encode_cs_loop(T, Depth, ScopeArgs, [HumpName | Fields], [String | List]);
parse_encode_cs_loop([#meta{name = Name, type = u8, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "// ", Comment, "\n",
        Padding, "writer.Write((System.Byte)", ScopeArgs, "[\"", HumpName, "\"]);"
    ],
    parse_encode_cs_loop(T, Depth, ScopeArgs, [HumpName | Fields], [String | List]);
parse_encode_cs_loop([#meta{name = Name, type = u16, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "// ", Comment, "\n",
        Padding, "writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)(System.UInt16)", ScopeArgs, "[\"", HumpName, "\"]));"
    ],
    parse_encode_cs_loop(T, Depth, ScopeArgs, [HumpName | Fields], [String | List]);
parse_encode_cs_loop([#meta{name = Name, type = u32, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "// ", Comment, "\n",
        Padding, "writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int32)(System.UInt32)", ScopeArgs, "[\"", HumpName, "\"]));"
    ],
    parse_encode_cs_loop(T, Depth, ScopeArgs, [HumpName | Fields], [String | List]);
parse_encode_cs_loop([#meta{name = Name, type = u64, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "// ", Comment, "\n",
        Padding, "writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)(System.UInt64)", ScopeArgs, "[\"", HumpName, "\"]));"
    ],
    parse_encode_cs_loop(T, Depth, ScopeArgs, [HumpName | Fields], [String | List]);
parse_encode_cs_loop([#meta{name = Name, type = i8, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "// ", Comment, "\n",
        Padding, "writer.Write((System.SByte)", ScopeArgs, "[\"", HumpName, "\"]);"
    ],
    parse_encode_cs_loop(T, Depth, ScopeArgs, [HumpName | Fields], [String | List]);
parse_encode_cs_loop([#meta{name = Name, type = i16, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "// ", Comment, "\n",
        Padding, "writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)", ScopeArgs, "[\"", HumpName, "\"]));"
    ],
    parse_encode_cs_loop(T, Depth, ScopeArgs, [HumpName | Fields], [String | List]);
parse_encode_cs_loop([#meta{name = Name, type = i32, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "// ", Comment, "\n",
        Padding, "writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int32)", ScopeArgs, "[\"", HumpName, "\"]));"
    ],
    parse_encode_cs_loop(T, Depth, ScopeArgs, [HumpName | Fields], [String | List]);
parse_encode_cs_loop([#meta{name = Name, type = i64, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "// ", Comment, "\n",
        Padding, "writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)", ScopeArgs, "[\"", HumpName, "\"]));"
    ],
    parse_encode_cs_loop(T, Depth, ScopeArgs, [HumpName | Fields], [String | List]);
parse_encode_cs_loop([#meta{name = Name, type = f32, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", HumpName, "Bytes = System.BitConverter.GetBytes((System.Single)", ScopeArgs, "[\"", HumpName, "\"]);", "\n",
        Padding, "if (System.BitConverter.IsLittleEndian) System.Array.Reverse(", HumpName, "Bytes);", "\n",
        Padding, "writer.Write(", HumpName, "Bytes);"
    ],
    parse_encode_cs_loop(T, Depth, ScopeArgs, [HumpName | Fields], [String | List]);
parse_encode_cs_loop([#meta{name = Name, type = f64, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", HumpName, "Bytes = System.BitConverter.GetBytes((System.Double)", ScopeArgs, "[\"", HumpName, "\"]);", "\n",
        Padding, "if (System.BitConverter.IsLittleEndian) System.Array.Reverse(", HumpName, "Bytes);", "\n",
        Padding, "writer.Write(", HumpName, "Bytes);"
    ],
    parse_encode_cs_loop(T, Depth, ScopeArgs, [HumpName | Fields], [String | List]);
parse_encode_cs_loop([#meta{name = Name, type = str, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", HumpName, "Bytes = encoding.GetBytes((System.String)", ScopeArgs, "[\"", HumpName, "\"]);", "\n",
        Padding, "writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)", HumpName, "Bytes.Length));", "\n",
        Padding, "writer.Write(", HumpName, "Bytes);"
    ],
    parse_encode_cs_loop(T, Depth, ScopeArgs, [HumpName | Fields], [String | List]);
parse_encode_cs_loop([#meta{name = Name, type = bst, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", HumpName, "Bytes = encoding.GetBytes((System.String)", ScopeArgs, "[\"", HumpName, "\"]);", "\n",
        Padding, "writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)", HumpName, "Bytes.Length));", "\n",
        Padding, "writer.Write(", HumpName, "Bytes);"
    ],
    parse_encode_cs_loop(T, Depth, ScopeArgs, [HumpName | Fields], [String | List]);
parse_encode_cs_loop([#meta{name = Name, type = rst, explain = [], comment = Comment} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", HumpName, "Bytes = encoding.GetBytes((System.String)", ScopeArgs, "[\"", HumpName, "\"]);", "\n",
        Padding, "writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)", HumpName, "Bytes.Length));", "\n",
        Padding, "writer.Write(", HumpName, "Bytes);"
    ],
    parse_encode_cs_loop(T, Depth, ScopeArgs, [HumpName | Fields], [String | List]);
parse_encode_cs_loop([#meta{name = Name, type = list, explain = Explain = [_ | _], comment = Comment, key = undefined} | T], Depth, ScopeArgs, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    %% recurse
    {_, Result} = parse_encode_cs_loop(Explain, Depth + 1, lists:concat([HumpName, "DataItem"]), [], []),
    String = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", HumpName, "Data = (System.Collections.ArrayList)", ScopeArgs, "[\"", HumpName, "\"];", "\n",
        Padding, "writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)", HumpName, "Data.Count));", "\n",
        Padding, "foreach(System.Collections.Generic.Dictionary<System.String, System.Object> ", HumpName, "DataItem in ", HumpName, "Data)", "\n",
        Padding, "{", "\n",
        Result, "\n",
        Padding, "}"
    ],
    parse_encode_cs_loop(T, Depth, ScopeArgs, [HumpName, lists:concat([HumpName, "Length"]) | Fields], [String | List]);
parse_encode_cs_loop([#meta{name = Name, type = list, explain = Explain = [_ | _], comment = Comment, key = Key} | T], Depth, ScopeArgs, Fields, List) ->
    Field = lists:keyfind(Key, #meta.name, Explain),
    Field == undefined andalso erlang:throw(lists:flatten(io_lib:format("Cound not found field ~ts in explain", [Key]))),
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    %% recurse
    {_, Result} = parse_encode_cs_loop(Explain, Depth + 1, lists:concat([HumpName, "DataItem.Value"]), [], []),
    String = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", HumpName, "Data = (System.Collections.Generic.Dictionary<System.Object, System.Collections.Generic.Dictionary<System.String, System.Object>>)", ScopeArgs, "[\"", HumpName, "\"];", "\n",
        Padding, "writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)", HumpName, "Data.Count));", "\n",
        Padding, "foreach(System.Collections.Generic.KeyValuePair<System.Object, System.Collections.Generic.Dictionary<System.String, System.Object>> ", HumpName, "DataItem in ", HumpName, "Data)", "\n",
        Padding, "{", "\n",
        Result, "\n",
        Padding, "}"
    ],
    parse_encode_cs_loop(T, Depth, ScopeArgs, [HumpName, lists:concat([HumpName, "Length"]) | Fields], [String | List]).

%% cs code
parse_decode_cs(Protocol, Meta) ->
    %% start with 3 tabs(4 space) padding
    Padding = lists:duplicate(3, "    "),
    {Fields, Result} = parse_decode_cs_loop(Meta, 4, [], []),
    %% format a protocol define
    String = [
        Padding, "case ", Protocol, ":", "\n",
        Padding, "{", "\n",
        Result, "\n",
        Padding, "    ", "return new System.Collections.Generic.Dictionary<System.String, System.Object>() {", Fields, "};", "\n",
        Padding, "}"
    ],
    lists:concat(String).

parse_decode_cs_loop([], _, Fields, List) ->
    %% construct as a list
    {string:join([["{\"", Name, "\", ", Name, "}"] || Name <- lists:reverse(Fields)], ", "), string:join(lists:reverse(List), "\n")};
parse_decode_cs_loop([#meta{name = Name, type = binary, explain = Length, comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    %% format one field
    String = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", HumpName, " = reader.ReadBytes(", integer_to_list(Length), ");"
    ],
    parse_decode_cs_loop(T, Depth, [HumpName | Fields], [String | List]);
parse_decode_cs_loop([#meta{name = Name, type = bool, explain = [], comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", HumpName, " = reader.ReadByte() != 0;"
    ],
    parse_decode_cs_loop(T, Depth, [HumpName | Fields], [String | List]);
parse_decode_cs_loop([#meta{name = Name, type = u8, explain = [], comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", HumpName, " = reader.ReadByte();"
    ],
    parse_decode_cs_loop(T, Depth, [HumpName | Fields], [String | List]);
parse_decode_cs_loop([#meta{name = Name, type = u16, explain = [], comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", HumpName, " = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());"
    ],
    parse_decode_cs_loop(T, Depth, [HumpName | Fields], [String | List]);
parse_decode_cs_loop([#meta{name = Name, type = u32, explain = [], comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", HumpName, " = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());"
    ],
    parse_decode_cs_loop(T, Depth, [HumpName | Fields], [String | List]);
parse_decode_cs_loop([#meta{name = Name, type = u64, explain = [], comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", HumpName, " = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());"
    ],
    parse_decode_cs_loop(T, Depth, [HumpName | Fields], [String | List]);
parse_decode_cs_loop([#meta{name = Name, type = i8, explain = [], comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", HumpName, " = reader.ReadSByte();"
    ],
    parse_decode_cs_loop(T, Depth, [HumpName | Fields], [String | List]);
parse_decode_cs_loop([#meta{name = Name, type = i16, explain = [], comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", HumpName, " = System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());"
    ],
    parse_decode_cs_loop(T, Depth, [HumpName | Fields], [String | List]);
parse_decode_cs_loop([#meta{name = Name, type = i32, explain = [], comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", HumpName, " = System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());"
    ],
    parse_decode_cs_loop(T, Depth, [HumpName | Fields], [String | List]);
parse_decode_cs_loop([#meta{name = Name, type = i64, explain = [], comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", HumpName, " = System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());"
    ],
    parse_decode_cs_loop(T, Depth, [HumpName | Fields], [String | List]);
parse_decode_cs_loop([#meta{name = Name, type = f32, explain = [], comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", HumpName, " = System.BitConverter.ToSingle(System.BitConverter.GetBytes(System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32())), 0);"
    ],
    parse_decode_cs_loop(T, Depth, [HumpName | Fields], [String | List]);
parse_decode_cs_loop([#meta{name = Name, type = f64, explain = [], comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", HumpName, " = System.BitConverter.ToDouble(System.BitConverter.GetBytes(System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64())), 0);"
    ],
    parse_decode_cs_loop(T, Depth, [HumpName | Fields], [String | List]);
parse_decode_cs_loop([#meta{name = Name, type = str, explain = [], comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", HumpName, "Length = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());", "\n",
        Padding, "var ", HumpName, " = encoding.GetString(reader.ReadBytes(", HumpName, "Length));"
    ],
    parse_decode_cs_loop(T, Depth, [HumpName | Fields], [String | List]);
parse_decode_cs_loop([#meta{name = Name, type = bst, explain = [], comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", HumpName, "Length = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());", "\n",
        Padding, "var ", HumpName, " = encoding.GetString(reader.ReadBytes(", HumpName, "Length));"
    ],
    parse_decode_cs_loop(T, Depth, [HumpName | Fields], [String | List]);
parse_decode_cs_loop([#meta{name = Name, type = rst, explain = [], comment = Comment} | T], Depth, Fields, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", HumpName, "Length = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());", "\n",
        Padding, "var ", HumpName, " = encoding.GetString(reader.ReadBytes(", HumpName, "Length));"
    ],
    parse_decode_cs_loop(T, Depth, [HumpName | Fields], [String | List]);
parse_decode_cs_loop([#meta{name = Name, type = list, explain = Explain = [_ | _], comment = Comment, key = undefined} | T], Depth, Fields, List) ->
    %% recurse
    {SubFields, Result} = parse_decode_cs_loop(Explain, Depth + 1, [], []),
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", HumpName, "Length = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());", "\n",
        Padding, "var ", HumpName, " = new System.Collections.ArrayList(", HumpName, "Length);", "\n",
        Padding, "while (", HumpName, "Length-- > 0)", "\n",
        Padding, "{", "\n",
        Result, "\n",
        Padding, "    // add", "\n",
        Padding, "    ", HumpName, ".Add(new System.Collections.Generic.Dictionary<System.String, System.Object>() {", SubFields, "});", "\n",
        Padding, "}"
    ],
    parse_decode_cs_loop(T, Depth, [HumpName | Fields], [String | List]);
parse_decode_cs_loop([#meta{name = Name, type = list, explain = Explain = [_ | _], comment = Comment, key = Key} | T], Depth, Fields, List) ->
    Field = lists:keyfind(Key, #meta.name, Explain),
    Field == undefined andalso erlang:throw(lists:flatten(io_lib:format("Cound not found field ~ts in explain", [Key]))),
    %% recurse
    {SubFields, Result} = parse_decode_cs_loop(Explain, Depth + 1, [], []),
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    HumpName = word:to_lower_hump(Name),
    String = [
        Padding, "// ", Comment, "\n",
        Padding, "var ", HumpName, "Length = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());", "\n",
        Padding, "var ", HumpName, " = new System.Collections.Generic.Dictionary<System.Object, System.Collections.Generic.Dictionary<System.String, System.Object>>(", HumpName, "Length);", "\n",
        Padding, "while (", HumpName, "Length-- > 0)", "\n",
        Padding, "{", "\n",
        Result, "\n",
        Padding, "    // add", "\n",
        Padding, "    ", HumpName, "[", word:to_lower_hump(Key), "] = new System.Collections.Generic.Dictionary<System.String, System.Object>() {", SubFields, "};", "\n",
        Padding, "}"
    ],
    parse_decode_cs_loop(T, Depth, [HumpName | Fields], [String | List]).

%%%===================================================================
%%% Parse Read Part
%%%===================================================================
parse_read(Protocol, SyntaxList, undefined) ->
    %% no handler
    parse_read(Protocol, SyntaxList, #handler{});
parse_read(0, _, #handler{module = undefined, function = undefined}) ->
    %% default handler code
    #code{erl = [], js_meta = [], lua_meta = [], handler = "handle(_, Protocol, Data) ->\n    {error, Protocol, Data}.\n"};
parse_read(0, _, #handler{module = Module, function = Function, arg = Arg, protocol = IsContainProtocol}) ->
    %% default handler code
    HandlerArgs = string:join([word:to_hump(A) || A <- [Arg, case IsContainProtocol of true -> "Protocol"; false -> "" end, "Data"], A =/= []], ", "),
    HandlerCode = lists:concat(["handle(", case word:to_hump(Arg) of [] -> "_"; HumpArg -> HumpArg end, ", ", case IsContainProtocol of true -> "Protocol"; false -> "_" end, ", ", "Data) ->\n    ", Module, ":", Function, "(", HandlerArgs, ").\n"]),
    #code{erl = [], js_meta = [], lua_meta = [], handler = HandlerCode};
parse_read(_, undefined, _) ->
    %% JsCode = lists:concat(["        \"", Protocol, "\" : ", "[]"]),
    JsCode = lists:concat(["[]"]),
    %% LuaCode = lists:concat(["        [", Protocol, "] = ", "{}"]),
    LuaCode = lists:concat(["{}"]),
    #code{erl = [], js_meta = JsCode, lua_meta = LuaCode, handler = []};
parse_read(Protocol, [], #handler{module = Module, function = Function, arg = Arg, protocol = IsContainProtocol}) ->
    %% erl code
    ErlCode = "read(" ++ integer_to_list(Protocol) ++ ", <<>>) ->\n    {ok, []};\n\n",
    %% JsCode = lists:concat(["        \"", Protocol, "\" : ", "[]"]),
    JsCode = lists:concat(["[]"]),
    %% LuaCode = lists:concat(["        [", Protocol, "] = ", "{}"]),
    LuaCode = lists:concat(["{}"]),
    %% handler code
    HandlerArgs = string:join([word:to_hump(A) || A <- [Arg, case IsContainProtocol of true -> integer_to_list(Protocol); false -> "" end], A =/= []], ", "),
    HandlerCode = lists:concat(["handle(", case word:to_hump(Arg) of [] -> "_"; HumpArg -> HumpArg end, ", ", Protocol, ", ", "[]) ->\n    ", Module, ":", Function, "(", HandlerArgs, ");\n\n"]),
    #code{erl = ErlCode, js_meta = JsCode, lua_meta = LuaCode, handler = HandlerCode};
parse_read(Protocol, SyntaxList = [_ | _], #handler{module = Module, function = Function, arg = Arg, protocol = IsContainProtocol}) ->
    List = [parse_read_unit(Syntax) || Syntax <- SyntaxList],
    %% collect code args
    ArgList = listing:collect_into(#field.args, List, fun(X) -> lists:flatten(X) end),
    %% ArgListCode = string:join(ArgList, ", "),
    %% split dynamic length binary extract expression
    {ArgPacks, RestPacks} = lists:splitwith(fun(#field{packs = Packs}) -> string:str(Packs, ":") =/= 0 end, List),
    %% packs Binary | <<Arg:Size, ...>> |  <<Arg:Size, ..., RestBinary/binary>>
    Packs = case ArgPacks of [] -> "Binary"; _ when RestPacks == [] -> lists:concat(["<<", string:join(listing:collect(#field.packs, ArgPacks), ", "), ">>"]); _ -> lists:concat(["<<", string:join(listing:collect(#field.packs, ArgPacks), ", "), ", Binary/binary>>"]) end,
    %% normal procedures
    Procedures = [Procedure || #field{procedures = Procedure} <- ArgPacks, Procedure =/= []],
    %% construct names
    Names = case RestPacks of [] -> []; _ -> ArgNames = [N ++ "Rest" || N <- lists:droplast(listing:collect(#field.args, RestPacks))], lists:zip(["Binary" | ArgNames], ArgNames ++ ["_"]) end,
    %% format procedure
    RestProcedures = lists:zipwith(fun(#field{procedures = [], packs = P}, {F, B}) -> io_lib:format("<<~s, ~s/binary>> = ~s", [P, B, F]); (#field{procedures = P}, {F, B}) -> io_lib:format(P, [B, F]) end, RestPacks, Names),
    ErlCode = lists:concat(["read(", Protocol, ", ", Packs, ") ->", case Procedures ++ RestProcedures of [] -> "\n"; Ps -> "\n" ++ ["    " ++ P ++ ",\n" || P <- Ps] end, "    {ok, ", join(ArgList), "};\n\n"]),
    %% collect unit meta
    MetaList = lists:flatten(listing:collect(#field.meta, List)),
    %% construct html code
    HtmlCode = parse_code_html(Protocol, MetaList),
    %% construct lua/js/cs code
    LuaCode = parse_encode_lua(Protocol, MetaList),
    LuaMeta = parse_meta_lua(Protocol, MetaList),
    JsCode = parse_encode_js(Protocol, MetaList),
    JsMeta = parse_meta_js(Protocol, MetaList),
    CsCode = parse_encode_cs(Protocol, MetaList),
    CsMeta = parse_meta_cs(Protocol, MetaList),
    %% construct erl handler code
    StateArgs = [word:to_hump(Arg) || Arg =/= []],
    ProtocolArgs = [integer_to_list(Protocol) || IsContainProtocol],
    %% module:function(User, protocol, ...)
    HandlerArgs = string:join(StateArgs ++ ProtocolArgs ++ ArgList, ", "),
    %% handle(User, protocol, [...]) ->
    HandlerCode = lists:concat(["handle(", case word:to_hump(Arg) of [] -> "_"; HumpArg -> HumpArg end, ", ", Protocol, ", ", join(ArgList), ") ->\n    ", Module, ":", Function, "(", HandlerArgs, ");\n\n"]),
    #code{erl = ErlCode, html = HtmlCode, lua_code = LuaCode, lua_meta = LuaMeta, js_code = JsCode, js_meta = JsMeta, cs_code = CsCode, cs_meta = CsMeta, handler = HandlerCode};
parse_read(_, _, _) ->
    #code{erl = [], html = [], js_meta = [], lua_meta = [], cs_meta = [], handler = []}.

%% parse unit
parse_read_unit(Unit = #binary{name = Name, default = Default, comment = Comment, explain = Explain}) ->
    HumpName = word:to_hump(case Default of [] -> Name; _ -> Default end),
    Args = io_lib:format("~s", [HumpName]),
    Packs = io_lib:format("~s:~w/binary", [HumpName, Explain]),
    #field{name = Name, meta = #meta{name = HumpName, type = element(1, Unit), comment = Comment, explain = Explain}, args = Args, packs = Packs};
parse_read_unit(Unit = #bool{name = Name, default = Default, comment = Comment}) ->
    HumpName = word:to_hump(case Default of [] -> Name; _ -> Default end),
    Args = io_lib:format("~sFlag", [HumpName]),
    Procedures = io_lib:format("~s = type:to_boolean(~s)", [Args, HumpName]),
    Packs = io_lib:format("~s:8", [HumpName]),
    #field{name = Name, procedures = Procedures, meta = #meta{name = HumpName, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_read_unit(Unit = #u8{name = Name, default = Default, comment = Comment}) ->
    HumpName = word:to_hump(case Default of [] -> Name; _ -> Default end),
    Args = io_lib:format("~s", [HumpName]),
    Packs = io_lib:format("~s:8", [HumpName]),
    #field{name = Name, meta = #meta{name = HumpName, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_read_unit(Unit = #u16{name = Name, default = Default, comment = Comment}) ->
    HumpName = word:to_hump(case Default of [] -> Name; _ -> Default end),
    Args = io_lib:format("~s", [HumpName]),
    Packs = io_lib:format("~s:16", [HumpName]),
    #field{name = Name, meta = #meta{name = HumpName, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_read_unit(Unit = #u32{name = Name, default = Default, comment = Comment}) ->
    HumpName = word:to_hump(case Default of [] -> Name; _ -> Default end),
    Args = io_lib:format("~s", [HumpName]),
    Packs = io_lib:format("~s:32", [HumpName]),
    #field{name = Name, meta = #meta{name = HumpName, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_read_unit(Unit = #u64{name = Name, default = Default, comment = Comment}) ->
    HumpName = word:to_hump(case Default of [] -> Name; _ -> Default end),
    Args = io_lib:format("~s", [HumpName]),
    Packs = io_lib:format("~s:64", [HumpName]),
    #field{name = Name, meta = #meta{name = HumpName, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_read_unit(Unit = #u128{name = Name, default = Default, comment = Comment}) ->
    HumpName = word:to_hump(case Default of [] -> Name; _ -> Default end),
    Args = io_lib:format("~s", [HumpName]),
    Packs = io_lib:format("~s:128", [HumpName]),
    #field{name = Name, meta = #meta{name = HumpName, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_read_unit(Unit = #i8{name = Name, default = Default, comment = Comment}) ->
    HumpName = word:to_hump(case Default of [] -> Name; _ -> Default end),
    Args = io_lib:format("~s", [HumpName]),
    Packs = io_lib:format("~s:8/signed", [HumpName]),
    #field{name = Name, meta = #meta{name = HumpName, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_read_unit(Unit = #i16{name = Name, default = Default, comment = Comment}) ->
    HumpName = word:to_hump(case Default of [] -> Name; _ -> Default end),
    Args = io_lib:format("~s", [HumpName]),
    Packs = io_lib:format("~s:16/signed", [HumpName]),
    #field{name = Name, meta = #meta{name = HumpName, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_read_unit(Unit = #i32{name = Name, default = Default, comment = Comment}) ->
    HumpName = word:to_hump(case Default of [] -> Name; _ -> Default end),
    Args = io_lib:format("~s", [HumpName]),
    Packs = io_lib:format("~s:32/signed", [HumpName]),
    #field{name = Name, meta = #meta{name = HumpName, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_read_unit(Unit = #i64{name = Name, default = Default, comment = Comment}) ->
    HumpName = word:to_hump(case Default of [] -> Name; _ -> Default end),
    Args = io_lib:format("~s", [HumpName]),
    Packs = io_lib:format("~s:64/signed", [HumpName]),
    #field{name = Name, meta = #meta{name = HumpName, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_read_unit(Unit = #i128{name = Name, default = Default, comment = Comment}) ->
    HumpName = word:to_hump(case Default of [] -> Name; _ -> Default end),
    Args = io_lib:format("~s", [HumpName]),
    Packs = io_lib:format("~s:128/signed", [HumpName]),
    #field{name = Name, meta = #meta{name = HumpName, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_read_unit(Unit = #f32{name = Name, default = Default, comment = Comment}) ->
    HumpName = word:to_hump(case Default of [] -> Name; _ -> Default end),
    Args = io_lib:format("~s", [HumpName]),
    Packs = io_lib:format("~s:32/float", [HumpName]),
    #field{name = Name, meta = #meta{name = HumpName, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_read_unit(Unit = #f64{name = Name, default = Default, comment = Comment}) ->
    HumpName = word:to_hump(case Default of [] -> Name; _ -> Default end),
    Args = io_lib:format("~s", [HumpName]),
    Packs = io_lib:format("~s:64/float", [HumpName]),
    #field{name = Name, meta = #meta{name = HumpName, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_read_unit(Unit = #bst{name = Name, default = Default, comment = Comment}) ->
    HumpName = word:to_hump(case Default of [] -> Name; _ -> Default end),
    Args = io_lib:format("~s", [HumpName]),
    Packs = io_lib:format("~s:16, ~s:~s/binary", [HumpName ++ "Length", HumpName, HumpName ++ "Length"]),
    #field{name = Name, meta = #meta{name = HumpName, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_read_unit(Unit = #str{name = Name, default = Default, comment = Comment}) ->
    HumpName = word:to_hump(case Default of [] -> Name; _ -> Default end),
    Args = io_lib:format("~sString", [HumpName]),
    Procedures = io_lib:format("~s = binary_to_list(~s)", [Args, HumpName]),
    Packs = io_lib:format("~s:16, ~s:~s/binary", [HumpName ++ "Length", HumpName, HumpName ++ "Length"]),
    #field{name = Name, procedures = Procedures, meta = #meta{name = HumpName, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};

%% structure unit
parse_read_unit(#tuple{name = Name, explain = Explain}) ->
    %% format per unit
    Field = parse_read_unit(Explain),
    Field#field{name = Name};

parse_read_unit(#list{name = Name, comment = Comment, explain = Explain, key = Key}) ->
    %% hump name is unpack bit variable bind
    HumpName = word:to_hump(Name),
    %% format subunit
    #field{args = Args, procedures = Procedures, packs = Packs, meta = Meta} = parse_read_unit(Explain),
    %% meta may be list or meta record
    ReviseMeta = lists:flatten([Meta]),
    %% format list pack info
    ListFunProcedures = case Procedures of [] -> []; _ -> Procedures ++ ", " end,
    ListProcedures = io_lib:format("{~s, ~~s} = protocol:read_list(fun(~sBinary) -> <<~s, ~sInnerRest/binary>> = ~sBinary, ~s{~s, ~sInnerRest} end, ~~s)", [HumpName, HumpName, Packs, HumpName, HumpName, ListFunProcedures, Args, HumpName]),
    %% Procedure = io_lib:format("~s = [~s || <<~s>> <= ~sBinary]", [HumpName, Args, Packs, HumpName]),
    %% read a list could not contain variable length binary like string/binary
    ListPacks = io_lib:format("~sBinary/binary", [HumpName]),
    %% ListPacks = io_lib:format("~sLength:16, ~sBinary:~sLength/binary-unit:~w", [HumpName, HumpName, HumpName, sum(ReviseMeta)]),
    #field{name = Name, args = HumpName, procedures = ListProcedures, packs = ListPacks, meta = #meta{name = Name, type = list, comment = Comment, explain = ReviseMeta, key = Key}};

parse_read_unit(Record) when is_tuple(Record) andalso is_atom(element(1, Record)) ->
    %% get beam abstract code
    Tag = element(1, Record),
    NameList = beam:find(Tag),
    %% error when beam abstract code empty
    NameList == [] andalso erlang:throw(need_to_update_beam_abstract_code),
    tuple_size(Record) =/= length(NameList) andalso erlang:throw(need_to_update_beam_abstract_code),
    %% zip field value and field name
    ZipList = lists:zip(tuple_to_list(Record), NameList),
    %% format per unit
    List = [parse_read_unit(case element(2, Field) of [] -> setelement(2, Field, Name); _ -> setelement(2, setelement(3, Field, element(2, Field)), Name) end) || {Field, Name} <- ZipList, is_unit(Field)],
    %% format function match param
    Args = lists:concat(["#", Tag, "{", string:join([io_lib:format("~s = ~s", [Names, Args]) || #field{name = Names, args = Args} <- List], ", "), "}"]),
    %% format function pack info
    Procedures = string:join(listing:collect(#field.procedures, List, []), ", "),
    Packs = string:join(listing:collect(#field.packs, List, []), ", "),
    #field{args = Args, procedures = Procedures, packs = Packs, name = listing:collect(#field.name, List), meta = lists:flatten(listing:collect(#field.meta, List, []))};
parse_read_unit(Tuple) when is_tuple(Tuple) andalso tuple_size(Tuple) > 0 ->
    %% format per unit
    List = [parse_read_unit(Field) || Field <- tuple_to_list(Tuple)],
    %% format function match args
    Args = lists:concat(["{", string:join([Args || #field{args = Args} <- List], ", "), "}"]),
    %% format function pack info
    Procedures = string:join(listing:collect(#field.procedures, List, []), ", "),
    Packs = string:join(listing:collect(#field.packs, List, []), ", "),
    #field{args = Args, procedures = Procedures, packs = Packs, name = listing:collect(#field.name, List, []), meta = listing:collect(#field.meta, List, [])};
parse_read_unit(Unit) ->
    erlang:throw(lists:flatten(io_lib:format("Unsupported read unit: ~tp", [Unit]))).

%%%===================================================================
%%% Parse Write Part
%%%===================================================================
parse_write(0, _) ->
    #code{erl = [], js_meta = [], lua_meta = [], handler = []};
parse_write(_, undefined) ->
    %% JsCode = lists:concat(["        \"", Protocol, "\" : ", "[]"]),
    JsCode = lists:concat(["[]"]),
    %% LuaCode = lists:concat(["        [", Protocol, "] = ", "{}"]),
    LuaCode = lists:concat(["{}"]),
    #code{erl = [], js_meta = JsCode, lua_meta = LuaCode};
parse_write(Protocol, []) ->
    %% erl code
    ErlCode = "write(" ++ integer_to_list(Protocol) ++ ", []) ->\n    {ok, protocol:pack(" ++ integer_to_list(Protocol) ++ ", <<>>)};\n\n",
    %% JsCode = lists:concat(["        \"", Protocol, "\" : ", "[]"]),
    JsCode = lists:concat(["[]"]),
    %% LuaCode = lists:concat(["        [", Protocol, "] = ", "{}"]),
    LuaCode = lists:concat(["{}"]),
    #code{erl = ErlCode, js_meta = JsCode, lua_meta = LuaCode};
parse_write(Protocol, SyntaxList = [_ | _]) ->
    %% set result string explain as protocol
    List = [parse_write_unit(Syntax) || Syntax <- SyntaxList],
    %% collect code args
    ArgList = listing:collect(#field.args, List),
    %% construct erl code
    Procedure = ["\n    " ++ Procedure ++ "," || #field{procedures = Procedure} <- List, Procedure =/= []],
    Packs = string:join(listing:collect(#field.packs, List), ", "),
    ErlCode = lists:concat(["write(", Protocol, ", ", join(ArgList), ") ->", Procedure, "\n    {ok, protocol:pack(", Protocol, ", <<", Packs, ">>)};\n\n"]),
    %% collect unit meta
    MetaList = lists:flatten(listing:collect(#field.meta, List)),
    %% construct html code
    HtmlCode = parse_code_html(Protocol, MetaList),
    %% construct lua/js/cs code
    LuaCode = parse_decode_lua(Protocol, MetaList),
    LuaMeta = parse_meta_lua(Protocol, MetaList),
    JsCode = parse_decode_js(Protocol, MetaList),
    JsMeta = parse_meta_js(Protocol, MetaList),
    CsCode = parse_decode_cs(Protocol, MetaList),
    CsMeta = parse_meta_cs(Protocol, MetaList),
    %% TextCode = lists:flatten([io_lib:format("text(~w, ~s, ~s) ->\n    <<\"~ts\"/utf8>>;\n", [Protocol, Key, Name, Translate]) || {Name, TranslateList} <- maps:to_list(Text), {Key, Translate} <- maps:to_list(TranslateList)]),
    #code{erl = ErlCode, html = HtmlCode, lua_code = LuaCode, lua_meta = LuaMeta, js_code = JsCode, js_meta = JsMeta, cs_code = CsCode, cs_meta = CsMeta, handler = []};
parse_write(_, _) ->
    #code{erl = [], html = [], lua_code = [], lua_meta = [], js_code = [], js_meta = [], cs_code = [], cs_meta = [], handler = []}.

%% parse unit
parse_write_unit(#zero{}) ->
    #field{args = "_"};
parse_write_unit(Unit = #binary{name = Name, default = Default, comment = Comment, explain = Explain}) ->
    HumpName = word:to_hump(case Default of [] -> Name; _ -> Default end),
    Args = io_lib:format("~s", [HumpName]),
    Packs = io_lib:format("~s:~tp/binary", [HumpName, Explain]),
    #field{name = Name, meta = #meta{name = HumpName, type = element(1, Unit), comment = Comment, explain = Explain}, args = Args, packs = Packs};
parse_write_unit(Unit = #bool{name = Name, default = Default, comment = Comment}) ->
    HumpName = word:to_hump(case Default of [] -> Name; _ -> Default end),
    Args = io_lib:format("~s", [HumpName]),
    Packs = io_lib:format("(type:to_flag(~s)):8", [HumpName]),
    #field{name = Name, meta = #meta{name = HumpName, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_write_unit(Unit = #u8{name = Name, default = Default, comment = Comment}) ->
    HumpName = word:to_hump(case Default of [] -> Name; _ -> Default end),
    Args = io_lib:format("~s", [HumpName]),
    Packs = io_lib:format("~s:8", [HumpName]),
    #field{name = Name, meta = #meta{name = HumpName, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_write_unit(Unit = #u16{name = Name, default = Default, comment = Comment}) ->
    HumpName = word:to_hump(case Default of [] -> Name; _ -> Default end),
    Args = io_lib:format("~s", [HumpName]),
    Packs = io_lib:format("~s:16", [HumpName]),
    #field{name = Name, meta = #meta{name = HumpName, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_write_unit(Unit = #u32{name = Name, default = Default, comment = Comment}) ->
    HumpName = word:to_hump(case Default of [] -> Name; _ -> Default end),
    Args = io_lib:format("~s", [HumpName]),
    Packs = io_lib:format("~s:32", [HumpName]),
    #field{name = Name, meta = #meta{name = HumpName, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_write_unit(Unit = #u64{name = Name, default = Default, comment = Comment}) ->
    HumpName = word:to_hump(case Default of [] -> Name; _ -> Default end),
    Args = io_lib:format("~s", [HumpName]),
    Packs = io_lib:format("~s:64", [HumpName]),
    #field{name = Name, meta = #meta{name = HumpName, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_write_unit(Unit = #u128{name = Name, default = Default, comment = Comment}) ->
    HumpName = word:to_hump(case Default of [] -> Name; _ -> Default end),
    Args = io_lib:format("~s", [HumpName]),
    Packs = io_lib:format("~s:128", [HumpName]),
    #field{name = Name, meta = #meta{name = HumpName, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_write_unit(Unit = #i8{name = Name, default = Default, comment = Comment}) ->
    HumpName = word:to_hump(case Default of [] -> Name; _ -> Default end),
    Args = io_lib:format("~s", [HumpName]),
    Packs = io_lib:format("~s:8/signed", [HumpName]),
    #field{name = Name, meta = #meta{name = HumpName, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_write_unit(Unit = #i16{name = Name, default = Default, comment = Comment}) ->
    HumpName = word:to_hump(case Default of [] -> Name; _ -> Default end),
    Args = io_lib:format("~s", [HumpName]),
    Packs = io_lib:format("~s:16/signed", [HumpName]),
    #field{name = Name, meta = #meta{name = HumpName, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_write_unit(Unit = #i32{name = Name, default = Default, comment = Comment}) ->
    HumpName = word:to_hump(case Default of [] -> Name; _ -> Default end),
    Args = io_lib:format("~s", [HumpName]),
    Packs = io_lib:format("~s:32/signed", [HumpName]),
    #field{name = Name, meta = #meta{name = HumpName, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_write_unit(Unit = #i64{name = Name, default = Default, comment = Comment}) ->
    HumpName = word:to_hump(case Default of [] -> Name; _ -> Default end),
    Args = io_lib:format("~s", [HumpName]),
    Packs = io_lib:format("~s:64/signed", [HumpName]),
    #field{name = Name, meta = #meta{name = HumpName, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_write_unit(Unit = #i128{name = Name, default = Default, comment = Comment}) ->
    HumpName = word:to_hump(case Default of [] -> Name; _ -> Default end),
    Args = io_lib:format("~s", [HumpName]),
    Packs = io_lib:format("~s:128/signed", [HumpName]),
    #field{name = Name, meta = #meta{name = HumpName, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_write_unit(Unit = #f32{name = Name, default = Default, comment = Comment}) ->
    HumpName = word:to_hump(case Default of [] -> Name; _ -> Default end),
    Args = io_lib:format("~s", [HumpName]),
    Packs = io_lib:format("~s:32/float", [HumpName]),
    #field{name = Name, meta = #meta{name = HumpName, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_write_unit(Unit = #f64{name = Name, default = Default, comment = Comment}) ->
    HumpName = word:to_hump(case Default of [] -> Name; _ -> Default end),
    Args = io_lib:format("~s", [HumpName]),
    Packs = io_lib:format("~s:64/float", [HumpName]),
    #field{name = Name, meta = #meta{name = HumpName, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_write_unit(Unit = #rst{name = Name, default = Default, comment = Comment}) ->
    HumpName = word:to_hump(case Default of [] -> Name; _ -> Default end),
    Args = io_lib:format("~s", [HumpName]),
    Packs = io_lib:format("(protocol:text(~s))/binary", [HumpName]),
    #field{name = Name, meta = #meta{name = HumpName, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_write_unit(Unit = #bst{name = Name, default = Default, comment = Comment}) ->
    HumpName = word:to_hump(case Default of [] -> Name; _ -> Default end),
    Args = io_lib:format("~s", [HumpName]),
    Packs = io_lib:format("(byte_size(~s)):16, (~s)/binary", [HumpName, HumpName]),
    #field{name = Name, meta = #meta{name = HumpName, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};
parse_write_unit(Unit = #str{name = Name, default = Default, comment = Comment}) ->
    HumpName = word:to_hump(case Default of [] -> Name; _ -> Default end),
    Args = io_lib:format("~s", [HumpName]),
    Packs = io_lib:format("(length(~s)):16, (list_to_binary(~s))/binary", [HumpName, HumpName]),
    #field{name = Name, meta = #meta{name = HumpName, type = element(1, Unit), explain = [], comment = Comment}, args = Args, packs = Packs};

%% structure unit
parse_write_unit(#tuple{name = Name, explain = Explain}) ->
    %% format per unit
    Field = parse_write_unit(Explain),
    Field#field{name = Name};

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
    #field{name = Name, args = HumpName, procedures = Procedure, packs = EtsPacks, meta = #meta{name = Name, type = list, explain = ReviseMeta, comment = Comment}};

parse_write_unit(#list{name = Name, comment = Comment, explain = Explain, key = Key}) ->
    %% hump name
    HumpName = word:to_hump(Name),
    %% format subunit
    #field{args = Args, procedures = Procedures, packs = Packs, meta = Meta} = parse_write_unit(Explain),
    ReviseMeta = lists:flatten([Meta]),
    %% format list pack info
    ListProcedures = io_lib:format("~sBinary = protocol:write_list(fun(~s) -> ~s<<~s>> end, ~s)", [HumpName, Args, case Procedures of [] -> []; _ -> Procedures ++ ", " end, Packs, HumpName]),
    ListPacks = io_lib:format("~sBinary/binary", [HumpName]),
    %% ListPacks = io_lib:format("(length(~s)):16, <<<<~s>> || ~s <- ~s>>/binary", [HumpName, Packs, Args, HumpName]),
    #field{name = Name, args = HumpName, procedures = ListProcedures, packs = ListPacks, meta = #meta{name = Name, type = list, comment = Comment, explain = ReviseMeta, key = Key}};

parse_write_unit(Record) when is_tuple(Record) andalso tuple_size(Record) > 0 andalso is_atom(element(1, Record)) ->
    %% get beam abstract code
    Tag = element(1, Record),
    NameList = beam:find(Tag),
    %% throw error when beam abstract code empty
    NameList == [] andalso erlang:throw(need_to_update_beam_abstract_code),
    tuple_size(Record) =/= length(NameList) andalso erlang:throw(need_to_update_beam_abstract_code),
    %% zip field value and field name
    ZipList = lists:zip(tuple_to_list(Record), NameList),
    %% format per unit
    List = [parse_write_unit(case element(2, Field) of [] -> setelement(2, Field, Name); _ -> setelement(2, setelement(3, Field, element(2, Field)), Name) end) || {Field, Name} <- ZipList, is_unit(Field)],
    %% format function match args
    Args = lists:concat(["#", Tag, "{", string:join([io_lib:format("~s = ~s", [Name, Args]) || #field{name = Name, args = Args} <- List], ", "), "}"]),
    %% format function pack info
    Procedures = string:join(listing:collect(#field.procedures, List, []), ", "),
    Packs = string:join(listing:collect(#field.packs, List, []), ", "),
    #field{args = Args, procedures = Procedures, packs = Packs, name = listing:collect(#field.name, List), meta = lists:flatten(listing:collect(#field.meta, List, []))};
parse_write_unit(Tuple) when is_tuple(Tuple) andalso tuple_size(Tuple) > 0 ->
    %% format per unit
    List = [parse_write_unit(Field) || Field <- tuple_to_list(Tuple)],
    %% format function match args
    Args = lists:concat(["{", string:join([Args || #field{args = Args} <- List], ", "), "}"]),
    %% format function pack info
    Procedures = string:join(listing:collect(#field.procedures, List, []), ", "),
    Packs = string:join(listing:collect(#field.packs, List, []), ", "),
    #field{args = Args, procedures = Procedures, packs = Packs, name = listing:collect(#field.name, List, []), meta = listing:collect(#field.meta, List, [])};
parse_write_unit(Unit) ->
    erlang:throw(lists:flatten(io_lib:format("Unsupported write unit: ~tp", [Unit]))).

%%%===================================================================
%%% Common Tool
%%%===================================================================
%% join string
join([]) ->
    [];
join([H]) ->
    H;
join(List) ->
    lists:concat(["[", string:join(List, ", "), "]"]).

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
is_unit(#tuple{}) -> true;
is_unit(#list{}) -> true;
is_unit(#ets{}) -> true;
is_unit(_) -> false.
