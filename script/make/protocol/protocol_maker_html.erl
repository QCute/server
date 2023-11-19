%%%-------------------------------------------------------------------
%%% @doc
%%% make protocol define to html metadata code
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_maker_html).
-export([format/3]).
-export([parse_code_html/2]).
-include("../../../include/serialize.hrl").
%% ast metadata
-record(meta, {name = [], type, comment = [], explain = [], key}).
%% lang code
-record(code, {protocol = 0, erl = [], handler = [], html = [], lua_code = [], lua_meta = [], js_code = [], js_meta = [], cs_code = [], cs_meta = []}).
%%%===================================================================
%%% API functions
%%%===================================================================
format(ProtocolComment, HtmlName, List) ->
    Head = format_head(HtmlName),
    Body = format_body(ProtocolComment, HtmlName, List),
    lists:concat([Head, "\n", Body]).

format_head(HtmlName) ->
    lists:concat(["<!DOCTYPE html>
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
            align-items: center;
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

        .right > .protocol > .read .digest:hover,
        .right > .protocol > .write .digest:hover {
            background-color: #f4f5f7;
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
            font-weight: bold;
        }

        .right > .protocol > .read .digest .type .map,
        .right > .protocol > .write .digest .type .map {
            color: #ed0d0c;
            font-weight: bold;
        }

        .right > .protocol > .read .digest .type .key,
        .right > .protocol > .write .digest .type .key {
            color: #ed0d0c;
            font-weight: bold;
        }

        .right > .protocol > .read .digest .type .bool,
        .right > .protocol > .write .digest .type .bool {
            color: #9c57b6;
            font-weight: bold;
        }

        .right > .protocol > .read .digest .type .binary,
        .right > .protocol > .write .digest .type .binary {
            color: #1a9f29;
            font-weight: bold;
        }

        .right > .protocol > .read .digest .type .rst,
        .right > .protocol > .write .digest .type .rst,
        .right > .protocol > .read .digest .type .bst,
        .right > .protocol > .write .digest .type .bst,
        .right > .protocol > .read .digest .type .str,
        .right > .protocol > .write .digest .type .str {
            color: #1a9f29;
            font-weight: bold;
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
            font-weight: bold;
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
            font-weight: bold;
        }

        .right > .protocol > .read .digest .type .f32,
        .right > .protocol > .write .digest .type .f32,
        .right > .protocol > .read .digest .type .f64,
        .right > .protocol > .write .digest .type .f64 {
            color: #a71b76;
            font-weight: bold;
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
</head>"]).

format_body(ProtocolComment, HtmlName, List) ->
    lists:concat(["<body onload='load(event)'>
    <div id='nav' class='left'>
",
        string:join([
            format_menu(Protocol, Comment) || {Protocol, Comment, _, _} <- List, Protocol =/= 0
        ], "\n")
        ,"
    </div>
    <div id='panel' class='right'>
        <div class='header'>", ProtocolComment, " - ", HtmlName, "</div>", "\n",
        [
            format_content(Protocol, Comment, ReadCode, WriteCode) || {Protocol, Comment, ReadCode, WriteCode} <- List, Protocol =/= 0
        ], "
    </div>
</body>
</html>"]).

format_menu(Protocol, Comment) ->
    lists:concat([
        "    " "    ", "<div class='protocol' onclick=\"javascript:location.href='#protocol-", Protocol, "'\"><div class='inner'>", Protocol, " - ", Comment, "</div></div>"
    ]).

format_content(Protocol, Comment, ReadCode, WriteCode) ->
    lists:concat([
        "    ", "    ", "<div class='title' id='protocol-", Protocol, "'>", Protocol, " - ", Comment, "</div>", "\n",
        "    ", "    ", "<div class='protocol'>", "\n",
        "    ", "    ", "    ", "<div class='title'>", "\n",
        "    ", "    ", "    ", "    ", "<svg width='16' height='16' viewBox='0 0 48 48' fill='none' xmlns='http://www.w3.org/2000/svg'><path d='M4 30L9 6H39L44 30' stroke='#ed0d0c' stroke-width='4' stroke-linecap='round' stroke-linejoin='round'/><path d='M4 30H14.9091L16.7273 36H31.2727L33.0909 30H44V43H4V30Z' fill='none' stroke='#ed0d0c' stroke-width='4' stroke-linejoin='round'/><path d='M18 20L24 14L30 20' stroke='#ed0d0c' stroke-width='4' stroke-linecap='round' stroke-linejoin='round'/><path d='M24 26V14' stroke='#ed0d0c' stroke-width='4' stroke-linecap='round' stroke-linejoin='round'/></svg>", "\n",
        "    ", "    ", "    ", "    ", "<span style='margin-left: 4px'>发送</span>", "\n",
        "    ", "    ", "    ", "</div>", "\n",
        "    ", "    ", "    ", "<div class='read'>", "\n",
        lists:flatten(ReadCode#code.html),
        "    ", "    ", "    ", "</div>", "\n",
        "    ", "    ", "    ", "<div class='title'>", "\n",
        "    ", "    ", "    ", "    ", "<svg width='16' height='16' viewBox='0 0 48 48' fill='none' xmlns='http://www.w3.org/2000/svg'><path d='M5 30L10 6H38L43 30' stroke='#1a9f29' stroke-width='4' stroke-linecap='round' stroke-linejoin='round'/><path d='M5 30H14.9091L16.7273 36H31.2727L33.0909 30H43V43H5V30Z' fill='none' stroke='#1a9f29' stroke-width='4' stroke-linejoin='round'/><path d='M18 20L24 26L30 20' stroke='#1a9f29' stroke-width='4' stroke-linecap='round' stroke-linejoin='round'/><path d='M24 26V14' stroke='#1a9f29' stroke-width='4' stroke-linecap='round' stroke-linejoin='round'/></svg>", "\n",
        "    ", "    ", "    ", "    ", "<span style='margin-left: 4px'>接收</span>", "\n",
        "    ", "    ", "    ", "</div>", "\n",
        "    ", "    ", "    ", "<div class='write'>", "\n",
        lists:flatten(WriteCode#code.html), "\n",
        "    ", "    ", "    ", "</div>", "\n",
        "    ", "    ", "</div>", "\n"
    ]).

%%%===================================================================
%%% meta
%%%===================================================================
%% html code
parse_code_html(_, []) ->
    "";
parse_code_html(_, Meta) ->
    %% start with 3 tabs(4 space) padding
    Padding = lists:duplicate(3, "    "),
    Codes = parse_code_html_loop(Meta, 4, []),
    %% format one protocol define
    %% lists:concat(["        \"", Protocol, "\" : [\n", Codes, "\n        ]"]).
    %% lists:concat(["[\n", Codes, "\n", Padding, "]"]).
    Code = [
        %% Padding, Padding, "<div class='title' id='protocol-", Protocol, "'>●", Meta#meta.name, "(", Protocol, ")</div>", "\n",
        Padding, Padding, "\n",
        Codes, "\n", "\n"
    ],
    lists:concat(Code).

parse_code_html_loop([], _, List) ->
    %% construct as a list
    string:join(lists:reverse(List), "\n");

parse_code_html_loop([#meta{name = _, type = zero} | T], Depth, List) ->
    parse_code_html_loop(T, Depth, List);

parse_code_html_loop([#meta{name = Name, type = binary, comment = Comment, explain = Length} | T], Depth, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    %% Code = lists:flatten(io_lib:format("~s{\"name\" : \"~s\", \"type\" : \"~s\", \"comment\" : \"~ts\", \"explain\" : ~w}", [Padding, word:to_lower_hump(Name), binary, Comment, Length])),
    Code = lists:flatten(io_lib:format(lists:concat([
        Padding, "<div class='field'>", "\n",
        Padding, "    ", "<div class='digest'>", "\n",
        Padding, "    ", "    ", "<div class='inner name'><div class='pad' style='width: ~tppx;position: relative;'><div class='top' style='width: calc(~tp% - 4px);position: absolute;height: 50%;top: 0; right: 4px;'></div><div class='bottom' style='width: calc(~tp% - 4px);height: 50%; position: absolute;bottom: 0; right: 4px;'></div></div><div class='align' style='width: calc(100% - ~tppx);'><span onclick='copy(event, this)'>名称: ~ts</span><div class='tips'>已复制</div></div></div>", "\n",
        Padding, "    ", "    ", "<div class='inner type'><span onclick='copy(event, this)'>类型: <span class='~ts'>~ts(~w)</span></span><div class='tips'>已复制</div></div>", "\n",
        Padding, "    ", "    ", "<div class='inner comment'><span onclick='copy(event, this)'>注释: ~ts</span><div class='tips'>已复制</div></div>", "\n",
        Padding, "    ", "</div>", "\n",
        Padding, "</div>"]), [(Depth - 3) * 16, 100 / (Depth - 3), 100 / (Depth - 3), (Depth - 3) * 16, word:to_lower_hump(Name), binary, binary, Length, Comment])),
    parse_code_html_loop(T, Depth, [Code | List]);

parse_code_html_loop([#meta{name = Name, type = Type, comment = Comment, explain = []} | T], Depth, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    %% Code = lists:flatten(io_lib:format("~s{\"name\" : \"~s\", \"type\" : \"~s\", \"comment\" : \"~ts\", \"explain\" : ~w}", [Padding, word:to_lower_hump(Name), Type, Comment, []])),
    %% Code = lists:flatten(io_lib:format("~s<div class='field'><div class='digest'><div class='inner comment' onclick='copy(event, this)'>注释: ~ts</div><div class='inner type' onclick='copy(event, this)'>类型: ~ts</div><div class='inner name' onclick='copy(event, this)'>名称: ~ts</div></div></div>", [Padding, Comment, Type, word:to_lower_hump(Name)])),
    Code = lists:flatten(io_lib:format(lists:concat([
        Padding, "<div class='field'>", "\n",
        Padding, "    ", "<div class='digest'>", "\n",
        Padding, "    ", "    ", "<div class='inner name'><div class='pad' style='width: ~tppx;position: relative;'><div class='top' style='width: calc(~tp% - 4px);height: 50%; position: absolute;top: 0; right: 4px;'></div><div class='bottom' style='width: calc(~tp% - 4px);height: 50%; position: absolute;bottom: 0; right: 4px;'></div></div><div class='align' style='width: calc(100% - ~tppx);'><span onclick='copy(event, this)'>名称: ~ts</span><div class='tips'>已复制</div></div></div>", "\n",
        Padding, "    ", "    ", "<div class='inner type'><span onclick='copy(event, this)'>类型: <span class='~ts'>~ts</span></span><div class='tips'>已复制</div></div>", "\n",
        Padding, "    ", "    ", "<div class='inner comment'><span onclick='copy(event, this)'>注释: ~ts</span><div class='tips'>已复制</div></div>", "\n",
        Padding, "    ", "</div>", "\n",
        Padding, "</div>"]), [(Depth - 3) * 16, 100 / (Depth - 3), 100 / (Depth - 3), (Depth - 3) * 16, word:to_lower_hump(Name), Type, Type, Comment])),
    parse_code_html_loop(T, Depth, [Code | List]);

parse_code_html_loop([#meta{name = _, type = tuple, comment = _, explain = Explain = [_ | _]} | T], Depth, List) ->
    %% recursive
    SubCodes = parse_code_html_loop(Explain, Depth, []),
    parse_code_html_loop(T, Depth, [SubCodes | List]);

parse_code_html_loop([#meta{name = _, type = record, comment = _, explain = Explain = [_ | _]} | T], Depth, List) ->
    %% recursive
    SubCodes = parse_code_html_loop(Explain, Depth, []),
    parse_code_html_loop(T, Depth, [SubCodes | List]);

parse_code_html_loop([#meta{name = Name, type = list, explain = Explain = [_ | _], comment = Comment, key = undefined} | T], Depth, List) ->
    %% recursive
    SubCodes = parse_code_html_loop(Explain, Depth + 2, []),
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    %% Code = lists:flatten(io_lib:format("~s{\"name\" : \"~s\", \"type\" : \"~s\", \"comment\" : \"~ts\", \"explain\" : [\n~ts\n~s]}", [Padding, word:to_lower_hump(Name), Type, Comment, SubCodes, Padding])),
    %% Code = lists:flatten(io_lib:format("~s<div class='field'><div class='digest'><div class='inner comment' onclick='copy(event, this)'>注释: ~ts</div><div class='inner type' onclick='copy(event, this)'>类型: ~ts</div><div class='inner name' onclick='copy(event, this)'>名称: ~ts</div></div><div class='sub'><div class='align'></div><div class='explain'>~ts</div></div></div>", [Padding, Comment, Type, word:to_lower_hump(Name), SubCodes])),
    Code = lists:flatten(io_lib:format(lists:concat([
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
        Padding, "</div>"]), [(Depth - 3) * 16, 100 / (Depth - 3), 100 / (Depth - 3), (Depth - 3) * 16, word:to_lower_hump(Name), list, list, Comment, SubCodes])),
    parse_code_html_loop(T, Depth, [Code | List]);

parse_code_html_loop([#meta{name = Name, type = list, explain = Explain = [_ | _], comment = Comment, key = Key} | T], Depth, List) ->
    %% recursive
    SubCodes = parse_code_html_loop(Explain, Depth + 2, []),
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    %% Code = lists:flatten(io_lib:format("~s{\"name\" : \"~s\", \"type\" : \"~s\", \"comment\" : \"~ts\", \"explain\" : [\n~ts\n~s]}", [Padding, word:to_lower_hump(Name), Type, Comment, SubCodes, Padding])),
    %% Code = lists:flatten(io_lib:format("~s<div class='field'><div class='digest'><div class='inner comment' onclick='copy(event, this)'>注释: ~ts</div><div class='inner type' onclick='copy(event, this)'>类型: ~ts</div><div class='inner name' onclick='copy(event, this)'>名称: ~ts</div></div><div class='sub'><div class='align'></div><div class='explain'>~ts</div></div></div>", [Padding, Comment, Type, word:to_lower_hump(Name), SubCodes])),
    Code = lists:flatten(io_lib:format(lists:concat([
        Padding, "<div class='field'>", "\n",
        Padding, "    ", "<div class='digest'>", "\n",
        Padding, "    ", "    ", "<div class='inner name'><div class='pad' style='width: ~tppx;position: relative;'><div class='top' style='width: calc(~tp% - 4px);height: 50%;position: absolute;top: 0; right: 4px;'></div><div class='bottom' style='width: calc(~tp% - 4px);height: 50%; position: absolute;bottom: 0; right: 4px;'></div></div><div class='align' style='width: calc(100% - ~tppx);'><span onclick='copy(event, this)'>名称: ~ts</span><div class='tips'>已复制</div></div></div>", "\n",
        Padding, "    ", "    ", "<div class='inner type'><span onclick='copy(event, this)'>类型: <span class='~ts'>~ts</span></span> / <span onclick='copy(event, this)'>键: <span class='~ts'>~ts</span></span><div class='tips'>已复制</div></div>", "\n",
        Padding, "    ", "    ", "<div class='inner comment'><span onclick='copy(event, this)'>注释: ~ts</span><div class='tips'>已复制</div></div>", "\n",
        Padding, "    ", "</div>", "\n",
        Padding, "    ", "<div class='sub'>", "\n",
        %% Padding, "    ", "    ", "<div class='align'></div>", "\n",
        Padding, "    ", "    ", "<div class='explain'>", "\n",
        "~ts", "\n",
        Padding, "    ", "    ", "</div>", "\n",
        Padding, "    ", "</div>", "\n",
        Padding, "</div>"]), [(Depth - 3) * 16, 100 / (Depth - 3), 100 / (Depth - 3), (Depth - 3) * 16, word:to_lower_hump(Name), map, map, key, word:to_lower_hump(Key), Comment, SubCodes])),
    parse_code_html_loop(T, Depth, [Code | List]);

parse_code_html_loop([#meta{name = Name, type = ets, explain = Explain = [_ | _], comment = Comment, key = undefined} | T], Depth, List) ->
    %% recursive
    SubCodes = parse_code_html_loop(Explain, Depth + 2, []),
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    %% Code = lists:flatten(io_lib:format("~s{\"name\" : \"~s\", \"type\" : \"~s\", \"comment\" : \"~ts\", \"explain\" : [\n~ts\n~s]}", [Padding, word:to_lower_hump(Name), Type, Comment, SubCodes, Padding])),
    %% Code = lists:flatten(io_lib:format("~s<div class='field'><div class='digest'><div class='inner comment' onclick='copy(event, this)'>注释: ~ts</div><div class='inner type' onclick='copy(event, this)'>类型: ~ts</div><div class='inner name' onclick='copy(event, this)'>名称: ~ts</div></div><div class='sub'><div class='align'></div><div class='explain'>~ts</div></div></div>", [Padding, Comment, Type, word:to_lower_hump(Name), SubCodes])),
    Code = lists:flatten(io_lib:format(lists:concat([
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
        Padding, "</div>"]), [(Depth - 3) * 16, 100 / (Depth - 3), 100 / (Depth - 3), (Depth - 3) * 16, word:to_lower_hump(Name), list, list, Comment, SubCodes])),
    parse_code_html_loop(T, Depth, [Code | List]);

parse_code_html_loop([#meta{name = Name, type = ets, explain = Explain = [_ | _], comment = Comment, key = Key} | T], Depth, List) ->
    %% recursive
    SubCodes = parse_code_html_loop(Explain, Depth + 2, []),
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    %% Code = lists:flatten(io_lib:format("~s{\"name\" : \"~s\", \"type\" : \"~s\", \"comment\" : \"~ts\", \"explain\" : [\n~ts\n~s]}", [Padding, word:to_lower_hump(Name), Type, Comment, SubCodes, Padding])),
    %% Code = lists:flatten(io_lib:format("~s<div class='field'><div class='digest'><div class='inner comment' onclick='copy(event, this)'>注释: ~ts</div><div class='inner type' onclick='copy(event, this)'>类型: ~ts</div><div class='inner name' onclick='copy(event, this)'>名称: ~ts</div></div><div class='sub'><div class='align'></div><div class='explain'>~ts</div></div></div>", [Padding, Comment, Type, word:to_lower_hump(Name), SubCodes])),
    Code = lists:flatten(io_lib:format(lists:concat([
        Padding, "<div class='field'>", "\n",
        Padding, "    ", "<div class='digest'>", "\n",
        Padding, "    ", "    ", "<div class='inner name'><div class='pad' style='width: ~tppx;position: relative;'><div class='top' style='width: calc(~tp% - 4px);height: 50%;position: absolute;top: 0; right: 4px;'></div><div class='bottom' style='width: calc(~tp% - 4px);height: 50%; position: absolute;bottom: 0; right: 4px;'></div></div><div class='align' style='width: calc(100% - ~tppx);'><span onclick='copy(event, this)'>名称: ~ts</span><div class='tips'>已复制</div></div></div>", "\n",
        Padding, "    ", "    ", "<div class='inner type'><span onclick='copy(event, this)'>类型: <span class='~ts'>~ts</span></span> / <span onclick='copy(event, this)'>键: <span class='~ts'>~ts</span></span><div class='tips'>已复制</div></div>", "\n",
        Padding, "    ", "    ", "<div class='inner comment'><span onclick='copy(event, this)'>注释: ~ts</span><div class='tips'>已复制</div></div>", "\n",
        Padding, "    ", "</div>", "\n",
        Padding, "    ", "<div class='sub'>", "\n",
        %% Padding, "    ", "    ", "<div class='align'></div>", "\n",
        Padding, "    ", "    ", "<div class='explain'>", "\n",
        "~ts", "\n",
        Padding, "    ", "    ", "</div>", "\n",
        Padding, "    ", "</div>", "\n",
        Padding, "</div>"]), [(Depth - 3) * 16, 100 / (Depth - 3), 100 / (Depth - 3), (Depth - 3) * 16, word:to_lower_hump(Name), map, map, key, word:to_lower_hump(Key), Comment, SubCodes])),
    parse_code_html_loop(T, Depth, [Code | List]).
