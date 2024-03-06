%%%-------------------------------------------------------------------
%%% @doc
%%% make protocol define to html metadata code
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_maker_html).
-export([format/3]).
-export([parse_code_html/2]).
-include("../../../include/serialize.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
format(ProtocolComment, HtmlName, List) ->
    Head = format_head(HtmlName),
    Body = format_body(ProtocolComment, HtmlName, List),
    lists:concat([Head, "\n", Body]).

format_head(HtmlName) ->
    lists:concat(["<!DOCTYPE html>
<html lang='zh-Hans'>
<head>
    <meta charset='UTF-8'>
    <link rel='icon' href='data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiPz48c3ZnIHdpZHRoPSIyNCIgaGVpZ2h0PSIyNCIgdmlld0JveD0iMCAwIDQ4IDQ4IiBmaWxsPSJub25lIiB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciPjxyZWN0IHg9IjgiIHk9IjQiIHdpZHRoPSIzMiIgaGVpZ2h0PSI0MCIgcng9IjIiIHN0cm9rZT0iIzMzMyIgc3Ryb2tlLXdpZHRoPSI0IiBzdHJva2UtbGluZWNhcD0icm91bmQiIHN0cm9rZS1saW5lam9pbj0icm91bmQiLz48cGF0aCBkPSJNMTYgNEgyNVYyMEwyMC41IDE2TDE2IDIwVjRaIiBmaWxsPSJub25lIiBzdHJva2U9IiMzMzMiIHN0cm9rZS13aWR0aD0iNCIgc3Ryb2tlLWxpbmVjYXA9InJvdW5kIiBzdHJva2UtbGluZWpvaW49InJvdW5kIi8+PHBhdGggZD0iTTE2IDI4SDI2IiBzdHJva2U9IiMzMzMiIHN0cm9rZS13aWR0aD0iNCIgc3Ryb2tlLWxpbmVjYXA9InJvdW5kIi8+PHBhdGggZD0iTTE2IDM0SDMyIiBzdHJva2U9IiMzMzMiIHN0cm9rZS13aWR0aD0iNCIgc3Ryb2tlLWxpbmVjYXA9InJvdW5kIi8+PC9zdmc+' type='image/x-icon' />
    <title>", HtmlName, "</title>
    <style>
        html, body { margin: 0; width: 100vw; height: 100vh; display: flex; }
        body { opacity: 0; animation: fade-in 1s forwards; }
        span { white-space: pre; }
        @keyframes fade-in { 0% { opacity: 0; } 100% { opacity: 1; } }
        div { display: flex; }
        a { color: #fff; text-decoration: none; }

        .left {
            width: 250px;
            height: 100vh;
            overflow: auto;
            flex-direction: column;
            align-items: center;
            color: #000;
            background-color: #f4f4f5;
            border-right: 1px solid #f6f6f7;
            /* IE and Edge */
            -ms-overflow-style: none;
            /* Firefox */
            scrollbar-width: none;
        }

        .left::-webkit-scrollbar {
            /* Webkit */
            display: none;
        }

        .left > .protocol {
            width: 88%;
            height: 24px;
            padding: 8px 0px 8px 0px;
            flex-shrink: 0;
            display: flex;
            justify-content: center;
            align-items: center;
        }

        .left > .protocol > .inner:hover {
            background-color: #eaeaeb;
            cursor: pointer;
        }

        .left > .protocol > .inner {
            width: calc(100% - 16px);
            height: 100%;
            display: flex;
            align-items: center;
            font-size: 0.88em;
            padding: 4px 8px 4px 8px;
            border-radius: 4px;
            font-size: 13px;
        }

        .right {
            width: calc(100vw - 250px);
            height: 100vh;
            overflow: auto;
            display: flex;
            flex-direction: column;
            align-items: center;
            background-color: #fff;
            scroll-padding-top: 10px;
        }

        .right > .header {
            margin-top: 16px;
            margin-bottom: 48px;
            font-size: 2em;
            font-weight: bold;
            width: 90%;
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
            font-size: 0.88em;
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

        .right > .protocol > .read .field,
        .right > .protocol > .write .field {
            width: 100%;
            position: relative;
            display: flex;
            flex-direction: column;
            align-items: flex-end;
        }

        .right > .protocol > .read .digest,
        .right > .protocol > .write .digest {
            width: 100%;
            margin-bottom: 4px;
            padding: 8px 0px 8px 0px;
            display: flex;
            justify-content: space-between;
            border-radius: 4px;
            background-color: #fafbfc;
            font-size: 0.88em;
        }

        .right > .protocol > .read .digest:hover,
        .right > .protocol > .write .digest:hover {
            background-color: #f4f5f7;
        }

        .right > .protocol > .read .digest .name,
        .right > .protocol > .write .digest .name {
            width: 50%;
            position: relative;
            cursor: pointer;
        }

        .right > .protocol > .read .digest .type,
        .right > .protocol > .write .digest .type {
            width: 20%;
            position: relative;
            cursor: pointer;
        }

        .right > .protocol > .read .digest .comment,
        .right > .protocol > .write .digest .comment {
            width: 30%;
            position: relative;
            cursor: pointer;
        }

        .right > .protocol > .read .field .sub > .line,
        .right > .protocol > .write .field .sub > .line {
            content: \"\";
            position: absolute;
            top: 28px;
            width: 0;
            height: calc(100% - 60px);
            border-left: 1px dotted #c0c4cc;
            margin-left: -10px;
            z-index: 1;
        }

        .right > .protocol > .read .field .child > .line,
        .right > .protocol > .write .field .child > .line {
            content: \"\";
            position: absolute;
            top: 28px;
            width: 0;
            height: 30px;
            border-left: 1px dotted #c0c4cc;
            margin-left: -10px;
            z-index: 1;
        }

        .right > .protocol > .read .field .sub .bar,
        .right > .protocol > .write .field .sub .bar,
        .right > .protocol > .read .field .child .bar,
        .right > .protocol > .write .field .child .bar {
            content: \"\";
            position: absolute;
            top: 18px;
            width: 26px;
            height: 0;
            border-top: 1px dotted #c0c4cc;
            margin-left: -10px;
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
        .right > .protocol > .write .sub,
        .right > .protocol > .read .child,
        .right > .protocol > .write .child {
            width: 100%;
            display: flex;
            flex-direction: column;
        }

        .right > .protocol > .read .digest .type .object,
        .right > .protocol > .write .digest .type .object {
            color: #ed0d0c;
            font-weight: bold;
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
            inputValue.value = target.innerText.trim();
            inputValue.select();
            document.execCommand('copy');
            document.body.removeChild(inputValue);
            // tips
            const { width, height } = target.parentElement.lastElementChild.getBoundingClientRect();
            target.parentElement.lastElementChild.style.top = `${event.y - height - 8}px`;
            target.parentElement.lastElementChild.style.left = `${event.x - (width / 2)}px`;
            target.parentElement.lastElementChild.style.visibility = 'visible';
            setTimeout(() => { target.parentElement.lastElementChild.style.visibility = 'hidden'; }, 1000);
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

format_content(Protocol, Comment, #data{html = #set{meta = #file{extra = Read}}}, #data{html = #set{meta = #file{extra = Write}}}) ->
    lists:concat([
        "    ", "    ", "<div class='title' id='protocol-", Protocol, "'>", Protocol, " - ", Comment, "</div>", "\n",
        "    ", "    ", "<div class='protocol'>", "\n",
        "    ", "    ", "    ", "<div class='title'>", "\n",
        "    ", "    ", "    ", "    ", "<svg width='16' height='16' viewBox='0 0 48 48' fill='none' xmlns='http://www.w3.org/2000/svg'><path d='M4 30L9 6H39L44 30' stroke='#ed0d0c' stroke-width='4' stroke-linecap='round' stroke-linejoin='round'/><path d='M4 30H14.9091L16.7273 36H31.2727L33.0909 30H44V43H4V30Z' fill='none' stroke='#ed0d0c' stroke-width='4' stroke-linejoin='round'/><path d='M18 20L24 14L30 20' stroke='#ed0d0c' stroke-width='4' stroke-linecap='round' stroke-linejoin='round'/><path d='M24 26V14' stroke='#ed0d0c' stroke-width='4' stroke-linecap='round' stroke-linejoin='round'/></svg>", "\n",
        "    ", "    ", "    ", "    ", "<span style='margin-left: 4px'>发送</span>", "\n",
        "    ", "    ", "    ", "</div>", "\n",
        "    ", "    ", "    ", "<div class='read'>", "\n",
        lists:flatten(Read),
        "    ", "    ", "    ", "</div>", "\n",
        "    ", "    ", "    ", "<div class='title'>", "\n",
        "    ", "    ", "    ", "    ", "<svg width='16' height='16' viewBox='0 0 48 48' fill='none' xmlns='http://www.w3.org/2000/svg'><path d='M5 30L10 6H38L43 30' stroke='#1a9f29' stroke-width='4' stroke-linecap='round' stroke-linejoin='round'/><path d='M5 30H14.9091L16.7273 36H31.2727L33.0909 30H43V43H5V30Z' fill='none' stroke='#1a9f29' stroke-width='4' stroke-linejoin='round'/><path d='M18 20L24 26L30 20' stroke='#1a9f29' stroke-width='4' stroke-linecap='round' stroke-linejoin='round'/><path d='M24 26V14' stroke='#1a9f29' stroke-width='4' stroke-linecap='round' stroke-linejoin='round'/></svg>", "\n",
        "    ", "    ", "    ", "    ", "<span style='margin-left: 4px'>接收</span>", "\n",
        "    ", "    ", "    ", "</div>", "\n",
        "    ", "    ", "    ", "<div class='write'>", "\n",
        lists:flatten(Write), "\n",
        "    ", "    ", "    ", "</div>", "\n",
        "    ", "    ", "</div>", "\n"
    ]).

%%%===================================================================
%%% meta
%%%===================================================================
%% html code
parse_code_html(_, Meta = #meta{type = Type}) ->
    %% start with 3 tabs(4 space) padding
    %% Padding = lists:duplicate(3, "    "),
    NewName = maps:get(?IS_UNIT(Type), #{true => '~', false => ""}),
    Codes = parse_code_html_loop([Meta#meta{name = NewName}], 4, Type, undefined, []),
    %% format one protocol define
    %% lists:concat(["        \"", Protocol, "\" &nbsp;[\n", Codes, "\n        ]"]).
    %% lists:concat(["[\n", Codes, "\n", Padding, "]"]).
    Code = lists:concat([
        %% Padding, Padding, "<div class='title' id='protocol-", Protocol, "'>·", Meta#meta.name, "(", Protocol, ")</div>", "\n",
        "\n",
        Codes, "\n",
        "\n"
    ]),

    #file{extra = Code}.

parse_code_html_loop([], _, _, _, List) ->
    %% construct as a list
    string:join(lists:reverse(List), "\n");

parse_code_html_loop([#meta{name = _, type = zero} | T], Depth, Parent, Key, List) ->
    parse_code_html_loop(T, Depth, Parent, Key, List);

parse_code_html_loop([#meta{name = Name, type = binary, explain = Length, comment = Comment} | T], Depth, Parent, Key, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    Code = lists:flatten(lists:concat([
        Padding, "<div class='field'>", "\n",
        Padding, "    ", "<div class='bar' style='left: ", (Depth - 4) * 16 - 4, "px;'></div>", "\n",
        Padding, "    ", "<div class='digest'>", "\n",
        Padding, "    ", "    ", "<div class='inner name'>", "\n",
        Padding, "    ", "    ", "    ", "<div class='pad' style='width: ", (Depth - 3) * 16, "px; position: relative;'>", "\n",
        Padding, "    ", "    ", "    ", "    ", "<div class='top' style='width: calc(" , 100 / (Depth - 3), "% - 4px);position: absolute;height: 50%;top: 0; right: 4px;'></div>", "\n",
        Padding, "    ", "    ", "    ", "    ", "<div class='bottom' style='width: calc(", (Depth - 3) * 16, "% - 4px);height: 50%; position: absolute;bottom: 0; right: 4px;'></div>", "\n",
        Padding, "    ", "    ", "    ", "</div>", "\n",
        Padding, "    ", "    ", "    ", "<div class='align' style='width: calc(100% - ", (Depth - 3) * 16, "px);'>", "\n",
        Padding, "    ", "    ", "    ", "    ", "<span onclick='copy(event, this)'>", word:to_lower_hump(Name), "</span>", "\n",
        Padding, "    ", "    ", "    ", "    ", "<div class='tips'>已复制</div>", "\n",
        Padding, "    ", "    ", "    ", "</div>", "\n",
        Padding, "    ", "    ", "</div>", "\n",
        Padding, "    ", "    ", "<div class='inner type'><span onclick='copy(event, this)'><span class='", binary, "'>", binary, "(", Length, ")</span></span><div class='tips'>已复制</div></div>", "\n",
        Padding, "    ", "    ", "<div class='inner comment'><span onclick='copy(event, this)'>", Comment, "</span><div class='tips'>已复制</div></div>", "\n",
        Padding, "    ", "</div>", "\n",
        Padding, "</div>"
    ])),
    parse_code_html_loop(T, Depth, Parent, Key, [Code | List]);

parse_code_html_loop([#meta{name = Name, type = Type, explain = undefined, comment = Comment} | T], Depth, Parent, Key, List) ->
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    Code = lists:flatten(lists:concat([
        Padding, "<div class='field'>", "\n",
        Padding, "    ", "<div class='bar' style='left: ", (Depth - 4) * 16 - 4, "px;'></div>", "\n",
        Padding, "    ", "<div class='digest'>", "\n",
        Padding, "    ", "    ", "<div class='inner name'>", "\n",
        Padding, "    ", "    ", "    ", "<div class='pad' style='width: ", (Depth - 3) * 16, "px;position: relative;'>", "\n",
        Padding, "    ", "    ", "    ", "    ", "<div class='top' style='width: calc(" , 100 / (Depth - 3), "% - 4px);height: 50%; position: absolute;top: 0; right: 4px;'></div>", "\n",
        Padding, "    ", "    ", "    ", "    ", "<div class='bottom' style='width: calc(", (Depth - 3) * 16, "% - 4px);height: 50%; position: absolute;bottom: 0; right: 4px;'></div>", "\n",
        Padding, "    ", "    ", "    ", "</div>", "\n",
        Padding, "    ", "    ", "    ", "<div class='align' style='width: calc(100% - ", (Depth - 3) * 16, "px);'>", "\n",
        Padding, "    ", "    ", "    ", "    ", "<span onclick='copy(event, this)'>", word:to_lower_hump(Name), "</span>", "\n",
        Padding, "    ", "    ", "    ", "    ", "<div class='tips'>已复制</div>", "\n",
        Padding, "    ", "    ", "    ", "</div>", "\n",
        Padding, "    ", "    ", "</div>", "\n",
        Padding, "    ", "    ", "<div class='inner type'><span onclick='copy(event, this)'><span class='", Type, "'>", Type, "</span></span><div class='tips'>已复制</div></div>", "\n",
        Padding, "    ", "    ", "<div class='inner comment'><span onclick='copy(event, this)'>", Comment, "</span><div class='tips'>已复制</div></div>", "\n",
        Padding, "    ", "</div>", "\n",
        Padding, "</div>"
    ])),
    parse_code_html_loop(T, Depth, Parent, Key, [Code | List]);

parse_code_html_loop([#meta{name = Name, type = tuple, explain = Explain, comment = Comment} | T], Depth, Parent, Key, List) ->
    %% recursive
    SubCodes = parse_code_html_loop(tuple_to_list(Explain), Depth + 2, tuple, undefined, []),
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    Type = "object",
    NameKey = lists:concat(["</span>[<span onclick='copy(event, this)'>", word:to_lower_hump(Key),  "</span><span>] = "]),
    IndexKey = maps:get(Key, #{undefined => ""}, NameKey),
    FieldName = maps:get(Parent, #{list => IndexKey, ets => IndexKey}, [lists:concat(["</span><span onclick='copy(event, this)'>", word:to_lower_hump(Name), "</span><span> "]) || Name =/= []]),
    Code = lists:flatten(lists:concat([
        Padding, "<div class='field'>", "\n",
        Padding, "    ", "<div class='bar' style='left: ", (Depth - 4) * 16 - 4, "px;'></div>", "\n",
        Padding, "    ", "<div class='digest'>", "\n",
        Padding, "    ", "    ", "<div class='inner name'>", "\n",
        Padding, "    ", "    ", "    ", "<div class='pad' style='width: ", (Depth - 3) * 16, "px;position: relative;'>", "\n",
        Padding, "    ", "    ", "    ", "    ", "<div class='top' style='width: calc(" , 100 / (Depth - 3), "% - 4px);height: 50%; position: absolute;top: 0; right: 4px;'></div>", "\n",
        Padding, "    ", "    ", "    ", "    ", "<div class='bottom' style='width: calc(", (Depth - 3) * 16, "% - 4px);height: 50%; position: absolute;bottom: 0; right: 4px;'></div>", "\n",
        Padding, "    ", "    ", "    ", "</div>", "\n",
        Padding, "    ", "    ", "    ", "<div class='align' style='width: calc(100% - ", (Depth - 3) * 16, "px);'>", "\n",
        Padding, "    ", "    ", "    ", "    ", "<span>", FieldName, "</span><span>{</span>", "\n",
        Padding, "    ", "    ", "    ", "    ", "<div class='tips'>已复制</div>", "\n",
        Padding, "    ", "    ", "    ", "</div>", "\n",
        Padding, "    ", "    ", "</div>", "\n",
        Padding, "    ", "    ", "<div class='inner type'><span onclick='copy(event, this)'><span class='", Type, "'>", Type, "</span></span><div class='tips'>已复制</div></div>", "\n",
        Padding, "    ", "    ", "<div class='inner comment'><span onclick='copy(event, this)'>", Comment, "</span><div class='tips'>已复制</div></div>", "\n",
        Padding, "    ", "</div>", "\n",
        Padding, "    ", "<div class='sub'>", "\n",
        Padding, "    ", "    ", "<div class='line' style='left: ", (Depth - 2) * 16 - 4, "px;'></div>", "\n",
        %% Padding, "    ", "    ", "<div class='explain'>", "\n",
        SubCodes, "\n",
        %% Padding, "    ", "    ", "</div>", "\n",
        Padding, "    ", "</div>", "\n",
        Padding, "    ", "<div class='digest'>", "\n",
        Padding, "    ", "    ", "<div class='inner name'>", "\n",
        Padding, "    ", "    ", "    ", "<div class='pad' style='width: ", (Depth - 3) * 16, "px;position: relative;'>", "\n",
        Padding, "    ", "    ", "    ", "    ", "<div class='top' style='width: calc(" , 100 / (Depth - 3), "% - 4px);height: 50%; position: absolute;top: 0; right: 4px;'></div>", "\n",
        Padding, "    ", "    ", "    ", "    ", "<div class='bottom' style='width: calc(", (Depth - 3) * 16, "% - 4px);height: 50%; position: absolute;bottom: 0; right: 4px;'></div>", "\n",
        Padding, "    ", "    ", "    ", "</div>", "\n",
        Padding, "    ", "    ", "    ", "<div class='align' style='width: calc(100% - ", (Depth - 3) * 16, "px);'>", "\n",
        Padding, "    ", "    ", "    ", "    ", "<span>}</span>", "\n",
        Padding, "    ", "    ", "    ", "    ", "<div class='tips'>已复制</div>", "\n",
        Padding, "    ", "    ", "    ", "</div>", "\n",
        Padding, "    ", "    ", "</div>", "\n",
        Padding, "    ", "</div>", "\n",
        Padding, "</div>"
    ])),
    parse_code_html_loop(T, Depth, Parent, Key, [Code | List]);

parse_code_html_loop([#meta{name = Name, type = record, explain = Explain, comment = Comment} | T], Depth, Parent, Key, List) ->
    SubExplain = [Meta || Meta = #meta{} <- tuple_to_list(Explain)],

    %% recursive
    SubCodes = parse_code_html_loop(SubExplain, Depth + 2, record, undefined, []),
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    Type = "object",
    NameKey = lists:concat(["</span>[<span onclick='copy(event, this)'>", word:to_lower_hump(Key),  "</span><span>] = "]),
    IndexKey = maps:get(Key, #{undefined => ""}, NameKey),
    FieldName = maps:get(Parent, #{list => IndexKey, ets => IndexKey}, [lists:concat(["</span><span onclick='copy(event, this)'>", word:to_lower_hump(Name), "</span><span> "]) || Name =/= []]),
    Code = lists:flatten(lists:concat([
        Padding, "<div class='field'>", "\n",
        Padding, "    ", "<div class='bar' style='left: ", (Depth - 4) * 16 - 4, "px;'></div>", "\n",
        Padding, "    ", "<div class='digest'>", "\n",
        Padding, "    ", "    ", "<div class='inner name'>", "\n",
        Padding, "    ", "    ", "    ", "<div class='pad' style='width: ", (Depth - 3) * 16, "px;position: relative;'>", "\n",
        Padding, "    ", "    ", "    ", "    ", "<div class='top' style='width: calc(" , 100 / (Depth - 3), "% - 4px);height: 50%; position: absolute;top: 0; right: 4px;'></div>", "\n",
        Padding, "    ", "    ", "    ", "    ", "<div class='bottom' style='width: calc(", (Depth - 3) * 16, "% - 4px);height: 50%; position: absolute;bottom: 0; right: 4px;'></div>", "\n",
        Padding, "    ", "    ", "    ", "</div>", "\n",
        Padding, "    ", "    ", "    ", "<div class='align' style='width: calc(100% - ", (Depth - 3) * 16, "px);'>", "\n",
        Padding, "    ", "    ", "    ", "    ", "<span>", FieldName, "</span><span>{</span>", "\n",
        Padding, "    ", "    ", "    ", "    ", "<div class='tips'>已复制</div>", "\n",
        Padding, "    ", "    ", "    ", "</div>", "\n",
        Padding, "    ", "    ", "</div>", "\n",
        Padding, "    ", "    ", "<div class='inner type'><span onclick='copy(event, this)'><span class='", Type, "'>", Type, "</span></span><div class='tips'>已复制</div></div>", "\n",
        Padding, "    ", "    ", "<div class='inner comment'><span onclick='copy(event, this)'>", Comment, "</span><div class='tips'>已复制</div></div>", "\n",
        Padding, "    ", "</div>", "\n",
        Padding, "    ", "<div class='sub'>", "\n",
        Padding, "    ", "    ", "<div class='line' style='left: ", (Depth - 2) * 16 - 4, "px;'></div>", "\n",
        %% Padding, "    ", "    ", "<div class='explain'>", "\n",
        SubCodes, "\n",
        %% Padding, "    ", "    ", "</div>", "\n",
        Padding, "    ", "</div>", "\n",
        Padding, "    ", "<div class='digest'>", "\n",
        Padding, "    ", "    ", "<div class='inner name'>", "\n",
        Padding, "    ", "    ", "    ", "<div class='pad' style='width: ", (Depth - 3) * 16, "px;position: relative;'>", "\n",
        Padding, "    ", "    ", "    ", "    ", "<div class='top' style='width: calc(" , 100 / (Depth - 3), "% - 4px);height: 50%; position: absolute;top: 0; right: 4px;'></div>", "\n",
        Padding, "    ", "    ", "    ", "    ", "<div class='bottom' style='width: calc(", (Depth - 3) * 16, "% - 4px);height: 50%; position: absolute;bottom: 0; right: 4px;'></div>", "\n",
        Padding, "    ", "    ", "    ", "</div>", "\n",
        Padding, "    ", "    ", "    ", "<div class='align' style='width: calc(100% - ", (Depth - 3) * 16, "px);'>", "\n",
        Padding, "    ", "    ", "    ", "    ", "<span>}</span>", "\n",
        Padding, "    ", "    ", "    ", "    ", "<div class='tips'>已复制</div>", "\n",
        Padding, "    ", "    ", "    ", "</div>", "\n",
        Padding, "    ", "    ", "</div>", "\n",
        Padding, "    ", "</div>", "\n",
        Padding, "</div>"
    ])),
    parse_code_html_loop(T, Depth, Parent, Key, [Code | List]);

parse_code_html_loop([#meta{name = Name, type = maps, explain = Explain, comment = Comment} | T], Depth, Parent, Key, List) ->
    %% recursive
    SubCodes = parse_code_html_loop(maps:values(Explain), Depth + 2, maps, undefined, []),
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    Type = "object",
    NameKey = lists:concat(["</span>[<span onclick='copy(event, this)'>", word:to_lower_hump(Key),  "</span><span>] = "]),
    IndexKey = maps:get(Key, #{undefined => ""}, NameKey),
    FieldName = maps:get(Parent, #{list => IndexKey, ets => IndexKey}, lists:concat(["</span><span onclick='copy(event, this)'>", word:to_lower_hump(Name), "</span><span> "])),
    Code = lists:flatten(lists:concat([
        Padding, "<div class='field'>", "\n",
        Padding, "    ", "<div class='bar' style='left: ", (Depth - 4) * 16 - 4, "px;'></div>", "\n",
        Padding, "    ", "<div class='digest'>", "\n",
        Padding, "    ", "    ", "<div class='inner name'>", "\n",
        Padding, "    ", "    ", "    ", "<div class='pad' style='width: ", (Depth - 3) * 16, "px;position: relative;'>", "\n",
        Padding, "    ", "    ", "    ", "    ", "<div class='top' style='width: calc(" , 100 / (Depth - 3), "% - 4px);height: 50%; position: absolute;top: 0; right: 4px;'></div>", "\n",
        Padding, "    ", "    ", "    ", "    ", "<div class='bottom' style='width: calc(", (Depth - 3) * 16, "% - 4px);height: 50%; position: absolute;bottom: 0; right: 4px;'></div>", "\n",
        Padding, "    ", "    ", "    ", "</div>", "\n",
        Padding, "    ", "    ", "    ", "<div class='align' style='width: calc(100% - ", (Depth - 3) * 16, "px);'>", "\n",
        Padding, "    ", "    ", "    ", "    ", "<span>", FieldName, "</span><span>{</span>", "\n",
        Padding, "    ", "    ", "    ", "    ", "<div class='tips'>已复制</div>", "\n",
        Padding, "    ", "    ", "    ", "</div>", "\n",
        Padding, "    ", "    ", "</div>", "\n",
        Padding, "    ", "    ", "<div class='inner type'><span onclick='copy(event, this)'><span class='", Type, "'>", Type, "</span></span><div class='tips'>已复制</div></div>", "\n",
        Padding, "    ", "    ", "<div class='inner comment'><span onclick='copy(event, this)'>", Comment, "</span><div class='tips'>已复制</div></div>", "\n",
        Padding, "    ", "</div>", "\n",
        Padding, "    ", "<div class='sub'>", "\n",
        Padding, "    ", "    ", "<div class='line' style='left: ", (Depth - 2) * 16 - 4, "px;'></div>", "\n",
        %% Padding, "    ", "    ", "<div class='explain'>", "\n",
        SubCodes, "\n",
        %% Padding, "    ", "    ", "</div>", "\n",
        Padding, "    ", "</div>", "\n",
        Padding, "    ", "<div class='digest'>", "\n",
        Padding, "    ", "    ", "<div class='inner name'>", "\n",
        Padding, "    ", "    ", "    ", "<div class='pad' style='width: ", (Depth - 3) * 16, "px;position: relative;'>", "\n",
        Padding, "    ", "    ", "    ", "    ", "<div class='top' style='width: calc(" , 100 / (Depth - 3), "% - 4px);height: 50%; position: absolute;top: 0; right: 4px;'></div>", "\n",
        Padding, "    ", "    ", "    ", "    ", "<div class='bottom' style='width: calc(", (Depth - 3) * 16, "% - 4px);height: 50%; position: absolute;bottom: 0; right: 4px;'></div>", "\n",
        Padding, "    ", "    ", "    ", "</div>", "\n",
        Padding, "    ", "    ", "    ", "<div class='align' style='width: calc(100% - ", (Depth - 3) * 16, "px);'>", "\n",
        Padding, "    ", "    ", "    ", "    ", "<span>}</span>", "\n",
        Padding, "    ", "    ", "    ", "    ", "<div class='tips'>已复制</div>", "\n",
        Padding, "    ", "    ", "    ", "</div>", "\n",
        Padding, "    ", "    ", "</div>", "\n",
        Padding, "    ", "</div>", "\n",
        Padding, "</div>"
    ])),
    parse_code_html_loop(T, Depth, Parent, Key, [Code | List]);

parse_code_html_loop([#meta{name = Name, type = list, explain = Explain, comment = Comment, key = undefined} | T], Depth, Parent, Key, List) ->
    SubExplain = [Meta#meta{name = maps:get(SubName == [] andalso ?IS_UNIT(SubType), #{true => '~', false => SubName})} || Meta = #meta{name = SubName, type = SubType} <- Explain],
    %% recursive
    SubCodes = parse_code_html_loop(SubExplain, Depth + 2, list, undefined, []),
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    Type = "list",
    Code = lists:flatten(lists:concat([
        Padding, "<div class='field'>", "\n",
        Padding, "    ", "<div class='bar' style='left: ", (Depth - 4) * 16 - 4, "px;'></div>", "\n",
        Padding, "    ", "<div class='digest'>", "\n",
        Padding, "    ", "    ", "<div class='inner name'>", "\n",
        Padding, "    ", "    ", "    ", "<div class='pad' style='width: ", (Depth - 3) * 16, "px;position: relative;'>", "\n",
        Padding, "    ", "    ", "    ", "    ", "<div class='top' style='width: calc(" , 100 / (Depth - 3), "% - 4px);height: 50%; position: absolute;top: 0; right: 4px;'></div>", "\n",
        Padding, "    ", "    ", "    ", "    ", "<div class='bottom' style='width: calc(", (Depth - 3) * 16, "% - 4px);height: 50%; position: absolute;bottom: 0; right: 4px;'></div>", "\n",
        Padding, "    ", "    ", "    ", "</div>", "\n",
        Padding, "    ", "    ", "    ", "<div class='align' style='width: calc(100% - ", (Depth - 3) * 16, "px);'>", "\n",
        Padding, "    ", "    ", "    ", "    ", "<span onclick='copy(event, this)'>", word:to_lower_hump(Name), "</span><span> [</span>", "\n",
        Padding, "    ", "    ", "    ", "    ", "<div class='tips'>已复制</div>", "\n",
        Padding, "    ", "    ", "    ", "</div>", "\n",
        Padding, "    ", "    ", "</div>", "\n",
        Padding, "    ", "    ", "<div class='inner type'><span onclick='copy(event, this)'><span class='", Type, "'>", Type, "</span></span><div class='tips'>已复制</div></div>", "\n",
        Padding, "    ", "    ", "<div class='inner comment'><span onclick='copy(event, this)'>", Comment, "</span><div class='tips'>已复制</div></div>", "\n",
        Padding, "    ", "</div>", "\n",
        Padding, "    ", "<div class='child'>", "\n",
        Padding, "    ", "    ", "<div class='line' style='left: ", (Depth - 2) * 16 - 4, "px;'></div>", "\n",
        %% Padding, "    ", "    ", "<div class='explain'>", "\n",
        SubCodes, "\n",
        %% Padding, "    ", "    ", "</div>", "\n",
        Padding, "    ", "</div>", "\n",
        Padding, "    ", "<div class='digest'>", "\n",
        Padding, "    ", "    ", "<div class='inner name'>", "\n",
        Padding, "    ", "    ", "    ", "<div class='pad' style='width: ", (Depth - 3) * 16, "px;position: relative;'>", "\n",
        Padding, "    ", "    ", "    ", "    ", "<div class='top' style='width: calc(" , 100 / (Depth - 3), "% - 4px);height: 50%; position: absolute;top: 0; right: 4px;'></div>", "\n",
        Padding, "    ", "    ", "    ", "    ", "<div class='bottom' style='width: calc(", (Depth - 3) * 16, "% - 4px);height: 50%; position: absolute;bottom: 0; right: 4px;'></div>", "\n",
        Padding, "    ", "    ", "    ", "</div>", "\n",
        Padding, "    ", "    ", "    ", "<div class='align' style='width: calc(100% - ", (Depth - 3) * 16, "px);'>", "\n",
        Padding, "    ", "    ", "    ", "    ", "<span>]</span>", "\n",
        Padding, "    ", "    ", "    ", "    ", "<div class='tips'>已复制</div>", "\n",
        Padding, "    ", "    ", "    ", "</div>", "\n",
        Padding, "    ", "    ", "</div>", "\n",
        Padding, "    ", "</div>", "\n",
        Padding, "</div>"
    ])),
    parse_code_html_loop(T, Depth, Parent, Key, [Code | List]);

parse_code_html_loop([#meta{name = Name, type = list, explain = Explain, comment = Comment, key = SubKey} | T], Depth, Parent, Key, List) ->
    SubExplain = [Meta#meta{name = maps:get(SubName == [] andalso ?IS_UNIT(SubType), #{true => '~', false => SubName})} || Meta = #meta{name = SubName, type = SubType} <- Explain],
    %% recursive
    SubCodes = parse_code_html_loop(SubExplain, Depth + 2, list, SubKey, []),
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    Type = "map",
    Code = lists:flatten(lists:concat([
        Padding, "<div class='field'>", "\n",
        Padding, "    ", "<div class='bar' style='left: ", (Depth - 4) * 16 - 4, "px;'></div>", "\n",
        Padding, "    ", "<div class='digest'>", "\n",
        Padding, "    ", "    ", "<div class='inner name'>", "\n",
        Padding, "    ", "    ", "    ", "<div class='pad' style='width: ", (Depth - 3) * 16, "px;position: relative;'>", "\n",
        Padding, "    ", "    ", "    ", "    ", "<div class='top' style='width: calc(" , 100 / (Depth - 3), "% - 4px);height: 50%; position: absolute;top: 0; right: 4px;'></div>", "\n",
        Padding, "    ", "    ", "    ", "    ", "<div class='bottom' style='width: calc(", (Depth - 3) * 16, "% - 4px);height: 50%; position: absolute;bottom: 0; right: 4px;'></div>", "\n",
        Padding, "    ", "    ", "    ", "</div>", "\n",
        Padding, "    ", "    ", "    ", "<div class='align' style='width: calc(100% - ", (Depth - 3) * 16, "px);'>", "\n",
        Padding, "    ", "    ", "    ", "    ", "<span onclick='copy(event, this)'>", word:to_lower_hump(Name), "</span><span> [</span>", "\n",
        Padding, "    ", "    ", "    ", "    ", "<div class='tips'>已复制</div>", "\n",
        Padding, "    ", "    ", "    ", "</div>", "\n",
        Padding, "    ", "    ", "</div>", "\n",
        Padding, "    ", "    ", "<div class='inner type'><span onclick='copy(event, this)'><span class='", Type, "'>", Type, "</span></span> / <span onclick='copy(event, this)'>键: <span class='", key, "'>", word:to_lower_hump(SubKey), "</span></span><div class='tips'>已复制</div></div>", "\n",
        Padding, "    ", "    ", "<div class='inner comment'><span onclick='copy(event, this)'>", Comment, "</span><div class='tips'>已复制</div></div>", "\n",
        Padding, "    ", "</div>", "\n",
        Padding, "    ", "<div class='child'>", "\n",
        Padding, "    ", "    ", "<div class='line' style='left: ", (Depth - 2) * 16 - 4, "px;'></div>", "\n",
        %% Padding, "    ", "    ", "<div class='explain'>", "\n",
        SubCodes, "\n",
        %% Padding, "    ", "    ", "</div>", "\n",
        Padding, "    ", "</div>", "\n",
        Padding, "    ", "<div class='digest'>", "\n",
        Padding, "    ", "    ", "<div class='inner name'>", "\n",
        Padding, "    ", "    ", "    ", "<div class='pad' style='width: ", (Depth - 3) * 16, "px;position: relative;'>", "\n",
        Padding, "    ", "    ", "    ", "    ", "<div class='top' style='width: calc(" , 100 / (Depth - 3), "% - 4px);height: 50%; position: absolute;top: 0; right: 4px;'></div>", "\n",
        Padding, "    ", "    ", "    ", "    ", "<div class='bottom' style='width: calc(", (Depth - 3) * 16, "% - 4px);height: 50%; position: absolute;bottom: 0; right: 4px;'></div>", "\n",
        Padding, "    ", "    ", "    ", "</div>", "\n",
        Padding, "    ", "    ", "    ", "<div class='align' style='width: calc(100% - ", (Depth - 3) * 16, "px);'>", "\n",
        Padding, "    ", "    ", "    ", "    ", "<span>]</span>", "\n",
        Padding, "    ", "    ", "    ", "    ", "<div class='tips'>已复制</div>", "\n",
        Padding, "    ", "    ", "    ", "</div>", "\n",
        Padding, "    ", "    ", "</div>", "\n",
        Padding, "    ", "</div>", "\n",
        Padding, "</div>"
    ])),
    parse_code_html_loop(T, Depth, Parent, Key, [Code | List]);

parse_code_html_loop([#meta{name = Name, type = ets, explain = Explain, comment = Comment, key = []} | T], Depth, Parent, Key, List) ->
    SubExplain = [Meta#meta{name = maps:get(SubName == [] andalso ?IS_UNIT(SubType), #{true => '~', false => SubName})} || Meta = #meta{name = SubName, type = SubType} <- Explain],
    %% recursive
    SubCodes = parse_code_html_loop(SubExplain, Depth + 2, ets, undefined, []),
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    Type = "list",
    Code = lists:flatten(lists:concat([
        Padding, "<div class='field'>", "\n",
        Padding, "    ", "<div class='bar' style='left: ", (Depth - 4) * 16 - 4, "px;'></div>", "\n",
        Padding, "    ", "<div class='digest'>", "\n",
        Padding, "    ", "    ", "<div class='inner name'>", "\n",
        Padding, "    ", "    ", "    ", "<div class='pad' style='width: ", (Depth - 3) * 16, "px;position: relative;'>", "\n",
        Padding, "    ", "    ", "    ", "    ", "<div class='top' style='width: calc(" , 100 / (Depth - 3), "% - 4px);height: 50%; position: absolute;top: 0; right: 4px;'></div>", "\n",
        Padding, "    ", "    ", "    ", "    ", "<div class='bottom' style='width: calc(", (Depth - 3) * 16, "% - 4px);height: 50%; position: absolute;bottom: 0; right: 4px;'></div>", "\n",
        Padding, "    ", "    ", "    ", "</div>", "\n",
        Padding, "    ", "    ", "    ", "<div class='align' style='width: calc(100% - ", (Depth - 3) * 16, "px);'>", "\n",
        Padding, "    ", "    ", "    ", "    ", "<span onclick='copy(event, this)'>", word:to_lower_hump(Name), "</span><span> [</span>", "\n",
        Padding, "    ", "    ", "    ", "    ", "<div class='tips'>已复制</div>", "\n",
        Padding, "    ", "    ", "    ", "</div>", "\n",
        Padding, "    ", "    ", "</div>", "\n",
        Padding, "    ", "    ", "<div class='inner type'><span onclick='copy(event, this)'><span class='", Type, "'>", Type, "</span></span><div class='tips'>已复制</div></div>", "\n",
        Padding, "    ", "    ", "<div class='inner comment'><span onclick='copy(event, this)'>", Comment, "</span><div class='tips'>已复制</div></div>", "\n",
        Padding, "    ", "</div>", "\n",
        Padding, "    ", "<div class='child'>", "\n",
        Padding, "    ", "    ", "<div class='line' style='left: ", (Depth - 2) * 16 - 4, "px;'></div>", "\n",
        %% Padding, "    ", "    ", "<div class='explain'>", "\n",
        SubCodes, "\n",
        %% Padding, "    ", "    ", "</div>", "\n",
        Padding, "    ", "</div>", "\n",
        Padding, "    ", "<div class='digest'>", "\n",
        Padding, "    ", "    ", "<div class='inner name'>", "\n",
        Padding, "    ", "    ", "    ", "<div class='pad' style='width: ", (Depth - 3) * 16, "px;position: relative;'>", "\n",
        Padding, "    ", "    ", "    ", "    ", "<div class='top' style='width: calc(" , 100 / (Depth - 3), "% - 4px);height: 50%; position: absolute;top: 0; right: 4px;'></div>", "\n",
        Padding, "    ", "    ", "    ", "    ", "<div class='bottom' style='width: calc(", (Depth - 3) * 16, "% - 4px);height: 50%; position: absolute;bottom: 0; right: 4px;'></div>", "\n",
        Padding, "    ", "    ", "    ", "</div>", "\n",
        Padding, "    ", "    ", "    ", "<div class='align' style='width: calc(100% - ", (Depth - 3) * 16, "px);'>", "\n",
        Padding, "    ", "    ", "    ", "    ", "<span>]</span>", "\n",
        Padding, "    ", "    ", "    ", "    ", "<div class='tips'>已复制</div>", "\n",
        Padding, "    ", "    ", "    ", "</div>", "\n",
        Padding, "    ", "    ", "</div>", "\n",
        Padding, "    ", "</div>", "\n",
        Padding, "</div>"
    ])),
    parse_code_html_loop(T, Depth, Parent, Key, [Code | List]);

parse_code_html_loop([#meta{name = Name, type = ets, explain = Explain, comment = Comment, key = [SubKey]} | T], Depth, Parent, Key, List) ->

    SubExplain = [Meta#meta{name = maps:get(SubName == [] andalso ?IS_UNIT(SubType), #{true => '~', false => SubName})} || Meta = #meta{name = SubName, type = SubType} <- Explain],

    %% recursive
    SubCodes = parse_code_html_loop(SubExplain, Depth + 2, ets, SubKey, []),
    %% alignment padding
    Padding = lists:duplicate(Depth, "    "),
    %% format one field
    Type = "map",
    Code = lists:flatten(lists:concat([
        Padding, "<div class='field'>", "\n",
        Padding, "    ", "<div class='bar' style='left: ", (Depth - 4) * 16 - 4, "px;'></div>", "\n",
        Padding, "    ", "<div class='digest'>", "\n",
        Padding, "    ", "    ", "<div class='inner name'>", "\n",
        Padding, "    ", "    ", "    ", "<div class='pad' style='width: ", (Depth - 3) * 16, "px;position: relative;'>", "\n",
        Padding, "    ", "    ", "    ", "    ", "<div class='top' style='width: calc(" , 100 / (Depth - 3), "% - 4px);height: 50%; position: absolute;top: 0; right: 4px;'></div>", "\n",
        Padding, "    ", "    ", "    ", "    ", "<div class='bottom' style='width: calc(", (Depth - 3) * 16, "% - 4px);height: 50%; position: absolute;bottom: 0; right: 4px;'></div>", "\n",
        Padding, "    ", "    ", "    ", "</div>", "\n",
        Padding, "    ", "    ", "    ", "<div class='align' style='width: calc(100% - ", (Depth - 3) * 16, "px);'>", "\n",
        Padding, "    ", "    ", "    ", "    ", "<span onclick='copy(event, this)'>", word:to_lower_hump(Name), "</span><span> [</span>", "\n",
        Padding, "    ", "    ", "    ", "    ", "<div class='tips'>已复制</div>", "\n",
        Padding, "    ", "    ", "    ", "</div>", "\n",
        Padding, "    ", "    ", "</div>", "\n",
        Padding, "    ", "    ", "<div class='inner type'><span onclick='copy(event, this)'><span class='", Type, "'>", Type, "</span></span> / <span onclick='copy(event, this)'>键: <span class='", key, "'>", word:to_lower_hump(SubKey), "</span></span><div class='tips'>已复制</div></div>", "\n",
        Padding, "    ", "    ", "<div class='inner comment'><span onclick='copy(event, this)'>", Comment, "</span><div class='tips'>已复制</div></div>", "\n",
        Padding, "    ", "</div>", "\n",
        Padding, "    ", "<div class='child'>", "\n",
        Padding, "    ", "    ", "<div class='line' style='left: ", (Depth - 2) * 16 - 4, "px;'></div>", "\n",
        %% Padding, "    ", "    ", "<div class='explain'>", "\n",
        SubCodes, "\n",
        %% Padding, "    ", "    ", "</div>", "\n",
        Padding, "    ", "</div>", "\n",
        Padding, "    ", "<div class='digest'>", "\n",
        Padding, "    ", "    ", "<div class='inner name'>", "\n",
        Padding, "    ", "    ", "    ", "<div class='pad' style='width: ", (Depth - 3) * 16, "px;position: relative;'>", "\n",
        Padding, "    ", "    ", "    ", "    ", "<div class='top' style='width: calc(" , 100 / (Depth - 3), "% - 4px);height: 50%; position: absolute;top: 0; right: 4px;'></div>", "\n",
        Padding, "    ", "    ", "    ", "    ", "<div class='bottom' style='width: calc(", (Depth - 3) * 16, "% - 4px);height: 50%; position: absolute;bottom: 0; right: 4px;'></div>", "\n",
        Padding, "    ", "    ", "    ", "</div>", "\n",
        Padding, "    ", "    ", "    ", "<div class='align' style='width: calc(100% - ", (Depth - 3) * 16, "px);'>", "\n",
        Padding, "    ", "    ", "    ", "    ", "<span>]</span>", "\n",
        Padding, "    ", "    ", "    ", "    ", "<div class='tips'>已复制</div>", "\n",
        Padding, "    ", "    ", "    ", "</div>", "\n",
        Padding, "    ", "    ", "</div>", "\n",
        Padding, "    ", "</div>", "\n",
        Padding, "</div>"
    ])),
    parse_code_html_loop(T, Depth, Parent, Key, [Code | List]).
