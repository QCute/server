%%%-------------------------------------------------------------------
%%% @doc
%%% protocol viewer
%%% @end
%%%-------------------------------------------------------------------
-module(viewer).
%% API
-export([start/0, stop/0]).
-export([do/1]).
%% Includes
-include_lib("inets/include/httpd.hrl").
-include("journal.hrl").
%% Macros
-define(ICON_FUNNY, "data:image/png;base64,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").

%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc start
-spec start() -> {ok, pid()} | {error, term()}.
start() ->
    inets:start(),
    inets:start(httpd, [{ipfamily, inet}, {port, 32768}, {server_name, "fake.me"}, {server_root, "."}, {document_root, "."}, {modules, [?MODULE]}]).

%% @doc stop
-spec stop() -> ok.
stop() ->
    inets:start(),
    httpc:request(get, {lists:concat(["http://127.0.0.1:32768/stop", "?", "auth=ok"]), []}, [{timeout, 10000}, {connect_timeout, 10000}, {ssl, [{verify, verify_none}]}], [{full_result, false}]),
    ok.

%% @doc do esi callback
-spec do(Request :: #mod{}) -> done | {break, Data :: term()} | {proceed, Data :: term()}.
do(Request = #mod{request_uri = RequestUri}) ->
    try
        {Action, Parameter} = lists:split(string:str(RequestUri, "?"), RequestUri),
        Get = [list_to_tuple(string:tokens(X, "=")) || X <- string:tokens(Parameter, "&")],
        io:format("Action:~p~n", [Action]),
        io:format("Get:~p~n", [Get])
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?STACKTRACE(Class, Reason, ?GET_STACKTRACE(Stacktrace))
    end,
    io:format("Uri:~p~n", [RequestUri]),
    handle_request(Request, list_to_binary(RequestUri)).

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% index, erl root by default
handle_request(_Request, <<"/">>) ->
    {ok, List} = file:list_dir_all("script/make/protocol/"),
    Data = string:join([io_lib:format("<li onclick='view(this.innerText)'>~s</li>", [Name]) || Name <- List, Name =/= ".", Name =/= ".."], "\n"),
    Body = index(Data, "erl", ?ICON_FUNNY),
    {proceed, [{response, {response, [{code, 200}, {content_length, integer_to_list(length(Body))}], Body}}]};

%% stop
handle_request(_Request, <<"/stop", _/binary>>) ->
    Pid = spawn(fun() -> receive _ -> erlang:halt() end end),
    erlang:send_after(1000, Pid, stop),
    {proceed, [{response, {response, [{code, 200}, {content_length, integer_to_list(length(""))}], ""}}]};

%% index
handle_request(_Request, <<"/erl">>) ->
    {ok, List} = file:list_dir_all("script/make/protocol/"),
    Data = string:join([io_lib:format("<li onclick='view(this.innerText)'>~s</li>", [Name]) || Name <- List, Name =/= ".", Name =/= ".."], "\n"),
    Body = index(Data, "erl", ?ICON_FUNNY),
    {proceed, [{response, {response, [{code, 200}, {content_length, integer_to_list(length(Body))}], Body}}]};
handle_request(_Request, <<"/lua">>) ->
    {ok, List} = file:list_dir_all("script/make/protocol/lua"),
    Data = string:join([io_lib:format("<li onclick='view(this.innerText)'>~s</li>", [Name]) || Name <- List, Name =/= ".", Name =/= ".."], "\n"),
    Body = index(Data, "lua", ?ICON_FUNNY),
    {proceed, [{response, {response, [{code, 200}, {content_length, integer_to_list(length(Body))}], Body}}]};
handle_request(_Request, <<"/js">>) ->
    {ok, List} = file:list_dir_all("script/make/protocol/js"),
    Data = string:join([io_lib:format("<li onclick='view(this.innerText)'>~s</li>", [Name]) || Name <- List, Name =/= ".", Name =/= ".."], "\n"),
    Body = index(Data, "js", ?ICON_FUNNY),
    {proceed, [{response, {response, [{code, 200}, {content_length, integer_to_list(length(Body))}], Body}}]};

%% jump
handle_request(_Request, <<"/action?page=view&type=erl&file=erl", _/binary>>) ->
    {ok, List} = file:list_dir_all("script/make/protocol/"),
    Data = string:join([io_lib:format("<li onclick='view(this.innerText)'>~s</li>", [Name]) || Name <- List, Name =/= ".", Name =/= ".."], "\n"),
    Body = index(Data, "erl", ?ICON_FUNNY),
    {proceed, [{response, {response, [{code, 200}, {content_length, integer_to_list(length(Body))}], Body}}]};
handle_request(_Request, <<"/action?page=view&type=erl&file=lua", _/binary>>) ->
    {ok, List} = file:list_dir_all("script/make/protocol/lua"),
    Data = string:join([io_lib:format("<li onclick='view(this.innerText)'>~s</li>", [Name]) || Name <- List, Name =/= ".", Name =/= ".."], "\n"),
    Body = index(Data, "lua", ?ICON_FUNNY),
    {proceed, [{response, {response, [{code, 200}, {content_length, integer_to_list(length(Body))}], Body}}]};
handle_request(_Request, <<"/action?page=view&type=erl&file=js", _/binary>>) ->
    {ok, List} = file:list_dir_all("script/make/protocol/js"),
    Data = string:join([io_lib:format("<li onclick='view(this.innerText)'>~s</li>", [Name]) || Name <- List, Name =/= ".", Name =/= ".."], "\n"),
    Body = index(Data, "js", ?ICON_FUNNY),
    {proceed, [{response, {response, [{code, 200}, {content_length, integer_to_list(length(Body))}], Body}}]};

%% view
handle_request(_Request, <<"/action?page=view&type=erl&file=", File/binary>>) ->
    Body = view(binary_to_list(<<"script/make/protocol/", File/binary>>), "erl", ?ICON_FUNNY),
    {proceed, [{response, {response, [{code, 200}, {content_length, integer_to_list(length(Body))}], Body}}]};
handle_request(_Request, <<"/action?page=view&type=lua&file=", File/binary>>) ->
    Body = view(binary_to_list(<<"script/make/protocol/lua/", File/binary>>), "lua", ?ICON_FUNNY),
    {proceed, [{response, {response, [{code, 200}, {content_length, integer_to_list(length(Body))}], Body}}]};
handle_request(_Request, <<"/action?page=view&type=js&file=", File/binary>>) ->
    Body = view(binary_to_list(<<"script/make/protocol/js/", File/binary>>), "js", ?ICON_FUNNY),
    {proceed, [{response, {response, [{code, 200}, {content_length, integer_to_list(length(Body))}], Body}}]};

%% download
handle_request(_Request, <<"/action?page=download&type=erl&file=", FileName/binary>>) ->
    {ok, Binary} = max(file:read_file(<<"script/make/protocol/", FileName/binary>>), {ok, <<>>}),
    Body = binary_to_list(Binary),
    File = binary_to_list(FileName),
    {proceed, [{response, {response, [{code, 200}, {content_type, "application/octet-stream"}, {"Content-Disposition", io_lib:format("attachment;filename=~s", [File])}, {content_length, integer_to_list(byte_size(Binary))}], Body}}]};
handle_request(_Request, <<"/action?page=download&type=lua&file=", FileName/binary>>) ->
    {ok, Binary} = max(file:read_file(<<"script/make/protocol/lua/", FileName/binary>>), {ok, <<>>}),
    Body = binary_to_list(Binary),
    File = binary_to_list(FileName),
    {proceed, [{response, {response, [{code, 200}, {content_type, "application/octet-stream"}, {"Content-Disposition", io_lib:format("attachment;filename=~s", [File])}, {content_length, integer_to_list(byte_size(Binary))}], Body}}]};
handle_request(_Request, <<"/action?page=download&type=js&file=", FileName/binary>>) ->
    {ok, Binary} = max(file:read_file(<<"script/make/protocol/js/", FileName/binary>>), {ok, <<>>}),
    Body = binary_to_list(Binary),
    File = binary_to_list(FileName),
    {proceed, [{response, {response, [{code, 200}, {content_type, "application/octet-stream"}, {"Content-Disposition", io_lib:format("attachment;filename=~s", [File])}, {content_length, integer_to_list(byte_size(Binary))}], Body}}]};

%%handle_request(_, <<"/favicon.ico">>) ->
%%    {proceed, [{response, {response, [{code, 404}, {content_type, "application/octet-stream"}, {"Content-Disposition", "attachment;filename=favicon.ico"}, {content_length, "0"}], []}}]};

handle_request(#mod{data = Data}, _) ->
    {proceed, Data}.


%% index page
index(Data, Type, Icon) ->
    Format = "
<!DOCTYPE html>
<html>
    <head>
        <title>Viewer</title>
        <link rel='shortcut icon' type='image/x-icon' href='~s' />
        <meta http-equiv='Content-Type' content='text/html; charset=utf-8' />
        <style>li:hover{cursor: pointer;}</style>
        <script src='https://cdn.bootcss.com/jquery/3.4.1/jquery.min.js'></script>
        <script>function to(url) {location.href = url;}</script>
        <script>function view(url) { to('action?page=view&type=~s&file=' + url); }</script>
    </head>
    <body>
        <ul>
~s
        </ul>
    </body>
</html>
",
    lists:flatten(io_lib:format(Format, [Icon, Type, Data])).

%% view page
view(File, Type, Icon) ->
    Name = filename:basename(File),
    Ext = tl(filename:extension(File)),
    Format = "
<!DOCTYPE html>
<html>
    <head>
        <title>~s</title>
        <link rel='shortcut icon' type='image/x-icon' href='~s' />
        <meta http-equiv='Content-Type' content='text/html; charset=utf-8' />
        <style>body, pre{margin: 0; padding: 0;} .no-select{-moz-user-select: none; -ms-user-select: none; -webkit-user-select: none; user-select: none;}</style>
        <style>code{margin: 0; padding: 0; font-family: 'Consolas';}</style>
        <style>.button{cursor: pointer; font-size: 16px; font-family: 'Microsoft YaHei'; padding: 8px 16px; border-radius: 2px;}</style>
        <style>.copy{color: white; background-color: #30B786;} .download{color: white; background-color: #30B786;}</style>
        <style>.action{padding: 0.5em;}</style>
        <link rel='stylesheet' href='//cdnjs.cloudflare.com/ajax/libs/highlight.js/9.15.8/styles/monokai-sublime.min.css'>
        <script src='//cdnjs.cloudflare.com/ajax/libs/highlight.js/9.15.8/highlight.min.js'></script>
        <script src='//cdnjs.cloudflare.com/ajax/libs/highlight.js/9.15.8/languages/lua.min.js'></script>
        <script src='//cdnjs.cloudflare.com/ajax/libs/highlight.js/9.15.8/languages/javascript.min.js'></script>
        <script src='//cdnjs.cloudflare.com/ajax/libs/highlight.js/9.15.8/languages/erlang.min.js'></script>
        <script>hljs.initHighlightingOnLoad();</script>
        <script>function download() { location.href = 'action?page=download&type=~s&file=' + document.title; }</script>
        <script>function copy () {let content = document.getElementById('code').value; let text = document.createElement('textarea'); text.value = content; document.body.appendChild(text); text.select(); document.execCommand('Copy'); text.style.display = 'none'; document.body.removeChild(text); window.alert('Copied');}</script>
    </head>
    <body>

            <pre><code class='~s'>
~s
            </code>
        </pre>
        <textarea id='code' hidden='hidden'>~s</textarea>
        <div class='action'><span> </span><span class='button copy no-select' onclick='copy()'>Copy</span><span> </span><span class='button download no-select' onclick='download()'>Download</span></div>
        <br/><br/><br/>
    </body>
</html>
",
    %% custom style can generate from this address
    %% https://highlightjs.org/static/demo/
    {ok, Binary} = max(file:read_file(File), {ok, <<>>}),
    lists:flatten(io_lib:format(Format, [Name, Icon, Type, Ext, Binary, Binary])).


