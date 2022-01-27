%%%-------------------------------------------------------------------
%%% @doc
%%% make protocol define to router code
%%% @end
%%%-------------------------------------------------------------------
-module(router_maker).
-export([start/6]).
%%%===================================================================
%%% API functions
%%%===================================================================
start(Path, OutFile, HeaderFile, JsFile, LuaFile, IgnoreList) ->
    case file:list_dir(maker:relative_path(Path)) of
        {ok, List} ->
            %% analyse protocol and name
            FileNames = [Name || Name <- List, filelib:is_dir(Path ++ Name) == false],
            Result = analyse(FileNames, Path, []),
            %% js protocol define
            write_js_code(JsFile, Result),
            %% lua protocol define
            write_lua_code(LuaFile, Result),
            %% protocol define
            write_header_code(Result, HeaderFile),
            %% make read/write/route code
            {ReadCode, WriteCode, RouteCode} = make_code(Result, IgnoreList, [], [], []),
            %% replace old code with new code
            replace_code(OutFile, Result, ReadCode, WriteCode, RouteCode);
        Error ->
            Error
    end.

%%%===================================================================
%%% Erl Router Part
%%%===================================================================
%% analyse file code
analyse([], _, List) ->
    lists:keysort(1, List);
analyse([File | T], Path, List) ->
    %% {ok, Binary} = file:read_file(maker:prim_script_path() ++ Path ++ File),
    %% extract name
    Name = lists:flatten(string:replace(filename:basename(File, ".erl"), "protocol_script_", "")),
    %% protocol
    {ok, Form} = epp:parse_file(maker:relative_path(Path ++ File), [], []),
    Values = [Value || {function, _, protocol, 0, [{clause, _, _, _, [{record, _, protocol, Fields} | _]} | _]} <- Form, {record_field, _, {atom, _, number}, {integer, _, Value}} <- Fields],
    %% error if protocol number not set or invalid
    (Values == [] orelse hd(Values) == 0) andalso erlang:throw(lists:flatten(io_lib:format("protocol number not found: ~s", [File]))),
    %% find name expression, force name assign
    %% {match, [String]} = re:run(Binary, "(?s)protocol\\(\\)\\s*->\\s*#protocol\\{.*?name\\s*=\\s*\\d+\\s*(?=,)", [{capture, first, list}]),
    %% extract name expression
    %% Expression = hd(lists:reverse(string:tokens(String, "\n"))),
    %% extract value expression
    %% Assignment = hd(lists:reverse(string:tokens(Expression, "="))),
    %% extract right value, revise when protocol name not assign
    %% Value = lists:sublist(string:strip(Assignment), 3),
    %% string to integer
    %% Integer = list_to_integer(Value),
    %% store it
    IoForm = [Cons || {function, _, protocol, 0, [{clause, _, _, _, [{record, _, protocol, Fields} | _]} | _]} <- Form, {record_field, _, {atom, _, io}, Cons} <- Fields],
    IoNames = make_io_name(hd(IoForm), []),
    analyse(T, Path, [{hd(Values), IoNames, Name} | List]).

%% make io name
make_io_name({nil, _}, List) ->
    lists:reverse(List);
make_io_name({cons, _, {record, _, io, Fields}, Cons}, List) ->
    Protocol = hd([Protocol || {record_field, _, {atom, _, protocol}, {integer, _, Protocol}} <- Fields]),
    %% time expr
    Interval = [parser:evaluate(erl_prettypr:format(erl_syntax:form_list([Expr]))) || {record_field, _, {atom, _, interval}, Expr} <- Fields],
    %% Value = tool:default(lists:append([Value || {record_field, _, {atom, _, alias}, {_, _, Value}} <- Fields]), undefined),
    %% handler function name
    %% Function = hd(tool:default([Name || {record_field, _, {atom, _, handler}, {record, _, handler, HandlerFields}} <- Fields, {record_field, _, {atom, _, function}, {atom, _, Name}} <- HandlerFields], [undefined])),
    Function = case [Name || {record_field, _, {atom, _, handler}, {record, _, handler, HandlerFields}} <- Fields, {record_field, _, {atom, _, function}, {atom, _, Name}} <- HandlerFields] of [] -> undefined; [ThisFunction | _] -> ThisFunction end,
    %% handle alias name
    %% Alias = hd(tool:default([Name || {record_field, _, {atom, _, handler}, {record, _, handler, HandlerFields}} <- Fields, {record_field, _, {atom, _, alias}, {_, _, Name}} <- HandlerFields], [Function])),
    Alias = case [Name || {record_field, _, {atom, _, handler}, {record, _, handler, HandlerFields}} <- Fields, {record_field, _, {atom, _, alias}, {_, _, Name}} <- HandlerFields] of [] -> Function; [ThisAlias | _] -> ThisAlias end,
    %% handler module name
    %% Module = tool:default(lists:append([Name || {record_field, _, {atom, _, handler}, {record, _, handler, HandlerFields}} <- Fields, {record_field, _, {atom, _, module}, {atom, _, Name}} <- HandlerFields]), undefined),
    NewList = [{Protocol, Interval, proplists:get_value(Alias, [{true, Function}, {undefined, Function}, {[], undefined}, {false, undefined}], Alias)} | List],
    make_io_name(Cons, NewList).

%% make code
make_code([], _, ReadCode, WriteCode, RouteCode) ->
    %% AllReadCode = ReadCode ++ "read(_, Protocol, _) ->\n    {error, Protocol}.\n\n",
    %% AllWriteCode = WriteCode ++ "write(_, Protocol, _) ->\n    {error, Protocol}.\n\n",
    AllReadCode = ReadCode ++ "        _ ->\n            {error, Protocol, Binary}\n",
    AllWriteCode = WriteCode ++ "        _ ->\n            {error, Protocol, Data}\n",
    AllRouteCode = RouteCode ++ "        _ ->\n            {error, Protocol, Data}\n",
    {AllReadCode, AllWriteCode, AllRouteCode};

make_code([{Protocol, _, Name} | T], IgnoreList, ReadCode, WriteCode, RouteCode) ->
    %% Read = io_lib:format("read(~w, Protocol, Binary) ->~n    ~s_protocol:read(Protocol, Binary);~n", [Protocol, Name]),
    %% Write = io_lib:format("write(~w, Protocol, Binary) ->~n    ~s_protocol:write(Protocol, Binary);~n", [Protocol, Name]),
    Read = io_lib:format("        ~w ->~n            ~s_protocol:read(Protocol, Binary);~n", [Protocol, Name]),
    Write = io_lib:format("        ~w ->~n            ~s_protocol:write(Protocol, Data);~n", [Protocol, Name]),
    %% except ignore list, for route code
    case lists:member(Name, IgnoreList) of
        true ->
            Route = io_lib:format("        ~w ->~n            ok;~n", [Protocol]);
        false ->
            %% store it
            Route = io_lib:format("        ~w ->~n            ~s_handler:handle(User, Protocol, Data);~n", [Protocol, Name])
    end,
    make_code(T, IgnoreList, ReadCode ++ Read, WriteCode ++ Write, RouteCode ++ Route).

%% replace code
replace_code(OutFile, Result, ReadCode, WriteCode, RouteCode) ->
    {ok, Binary} = file:read_file(maker:relative_path(OutFile)),
    %% read
    ReadData = "read(Protocol, Binary) ->\n    case Protocol div 100 of\n" ++ ReadCode ++ "    end.\n",
    ReplaceRead = re:replace(Binary, "(?m)(?s)(?<!\\S)(^read.+?)(?=\\.$|\\%)\\.\\n?", ReadData, [{return, binary}]),
    %% write
    WriteData = "write(Protocol, Data) ->\n    case Protocol div 100 of\n" ++ WriteCode ++ "    end.\n",
    ReplaceWrite = re:replace(ReplaceRead, "(?m)(?s)(?<!\\S)(^write.+?)(?=\\.$|\\%)\\.\\n?", WriteData, [{return, binary}]),
    %% route
    RouteData = "dispatch(User, Protocol, Data) ->\n    case Protocol div 100 of\n" ++ RouteCode ++ "    end.\n",
    ReplaceRoute = re:replace(ReplaceWrite, "(?m)(?s)(?<!\\S)(^dispatch.+?)(?=\\.$|\\%)\\.\\n?", RouteData, [{return, binary}]),
    %% interval
    IntervalData = format_interval_code(Result),
    ReplaceInterval = re:replace(ReplaceRoute, "(?m)(?s)(?<!\\S)(^interval.+?)(?=\\.$|\\%)\\.\\n?", IntervalData, [{return, binary}]),
    %% interval record
    IntervalRecord = io_lib:format("-record(protocol_interval, {~s}).", [string:join([lists:concat(["'", Protocol, "' = 0"]) || {Protocol, [_], _} <- lists:append([List || {_, List, _} <- Result])], ", ")]),
    Data = re:replace(ReplaceInterval, "(?m)(?s)(?<!\\S)(-record\\s*\\(\\s*protocol_interval\\s*,.+?)(?=\\.$|\\.\\%)\\.", IntervalRecord, [{return, binary}]),
    %% write file data
    file:write_file(maker:relative_path(OutFile), Data).

format_interval_code(List) ->
    Interval = [format_interval_code(Protocol, Interval) || {Protocol, [Interval], _} <- lists:append([NameList || {_, NameList, _} <- List])],
    lists:concat([string:join(Interval, ""), "interval(State = #client{protocol_interval = undefined}, _) ->\n    {true, State#client{protocol_interval = #protocol_interval{}}};\ninterval(State, _) ->\n    {true, State}.\n"]).

format_interval_code(Protocol, Interval) ->
    io_lib:format("interval(State = #client{protocol_interval = ProtocolInterval = #protocol_interval{'~w' = Before}}, ~w) ->
    Now = time:millisecond(),
    case Before + ~w =< Now of
        true ->
            {true, State#client{protocol_interval = ProtocolInterval#protocol_interval{'~w' = Now}}};
        false ->
            {false, State}
    end;~n", [Protocol, Protocol, Interval, Protocol]).

%% write io name header code
write_header_code(List, HeaderFile) ->
    Code = string:join([Code || Code <- [write_header_code_loop(lists:reverse(NameList), Name, []) || {_, NameList, Name} <- List], Code =/= []], "\n") ++ "\n\n",
    file:write_file(HeaderFile, Code).

write_header_code_loop([], _, []) ->
    [];
write_header_code_loop([], Name, List) ->
    string:join([io_lib:format("
%%%===================================================================
%%% ~s
%%%===================================================================
", [Name]) | List], "\n");
write_header_code_loop([{_, _, undefined} | T], Name, List) ->
    write_header_code_loop(T, Name, List);
write_header_code_loop([{NameProtocol, _, NameValue} | T], Name, List) ->
    Code = io_lib:format("-define(PROTOCOL_~s_~s,~s~w).", [string:to_upper(Name), string:to_upper(type:to_list(NameValue)), lists:duplicate(35 - length(Name ++ type:to_list(NameValue)), " "), NameProtocol]),
    write_header_code_loop(T, Name, [Code | List]).

%%%====================================================================
%%% Js Define Part
%%%====================================================================
%% write js protocol define function
write_js_code(JsFile, List) ->
    Function = "function getProtocolDefine(protocol, type) {\n    switch (Math.trunc(protocol / 100)) {\n~s\n        default:throw(\"unknown protocol define: \" + protocol)\n    }\n}",
    Code = string:join([io_lib:format("        case ~w: return ~sProtocol[protocol][type];", [Protocol, word:to_lower_hump(Name)]) || {Protocol, _, Name} <- List], "\n"),
    file:write_file(maker:relative_path(JsFile), lists:flatten(io_lib:format(Function, [Code]))).

%%%====================================================================
%%% Lua Define Part
%%%====================================================================
%% write lua protocol define function
write_lua_code(LuaFile, [{FirstProto, _, FirstName} | List]) ->
    Function = "function getProtocolDefine(protocol, type)\n    local code = math.floor(protocol / 100)\n~s\n    else\n        error(string.format(\"unknown protocol define: %d\", protocol))\n    end\nend",
    First = io_lib:format("    if code == ~w then\n        return ~sProtocol[protocol][type]", [FirstProto, word:to_lower_hump(FirstName)]),
    Code = string:join([io_lib:format("    elseif code == ~w then\n        return ~sProtocol[protocol][type]", [Protocol, word:to_lower_hump(Name)]) || {Protocol, _, Name} <- List], "\n"),
    file:write_file(maker:relative_path(LuaFile), lists:flatten(io_lib:format(Function, [First ++ "\n" ++ Code]))).
