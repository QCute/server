%%%-------------------------------------------------------------------
%%% @doc
%%% module router maker
%%% @end
%%%-------------------------------------------------------------------
-module(router_maker).
-export([start/3]).
%%%===================================================================
%%% API functions
%%%===================================================================
start(Path, OutFile, IgnoreList) ->
    case file:list_dir(maker:relative_path(Path)) of
        {ok, List} ->
            %% analyse protocol and name
            FileNames = [Name || Name <- List, filelib:is_dir(Path ++ Name) == false],
            Result = analyse(FileNames, Path, []),
            %% js protocol define
            write_js_code(Result),
            %% lua protocol define
            write_lua_code(Result),
            %% make read/write/route code
            {ReadCode, WriteCode, RouteCode} = make_code(Result, IgnoreList, [], [], []),
            %% replace old code with new code
            replace_code(OutFile, ReadCode, WriteCode, RouteCode);
        Error ->
            Error
    end.

%%%===================================================================
%%% Erl Router Part
%%%===================================================================
%% analyse file code
analyse([], _, List) ->
    lists:keysort(1, List);
analyse([File | T], Path,  List) ->
    %% {ok, Binary} = file:read_file(maker:prim_script_path() ++ Path ++ File),
    %% extract name
    Name = lists:flatten(string:replace(filename:basename(File, ".erl"), "protocol_script_", "")),
    %% protocol
    {ok, Form} = epp:parse_file(maker:relative_path(Path ++ File), [], []),
    Values = [Value || {'function', _, protocol, 0, [{'clause', _, _, _, [{cons, _, {record, _, protocol, Fields}, _} | _]} | _]} <- Form, {record_field, _, {atom, _, number}, {integer, _, Value}} <- Fields],
    %% throw if protocol name not set or invalid
    (Values == [] orelse hd(Values) == 0) andalso erlang:throw("protocol name not found:" ++ File),
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
    analyse(T, Path, [{hd(Values), Name} | List]).

%% make code
make_code([], _, ReadCode, WriteCode, RouteCode) ->
    %% AllReadCode = ReadCode ++ "read(_, Protocol, _) ->\n    {error, Protocol}.\n\n",
    %% AllWriteCode = WriteCode ++ "write(_, Protocol, _) ->\n    {error, Protocol}.\n\n",
    AllReadCode = ReadCode ++ "        _ ->\n            {error, Protocol, Binary}\n",
    AllWriteCode = WriteCode ++ "        _ ->\n            {error, Protocol, Data}\n",
    AllRouteCode = RouteCode ++ "        _ ->\n            {error, protocol, Protocol}\n",
    {AllReadCode, AllWriteCode, AllRouteCode};
    
make_code([{Protocol, Name} | T], IgnoreList, ReadCode, WriteCode, RouteCode) ->
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
            Route = io_lib:format("        ~w ->~n            ~s_handler:handle(Protocol, User, Data);~n", [Protocol, Name])
    end,
    make_code(T, IgnoreList, ReadCode ++ Read, WriteCode ++ Write, RouteCode ++ Route).

%% replace code
replace_code(OutFile, ReadCode, WriteCode, RouteCode) ->
    {ok, Binary} = file:read_file(maker:relative_path(OutFile)),
    %% read
    ReadData = "read(Protocol, Binary) ->\n    case Protocol div 100 of\n" ++ ReadCode ++ "    end.\n",
    ReplaceRead = re:replace(Binary, "(?m)(?s)(?<!\\S)(^read.+?)(?=\\.$|\\%)\\.\\n?", ReadData, [{return, binary}]),
    %% write
    WriteData = "write(Protocol, Data) ->\n    case Protocol div 100 of\n" ++ WriteCode ++ "    end.\n",
    ReplaceWrite = re:replace(ReplaceRead, "(?m)(?s)(?<!\\S)(^write.+?)(?=\\.$|\\%)\\.\\n?", WriteData, [{return, binary}]),
    %% route
    RouteData = "dispatch(User, Protocol, Data) ->\n    case Protocol div 100 of\n" ++ RouteCode ++ "    end.\n",
    Data = re:replace(ReplaceWrite, "(?m)(?s)(?<!\\S)(^dispatch.+?)(?=\\.$|\\%)\\.\\n?", RouteData, [{return, binary}]),
    %% write file data
    file:write_file(maker:relative_path(OutFile), Data).

%%%====================================================================
%%% Js Define Part
%%%====================================================================
%% write js protocol define function
write_js_code(List) ->
    Function = "function getProtocolDefine(type, protocol) {\n    switch (Math.trunc(protocol / 100)) {\n~s\n        default:throw(\"unknown protocol define: \" + protocol)\n    }\n}",
    Code = string:join([io_lib:format("        case ~w: return ~sProtocol[type][protocol];", [Protocol, word:to_lower_hump(Name)]) || {Protocol, Name} <- List], "\n"),
    file:write_file(maker:relative_path("script/make/protocol/js/ProtocolDefine.js"), lists:flatten(io_lib:format(Function, [Code]))).

%%%====================================================================
%%% Lua Define Part
%%%====================================================================
%% write lua protocol define function
write_lua_code([{FirstProto, FirstName} | List]) ->
    Function = "function getProtocolDefine(type, protocol)\n    local code = math.floor(protocol / 100)\n~s\n    else\n        error(string.format(\"unknown protocol define: %d\", protocol))\n    end\nend",
    First = io_lib:format("    if code == ~w then\n        return ~sProtocol[type][protocol]", [FirstProto, word:to_lower_hump(FirstName)]),
    Code = string:join([io_lib:format("    elseif code == ~w then\n        return ~sProtocol[type][protocol]", [Protocol, word:to_lower_hump(Name)]) || {Protocol, Name} <- List], "\n"),
    file:write_file(maker:relative_path("script/make/protocol/lua/ProtocolDefine.lua"), lists:flatten(io_lib:format(Function, [First ++ "\n" ++ Code]))).
