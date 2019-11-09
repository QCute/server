%%%------------------------------------------------------------------
%%% @doc
%%% module router maker
%%% @end
%%%------------------------------------------------------------------
-module(router_maker).
-export([start/3]).
%%%==================================================================
%%% API functions
%%%==================================================================
start(Path, OutFile, IgnoreList) ->
    case file:list_dir(Path) of
        {ok, List} ->
            %% analyse protocol and name
            FileNames = [Name || Name <- List, filelib:is_dir(Path ++ Name) == false],
            Result = analyse(FileNames, Path, []),
            %% json protocol define
            write_json_code(Result),
            %% lua protocol define
            write_lua_code(Result),
            %% make read/write/route code
            {ReadCode, WriteCode, RouteCode} = make_code(Result, IgnoreList, [], [], []),
            %% replace old code with new code
            replace_code(OutFile, ReadCode, WriteCode, RouteCode);
        Error ->
            Error
    end.

%%%==================================================================
%%% Erl Router Part
%%%==================================================================
%% analyse file code
analyse([], _, List) ->
    lists:keysort(1, List);
analyse([File | T], Path,  List) ->
    {ok, Binary} = file:read_file(Path ++ File),
    %% extract name
    Name = filename:basename(File, ".erl") -- "protocol_script_",
    %% find name expression, force name assign
    {match, [String]} = re:run(Binary, "(?s)protocol\\(\\)\\s*->\\s*#protocol\\{.*?name\\s*=\\s*\\d+\\s*(?=,)", [{capture, first, list}]),
    %% extract name expression
    Expression = hd(lists:reverse(string:tokens(String, "\n"))),
    %% extract value expression
    Assignment = hd(lists:reverse(string:tokens(Expression, "="))),
    %% extract right value, revise when protocol name not assign
    Value = lists:sublist(string:strip(Assignment), 3),
    %% string to integer
    Integer = type:to_integer(Value),
    %% store it
    analyse(T, Path, [{Integer, Name} | List]).

%% make code
make_code([], _, ReadCode, WriteCode, RouteCode) ->
    %% AllReadCode = ReadCode ++ "read(_, Protocol, _) ->\n    {error, Protocol}.\n\n",
    %% AllWriteCode = WriteCode ++ "write(_, Protocol, _) ->\n    {error, Protocol}.\n\n",
    AllReadCode = ReadCode ++ "        _ ->\n            {error, Protocol}\n",
    AllWriteCode = WriteCode ++ "        _ ->\n            {error, Protocol}\n",
    AllRouteCode = RouteCode ++ "        _ ->\n            {error, protocol, Protocol}\n",
    {AllReadCode, AllWriteCode, AllRouteCode};
    
make_code([{Protocol, Name} | T], IgnoreList, ReadCode, WriteCode, RouteCode) ->
    %% Read = io_lib:format("read(~p, Protocol, Binary) ->~n    ~s_protocol:read(Protocol, Binary);~n", [Protocol, Name]),
    %% Write = io_lib:format("write(~p, Protocol, Binary) ->~n    ~s_protocol:write(Protocol, Binary);~n", [Protocol, Name]),
    Read = io_lib:format("        ~p ->~n            ~s_protocol:read(Protocol, Binary);~n", [Protocol, Name]),
    Write = io_lib:format("        ~p ->~n            ~s_protocol:write(Protocol, Binary);~n", [Protocol, Name]),
    %% except ignore list, for route code
    case lists:member(Name, IgnoreList) of
        true ->
            Route = io_lib:format("        ~p ->~n            ok;~n", [Protocol]);
        false ->
            %% store it
            Route = io_lib:format("        ~p ->~n            ~s_handler:handle(Protocol, User, Data);~n", [Protocol, Name])
    end,
    make_code(T, IgnoreList, ReadCode ++ Read, WriteCode ++ Write, RouteCode ++ Route).

%% replace code
replace_code(OutFile, ReadCode, WriteCode, RouteCode) ->
    {ok, Binary} = file:read_file(OutFile),
    %% read
    ReadData = "read(Protocol, Binary) ->\n    case Protocol div 100 of\n" ++ ReadCode ++ "    end.\n",
    ReplaceRead = re:replace(Binary, "(?m)(?s)(?<!\\S)(^read.+?)(?=\\.$|\\%)\\.\\n?", ReadData, [{return, binary}]),
    %% write
    WriteData = "write(Protocol, Binary) ->\n    case Protocol div 100 of\n" ++ WriteCode ++ "    end.\n",
    ReplaceWrite = re:replace(ReplaceRead, "(?m)(?s)(?<!\\S)(^write.+?)(?=\\.$|\\%)\\.\\n?", WriteData, [{return, binary}]),
    %% route
    RouteData = "dispatch(User, Protocol, Data) ->\n    case Protocol div 100 of\n" ++ RouteCode ++ "    end.\n",
    Data = re:replace(ReplaceWrite, "(?m)(?s)(?<!\\S)(^handle_routing.+?)(?=\\.$|\\%)\\.\\n?", RouteData, [{return, binary}]),
    %% write file data
    file:write_file(OutFile, Data).

%%%===================================================================
%%% Js Define Part
%%%===================================================================
%% write json protocol define function
write_json_code(List) ->
    Function = "function getProtocolDefine(type, protocol) {\n    switch (Math.trunc(protocol / 100)) {\n~s\n        default:throw(\"unknown protocol define: \"+ protocol)\n    }\n}",
    Code = string:join([io_lib:format("        case ~p: return ~sProtocol[type][protocol];", [Protocol, Name]) || {Protocol, Name} <- List], "\n"),
    file:write_file("script/make/protocol/json/ProtocolDefine.js", lists:flatten(io_lib:format(Function, [Code]))).

%%%===================================================================
%%% Lua Define Part
%%%===================================================================
%% write lua protocol define function
write_lua_code([{FirstProto, FirstName} | List]) ->
    Function = "function getProtocolDefine(type, protocol)\n    local code = math.floor(protocol / 100)\n~s\n    else\n        error(string.format(\"unknown protocol define: %d\", protocol))\n    end\nend",
    First = io_lib:format("    if code == ~p then\n        return ~sProtocol[type][protocol]", [FirstProto, FirstName]),
    Code = string:join([io_lib:format("    elseif code == ~p then\n        return ~sProtocol[type][protocol]", [Protocol, Name]) || {Protocol, Name} <- List], "\n"),
    file:write_file("script/make/protocol/lua/ProtocolDefine.lua", lists:flatten(io_lib:format(Function, [First ++ "\n" ++ Code]))).
