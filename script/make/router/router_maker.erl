%%%-------------------------------------------------------------------
%%% @doc
%%% make protocol define to router code
%%% @end
%%%-------------------------------------------------------------------
-module(router_maker).
-export([start/1]).
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc for shell
start(List) ->
    maker:start(fun parse_file/1, List).

%%%===================================================================
%%% Erl Router Part
%%%===================================================================
parse_file(Item = #{path := Path, lang := erl, type := router}) ->
    IgnoreList = maps:get(ignore, Item, []),
    FileList = filelib:wildcard(maker:relative_path(Path) ++ "/*script*"),
    %% analyse protocol and name
    Result = analyse(FileList, Path, []),
    %% make read/write/route code
    {ReadCode, WriteCode, RouteCode} = make_code(Result, IgnoreList, [], [], []),
    %% replace old code with new code
    make_replace_pattern(Result, ReadCode, WriteCode, RouteCode);
parse_file(#{path := Path, lang := erl, type := define}) ->
    FileList = filelib:wildcard(maker:relative_path(Path) ++ "/*script*"),
    %% analyse protocol and name
    Result = analyse(FileList, Path, []),
    %% protocol define
    make_header_pattern(Result);
parse_file(#{path := Path, lang := lua, type := router}) ->
    FileList = filelib:wildcard(maker:relative_path(Path) ++ "/*script*"),
    %% analyse protocol and name
    Result = analyse(FileList, Path, []),
    %% lua protocol router
    make_lua_pattern(Result);
parse_file(#{path := Path, lang := lua, type := define}) ->
    FileList = filelib:wildcard(maker:relative_path(Path) ++ "/*script*"),
    %% analyse protocol and name
    Result = analyse(FileList, Path, []),
    %% lua protocol define
    make_lua_meta_pattern(Result);
parse_file(#{path := Path, lang := js, type := router}) ->
    FileList = filelib:wildcard(maker:relative_path(Path) ++ "/*script*"),
    %% analyse protocol and name
    Result = analyse(FileList, Path, []),
    %% js protocol router
    make_js_pattern(Result);
parse_file(#{path := Path, lang := js, type := define}) ->
    FileList = filelib:wildcard(maker:relative_path(Path) ++ "/*script*"),
    %% analyse protocol and name
    Result = analyse(FileList, Path, []),
    %% js protocol define
    make_js_meta_pattern(Result);
parse_file(#{path := Path, lang := cs, type := router}) ->
    FileList = filelib:wildcard(maker:relative_path(Path) ++ "/*script*"),
    %% analyse protocol and name
    Result = analyse(FileList, Path, []),
    %% cs protocol router
    make_cs_pattern(Result);
parse_file(#{path := Path, lang := cs, type := define}) ->
    FileList = filelib:wildcard(maker:relative_path(Path) ++ "/*script*"),
    %% analyse protocol and name
    Result = analyse(FileList, Path, []),
    %% cs protocol define
    make_cs_meta_pattern(Result);
parse_file(#{path := Path, lang := html, type := define}) ->
    FileList = filelib:wildcard(maker:relative_path(Path) ++ "/*script*"),
    %% analyse protocol and name
    Result = analyse(FileList, Path, []),
    %% protocol define
    make_html_pattern(Result).

%% analyse file code
analyse([], _, List) ->
    lists:keysort(1, List);
analyse([File | T], Path, List) ->
    %% extract name
    Name = lists:flatten(string:replace(filename:basename(File, ".erl"), "protocol_script_", "")),
    %% protocol
    {ok, Form} = epp:parse_file(File, [], []),
    Numbers = [Value || {function, _, protocol, 0, [{clause, _, _, _, [{record, _, protocol, Fields} | _]} | _]} <- Form, {record_field, _, {atom, _, number}, {integer, _, Value}} <- Fields],
    Comments = [Value || {function, _, protocol, 0, [{clause, _, _, _, [{record, _, protocol, Fields} | _]} | _]} <- Form, {record_field, _, {atom, _, comment}, {_, _, Value}} <- Fields],
    %% error if protocol number not set or invalid
    (Numbers == [] orelse hd(Numbers) == 0) andalso erlang:throw(lists:flatten(io_lib:format("protocol number not found: ~s", [File]))),
    (Comments == [] orelse hd(Comments) == 0) andalso erlang:throw(lists:flatten(io_lib:format("comment not found: ~s", [File]))),
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
    analyse(T, Path, [{hd(Numbers), hd(Comments), IoNames, Name} | List]).

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

make_code([{Protocol, _, _, Name} | T], IgnoreList, ReadCode, WriteCode, RouteCode) ->
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
make_replace_pattern(Result, ReadCode, WriteCode, RouteCode) ->
    %% interval record
    IntervalRecordPattern = "(?m)(?s)(?<!\\S)(-record\\s*\\(\\s*protocol_interval\\s*,.+?)(?=\\.$|\\.\\%)\\.",
    IntervalRecordCode = io_lib:format("-record(protocol_interval, {~s}).", [string:join([lists:concat(["'", Protocol, "' = 0"]) || {Protocol, [_], _} <- lists:append([List || {_, _, List, _} <- Result])], ", ")]),
    %% interval
    IntervalPattern = "(?m)(?s)(?<!\\S)(^interval.+?)(?=\\.$|\\%)\\.\\n?",
    IntervalCode = format_interval_code(Result),
    %% read
    ReadPattern = "(?m)(?s)(?<!\\S)(^read.+?)(?=\\.$|\\%)\\.\\n?",
    ReadReplaceCode = "read(Protocol, Binary) ->\n    case Protocol div 100 of\n" ++ ReadCode ++ "    end.\n",
    %% write
    WritePattern = "(?m)(?s)(?<!\\S)(^write.+?)(?=\\.$|\\%)\\.\\n?",
    WriteReplaceCode = "write(Protocol, Data) ->\n    case Protocol div 100 of\n" ++ WriteCode ++ "    end.\n",
    %% route
    RoutePattern = "(?m)(?s)(?<!\\S)(^dispatch.+?)(?=\\.$|\\%)\\.\\n?",
    RouteReplaceCode = "dispatch(User, Protocol, Data) ->\n    case Protocol div 100 of\n" ++ RouteCode ++ "    end.\n",
    %% pattern and code
    [#{pattern => ReadPattern, code => ReadReplaceCode}, #{pattern => WritePattern, code => WriteReplaceCode}, #{pattern => RoutePattern, code => RouteReplaceCode}, #{pattern => IntervalPattern, code => IntervalCode}, #{pattern => IntervalRecordPattern, code => IntervalRecordCode}].

format_interval_code(List) ->
    Interval = [format_interval_code(Protocol, Interval) || {Protocol, [Interval], _} <- lists:append([NameList || {_, _, NameList, _} <- List])],
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
make_header_pattern(List) ->
    Code = string:join([Code || Code <- [format_header_code_loop(lists:reverse(NameList), Name, []) || {_, _, NameList, Name} <- List], Code =/= []], "\n") ++ "\n\n",
    [#{pattern => "(?s).*", code => Code}].

format_header_code_loop([], _, []) ->
    [];
format_header_code_loop([], Name, List) ->
    string:join([io_lib:format("
%%%===================================================================
%%% ~s
%%%===================================================================
", [Name]) | List], "\n");
format_header_code_loop([{_, _, undefined} | T], Name, List) ->
    format_header_code_loop(T, Name, List);
format_header_code_loop([{NameProtocol, _, NameValue} | T], Name, List) ->
    Code = io_lib:format("-define(PROTOCOL_~s_~s,~s~w).", [string:to_upper(Name), string:to_upper(type:to_list(NameValue)), lists:duplicate(35 - length(Name ++ type:to_list(NameValue)), " "), NameProtocol]),
    format_header_code_loop(T, Name, [Code | List]).

%%%====================================================================
%%% Lua Define Part
%%%====================================================================
%% write lua protocol function
make_lua_pattern(List) ->
    RequireCode = string:join([io_lib:format("require(\"./~sProtocol\")", [word:to_hump(Name)]) || {_, _, _, Name} <- List], "\n"),
    EncodeCase = string:join([lists:concat(["    ", "    ", "[", Protocol, "] = encode", word:to_hump(Name), "Protocol,"]) || {Protocol, _, _, Name} <- List], "\n"),
    EncodeFunction = [
        "function encodeProtocol(offset, protocol, data) ", "\n",
        "    ", "local switch = {", "\n",
        EncodeCase, "\n",
        "    ", "}", "\n",
        "    ", "local method = switch[math.floor(protocol / 100)]", "\n",
        "    ", "if method then", "\n",
        "    ", "    ", "return method(offset, protocol, data)", "\n",
        "    ", "else", "\n",
        "    ", "    ", "error(string.format(\"unknown protocol define: %d\", protocol))", "\n",
        "    ", "end", "\n",
        "end"
    ],
    DecodeCase = string:join([lists:concat(["    ", "    ", "[", Protocol, "] = decode", word:to_hump(Name), "Protocol,"]) || {Protocol, _, _, Name} <- List], "\n"),
    DecodeFunction = [
        "function decodeProtocol(offset, protocol, data) ", "\n",
        "    ", "local switch = {", "\n",
        DecodeCase, "\n",
        "    ", "}", "\n",
        "    ", "local method = switch[math.floor(protocol / 100)]", "\n",
        "    ", "if method then", "\n",
        "    ", "    ", "return method(offset, protocol, data)", "\n",
        "    ", "else", "\n",
        "    ", "    ", "error(string.format(\"unknown protocol define: %d\", protocol))", "\n",
        "    ", "end", "\n",
        "end"
    ],
    Code = lists:flatten([RequireCode, "\n\n", EncodeFunction, "\n\n", DecodeFunction]),
    [#{pattern => "(?s).*", code => Code}].

%% write lua protocol define function
make_lua_meta_pattern(List) ->
    RequireCode = string:join([io_lib:format("require(\"./~sProtocol\")", [word:to_hump(Name)]) || {_, _, _, Name} <- List], "\n"),
    ReadCase = string:join([lists:concat(["    ", "    ", "[", Protocol, "] = ", word:to_lower_hump(Name), "Protocol"]) || {Protocol, _, _, Name} <- List], ",\n"),
    ReadFunction = ["function getReadProtocolDefine(protocol, type)", "\n",
        "    ", "local switch = {", "\n",
        ReadCase, "\n",
        "    ", "}", "\n",
        "    ", "local data = switch[math.floor(protocol / 100)]", "\n",
        "    ", "if data then", "\n",
        "    ", "    ", "return data[protocol].read", "\n",
        "    ", "else", "\n",
        "    ", "    ", "error(string.format(\"unknown protocol define: %d\", protocol))", "\n",
        "    ", "end", "\n",
        "end"
    ],
    WriteCase = string:join([lists:concat(["    ", "    ", "[", Protocol, "] = ", word:to_lower_hump(Name), "Protocol"]) || {Protocol, _, _, Name} <- List], ",\n"),
    WriteFunction = ["function getWriteProtocolDefine(protocol, type)", "\n",
        "    ", "local switch = {", "\n",
        WriteCase, "\n",
        "    ", "}", "\n",
        "    ", "local data = switch[math.floor(protocol / 100)]", "\n",
        "    ", "if data then", "\n",
        "    ", "    ", "return data[protocol].write", "\n",
        "    ", "else", "\n",
        "    ", "    ", "error(string.format(\"unknown protocol define: %d\", protocol))", "\n",
        "    ", "end", "\n",
        "end"
    ],
    Code = lists:flatten([RequireCode, "\n\n", ReadFunction, "\n\n", WriteFunction]),
    [#{pattern => "(?s).*", code => Code}].

%%%====================================================================
%%% Js Define Part
%%%====================================================================
%% write js protocol function
make_js_pattern(List) ->
    ImportCode = string:join([io_lib:format("import { encode~sProtocol, decode~sProtocol } from \"./~sProtocol.js\";", [word:to_hump(Name), word:to_hump(Name), word:to_hump(Name)]) || {_, _, _, Name} <- List], "\n"),
    EncodeCase = string:join([lists:concat(["    ", "    ", "case ", Protocol, ": return encode", word:to_hump(Name), "Protocol(textEncoder, view, offset, protocol, data);"]) || {Protocol, _, _, Name} <- List], "\n"),
    EncodeFunction = [
        "export function encodeProtocol(textEncoder, view, offset, protocol, data) {", "\n",
        "    ", "switch (Math.trunc(protocol / 100)) {", "\n",
        EncodeCase, "\n",
        "    ", "    ", "default:throw(\"unknown protocol define: \" + protocol)", "\n",
        "    ", "}", "\n",
        "}"
    ],
    DecodeCase = string:join([lists:concat(["    ", "    ", "case ", Protocol, ": return decode", word:to_hump(Name), "Protocol(textDecoder, view, offset, protocol);"]) || {Protocol, _, _, Name} <- List], "\n"),
    DecodeFunction = [
        "export function decodeProtocol(textDecoder, view, offset, protocol) {", "\n",
        "    ", "switch (Math.trunc(protocol / 100)) {", "\n",
        DecodeCase, "\n",
        "    ", "    ", "default:throw(\"unknown protocol define: \" + protocol)", "\n",
        "    ", "}", "\n",
        "}"
    ],
    Code = lists:flatten([ImportCode, "\n\n", EncodeFunction, "\n\n", DecodeFunction]),
    [#{pattern => "(?s).*", code => Code}].

%% write js protocol define function
make_js_meta_pattern(List) ->
    ImportCode = string:join([io_lib:format("import ~sProtocol from \"./~sProtocol.js\";", [word:to_lower_hump(Name), word:to_hump(Name)]) || {_, _, _, Name} <- List], "\n"),
    ReadCase = string:join([lists:concat(["    ", "    ", "case ", Protocol, ": return ", word:to_lower_hump(Name), "Protocol[protocol].read;"]) || {Protocol, _, _, Name} <- List], "\n"),
    ReadFunction = [
        "export function getReadProtocolDefine(protocol) {", "\n",
        "    ", "switch (Math.trunc(protocol / 100)) {", "\n",
        ReadCase, "\n",
        "    ", "    ", "default:throw(\"unknown protocol define: \" + protocol)", "\n",
        "    ", "}", "\n",
        "}"
    ],
    WriteCase = string:join([lists:concat(["    ", "    ", "case ", Protocol, ": return ", word:to_lower_hump(Name), "Protocol[protocol].write;"]) || {Protocol, _, _, Name} <- List], "\n"),
    WriteFunction = [
        "export function getWriteProtocolDefine(protocol) {", "\n",
        "    ", "switch (Math.trunc(protocol / 100)) {", "\n",
        WriteCase, "\n",
        "    ", "    ", "default:throw(\"unknown protocol define: \" + protocol)", "\n",
        "    ", "}", "\n",
        "}"
    ],
    Code = lists:flatten([ImportCode, "\n\n", ReadFunction, "\n\n", WriteFunction]),
    [#{pattern => "(?s).*", code => Code}].

%%%====================================================================
%%% Cs Define Part
%%%====================================================================
%% write cs protocol function
make_cs_pattern(List) ->
    EncodeCase = string:join([lists:concat(["    ", "    ", "    ", "case ", Protocol, ": ", word:to_hump(Name), "Protocol.Encode(encoding, writer, protocol, data);break;"]) || {Protocol, _, _, Name} <- List], "\n"),
    EncodeFunction = [
        "    ", "public static void Encode(System.Text.Encoding encoding, System.IO.BinaryWriter writer, System.UInt16 protocol, System.Collections.Generic.Dictionary<System.String, System.Object> data)", "\n",
        "    ", "{", "\n",
        "    ", "    ", "switch (protocol / 100)", "\n",
        "    ", "    ", "{", "\n",
        EncodeCase, "\n",
        "    ", "    ", "    ", "default:throw new System.ArgumentException(System.String.Format(\"unknown protocol define: {0}\", protocol));", "\n",
        "    ", "    ", "}", "\n",
        "    ", "}"
    ],
    DecodeCase = string:join([lists:concat(["    ", "    ", "    ", "case ", Protocol, ": return ", word:to_hump(Name), "Protocol.Decode(encoding, reader, protocol);"]) || {Protocol, _, _, Name} <- List], "\n"),
    DecodeFunction = [
        "    ", "public static System.Collections.Generic.Dictionary<System.String, System.Object> Decode(System.Text.Encoding encoding, System.IO.BinaryReader reader, System.UInt16 protocol)", "\n",
        "    ", "{", "\n",
        "    ", "    ", "switch (protocol / 100)", "\n",
        "    ", "    ", "{", "\n",
        DecodeCase, "\n",
        "    ", "    ", "    ", "default:throw new System.ArgumentException(System.String.Format(\"unknown protocol define: {0}\", protocol));", "\n",
        "    ", "    ", "}", "\n",
        "    ", "}"
    ],
    Code = lists:flatten([
        "class ProtocolRouter", "\n",
        "{", "\n",
        EncodeFunction, "\n\n",
        DecodeFunction, "\n",
        "}"
    ]),
    Cast = "public static class Cast
{
    public static System.Byte ToUInt8(this object data)
    {
        return (System.Byte)data;
    }
    public static System.UInt16 ToUInt16(this object data)
    {
        return (System.UInt16)data;
    }
    public static System.UInt32 ToUInt32(this object data)
    {
        return (System.UInt32)data;
    }
    public static System.UInt64 ToUInt64(this object data)
    {
        return (System.UInt64)data;
    }

    public static System.SByte ToInt8(this object data)
    {
        return (System.SByte)data;
    }
    public static System.Int16 ToInt16(this object data)
    {
        return (System.Int16)data;
    }
    public static System.Int32 ToInt32(this object data)
    {
        return (System.Int32)data;
    }
    public static System.Int64 ToInt64(this object data)
    {
        return (System.Int64)data;
    }

    public static System.Single ToFloat32(this object data)
    {
        return (System.Single)data;
    }
    public static System.Double ToFloat64(this object data)
    {
        return (System.Double)data;
    }

    public static System.Boolean ToBoolean(this object data)
    {
        return (System.Boolean)data;
    }
    public static System.Byte[] ToBinary(this object data)
    {
        return (System.Byte[])data;
    }

    public static System.String ToString(this object data)
    {
        return (System.String)data;
    }

    public static System.Collections.Generic.Dictionary<System.String, System.Object> ToMap(this object data)
    {
        return (System.Collections.Generic.Dictionary<System.String, System.Object>)data;
    }

    public static System.Collections.ArrayList ToList(this object data)
    {
        return (System.Collections.ArrayList)data;
    }
}",
    [#{pattern => "(?s).*", code => lists:concat([Code, "\n", "\n", Cast])}].

%% write cs protocol define function
make_cs_meta_pattern(List) ->
    Case = string:join([lists:concat(["    ", "    ", "    ", "case ", Protocol, ": return (List)(((Map)", word:to_hump(Name), "Protocol.GetMeta()[protocol.ToString()])[type]);"]) || {Protocol, _, _, Name} <- List], "\n"),
    Function = [
        "    ", "public static List Get(System.UInt16 protocol, System.String type)", "\n",
        "    ", "{", "\n",
        "    ", "    ", "switch (protocol / 100)", "\n",
        "    ", "    ", "{", "\n",
        Case, "\n",
        "    ", "    ", "    ", "default:throw new System.ArgumentException(System.String.Format(\"unknown protocol define: {0}\", protocol));", "\n",
        "    ", "    ", "}", "\n",
        "    ", "}"
    ],
    Code = lists:flatten([
        "using List = System.Collections.ArrayList;", "\n",
        "using Map = System.Collections.Generic.Dictionary<System.String, System.Object>;", "\n",
        "\n",
        "class ProtocolDefine", "\n",
        "{", "\n",
        Function, "\n",
        "}"
    ]),
    Cast = "public static class Cast
{
    public static System.Byte ToUInt8(this object data)
    {
        return (System.Byte)data;
    }
    public static System.UInt16 ToUInt16(this object data)
    {
        return (System.UInt16)data;
    }
    public static System.UInt32 ToUInt32(this object data)
    {
        return (System.UInt32)data;
    }
    public static System.UInt64 ToUInt64(this object data)
    {
        return (System.UInt64)data;
    }

    public static System.SByte ToInt8(this object data)
    {
        return (System.SByte)data;
    }
    public static System.Int16 ToInt16(this object data)
    {
        return (System.Int16)data;
    }
    public static System.Int32 ToInt32(this object data)
    {
        return (System.Int32)data;
    }
    public static System.Int64 ToInt64(this object data)
    {
        return (System.Int64)data;
    }

    public static System.Single ToFloat32(this object data)
    {
        return (System.Single)data;
    }
    public static System.Double ToFloat64(this object data)
    {
        return (System.Double)data;
    }

    public static System.Boolean ToBoolean(this object data)
    {
        return (System.Boolean)data;
    }
    public static System.Byte[] ToBinary(this object data)
    {
        return (System.Byte[])data;
    }

    public static System.String ToString(this object data)
    {
        return (System.String)data;
    }

    public static Map ToMap(this object data)
    {
        return (Map)data;
    }

    public static List ToList(this object data)
    {
        return (List)data;
    }
}",
    [#{pattern => "(?s).*", code => lists:concat([Code, "\n", "\n", Cast])}].

%%%====================================================================
%%% HTML Define Part
%%%====================================================================
make_html_pattern(List) ->
    MenuCode = string:join([io_lib:format("<div class='protocol' onclick='load(\"~tsProtocol.html\")'><div class='inner'>~tp - ~ts</div></div>", [word:to_hump(Name), Protocol, Comment]) || {Protocol, Comment, _, Name} <- List], lists:concat(["\n", "    ", "    "])),
    Code = io_lib:format("<!DOCTYPE html>
<html>
<head>
    <meta charset='UTF-8'>
    <link rel='icon' href='data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiPz48c3ZnIHdpZHRoPSIyNCIgaGVpZ2h0PSIyNCIgdmlld0JveD0iMCAwIDQ4IDQ4IiBmaWxsPSJub25lIiB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciPjxyZWN0IHg9IjgiIHk9IjQiIHdpZHRoPSIzMiIgaGVpZ2h0PSI0MCIgcng9IjIiIHN0cm9rZT0iIzMzMyIgc3Ryb2tlLXdpZHRoPSI0IiBzdHJva2UtbGluZWNhcD0icm91bmQiIHN0cm9rZS1saW5lam9pbj0icm91bmQiLz48cGF0aCBkPSJNMTYgNEgyNVYyMEwyMC41IDE2TDE2IDIwVjRaIiBmaWxsPSJub25lIiBzdHJva2U9IiMzMzMiIHN0cm9rZS13aWR0aD0iNCIgc3Ryb2tlLWxpbmVjYXA9InJvdW5kIiBzdHJva2UtbGluZWpvaW49InJvdW5kIi8+PHBhdGggZD0iTTE2IDI4SDI2IiBzdHJva2U9IiMzMzMiIHN0cm9rZS13aWR0aD0iNCIgc3Ryb2tlLWxpbmVjYXA9InJvdW5kIi8+PHBhdGggZD0iTTE2IDM0SDMyIiBzdHJva2U9IiMzMzMiIHN0cm9rZS13aWR0aD0iNCIgc3Ryb2tlLWxpbmVjYXA9InJvdW5kIi8+PC9zdmc+' type='image/x-icon' />
    <title>Protocol</title>
    <style>
        html, body { margin: 0; width: 100vw; height: 100vh; display: flex; }
        body { opacity: 0; animation: fade-in 1s forwards; }
        @keyframes fade-in { 0% { opacity: 0; } 100% { opacity: 1; } }
        div { display: flex; }
        a { color: #fff; text-decoration: none; }
        iframe { width: 100%; height: 100%; border: unset; outline: unset; }

        .left {
            width: 200px;
            height: 100vh;
            overflow: auto;
            flex-direction: column;
            background-color: #323232;
        }

        .left > .protocol {
            width: 100%;
            padding: 8px 0px 8px 0px;
            margin-bottom: 2%;
            flex-shrink: 0;
            display: flex;
            justify-content: center;
            align-items: center;
            color: #fff;
            font-size: 0.88em;
        }

        .left > .protocol:hover {
            background-color: rgba(255,255,255,.1);
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
            width: calc(100vw - 80px);
        }

    </style>
    <script>
        function load(src) {
            const styles = {
                '.left': {
                    'color': '#000',
                    'background-color': '#f4f5f7',
                },
                '.left > .protocol:hover': {
                    'background-color': '#eaecf0',
                },
            };
            document.querySelector('iframe').src = `${src}?style=${encodeURIComponent(JSON.stringify(styles))}`;
        }
    </script>
</head>
<body>
    <div id='nav' class='left'>
        ~ts
    </div>
    <div id='pannel' class='right'>
        <iframe></iframe>
    </div>

</body>
</html>", [MenuCode]),
    [#{pattern => "(?s).*", code => unicode:characters_to_binary(lists:flatten(Code))}].
