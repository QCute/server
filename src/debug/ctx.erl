%%%-------------------------------------------------------------------
%%% @doc
%%% module ctx (context)
%%% @end
%%%-------------------------------------------------------------------
-module(ctx).
-compile(nowarn_export_all).
-compile(nowarn_deprecated_function).
%% API
-compile(export_all).

%% @doc for e script
main(_) ->
    code:add_path("beam"),
    code:add_path("../beam"),
    code:add_path("../../beam"),
    %io:format("~p~n", [word:sensitive("官方")]),
    %% equip


    io:format("~ts~n", [ts()]),
    %% rands
    ok.


append([H|T], Tail) ->
    [H|append(T, Tail)];
append([], Tail) ->
    Tail.


%% @doc file encoding test
ts() ->
    case "一" of
        [228, 184, 128] ->
            utf8;
        [78, 0] ->
            unicode;
        [210, 187] ->
            gbk
    end.

%% 一
%% <<228,184,128>>  .utf8      228*256*256 + 184*256 + 128   [14989440]
%% <<78,0>>         .unicode   78*256 + 0                    [19968]
%% <<210,187>>      .gbk       210*256+187                   [53947]


%% @doc recompile and reload module
cc(Module) ->
    %% in config dir by default
    cc(Module, "../include/", "../beam/").
cc(Module, IncludePath, BeamPath) ->
    case os:type() of
        {win32, nt} ->
            Command = lists:concat(["chcp 65001>nul && where /R ..\\src\\ ", Module, ".erl"]);
        _ ->
            Command = lists:concat(["find ../src/ -name ", Module, ".erl"])
    end,
    %% recompile
    FilePath = [C || C <- os:cmd(Command), C =/= $\r andalso C =/= $\n],
    c:c(FilePath, [debug_info, {i, IncludePath}, {outdir, BeamPath}]),
    %% reload
    c:l(Module).

%% @doc hot reload all module
reload() ->
    %% in config dir
    reload("../beam").
reload(BeamPath) ->
    case os:type() of
        {win32, nt} ->
            LineEnding = "\r\n",
            ListCommand = "powershell ls";
        _ ->
            LineEnding = "\n",
            ListCommand = "ls -l "
    end,
    LineList = string:tokens(os:cmd(ListCommand ++ BeamPath), LineEnding),
    [c:l(list_to_atom(hd(hd(element(2, re:run(Line, "\\w+(?=\\.beam)", [global, {capture, first, list}])))))) || Line <- LineList, string:str(Line, ".beam") =/= 0],
    ok.



%% format
format(F, A) ->
    binary_to_list(list_to_binary(io_lib:format(F, A))).

%% to hump name
hump(Binary) when is_binary(Binary) ->
    hump(binary_to_list(Binary));
hump(Atom) when is_atom(Atom) ->
    hump(atom_to_list(Atom));
hump(Name) ->
    lists:concat([[case 96 < H andalso H < 123 of true -> H - 32; _ -> H end | T] || [H | T] <- string:tokens(Name, "_")]).



%% match record(multi line)
%% 跨行匹配左边不接非空白字符，名字开头，后接以.结尾或者后面是注释%的记录
%% (?s)(?<!\\S)(-record\\(~s\\s*,.+?)(?=\\.$|\\%)\\.

%% function(multi line)
%% 跨行匹配左边不接非空白字符，名字开头，后接以.结尾或者后面是注释%的函数
%% (?s)(?<!\\S)(~s.+?)(?=\\.$|\\%)\\.

%% define(single line)
%% 跨行匹配左边不接非空白字符，名字开头，后接以.结尾或者后面是注释%的定义
%% (?<!\\S)(-define\\s*\\(~s.+?)(?=\\.$|\\%)\\.

%% all include(single line)
%% 跨行匹配左边不接非空白字符，名字开头，后接以.结尾或者后面是注释%的依赖
%% (?<!\\S)(-include\\s*\\(~s\\s*\\.+?)(?=\\.$|\\%)\\.
%% 匹配所有include
%% (?<!\\S)(-include.+?)(?=\\.$|\\%)\\.

