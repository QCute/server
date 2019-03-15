%%%-------------------------------------------------------------------
%%% @doc
%%% module script use for erlang script
%%% @end
%%%-------------------------------------------------------------------
-module(script).
-compile(nowarn_export_all).
-compile(export_all).

main([]) ->
    make();
main(["clean"]) ->
    clean();
main(["maker"]) ->
    maker();
main(["beam"]) ->
    beam();
main(["pt", File]) ->
    main(["protocol", File]);
main(["protocol", Name | T]) ->
    protocol(Name, T);
main([Name | T]) ->
    script(Name, T).

make() ->
    file:set_cwd(script_path()),
    make:all(),
    ok.

clean() ->
    file:set_cwd(script_path() ++ "../beam"),
    case os:type() of
        {win32, _} ->
            Cmd = "powershell rm ";
        _ ->
            Cmd = "rm "
    end,
    os:cmd(Cmd ++ "*.beam"),
    ok.

maker() ->
    file:set_cwd(script_path() ++ "../src/make/"),
    make:all(),
    ok.

beam() ->
    Path = script_path(),
    update_include(),
    os:cmd("erlc +debug_info -o " ++ Path ++ "../beam/ " ++ Path ++ "../src/debug/user_default.erl"),
    ok.

protocol(Name, T) ->
    Path = script_path(),
    Cmd = lists:flatten(lists:concat(["escript ", Path, "../src/make/protocol/protocol_script_", Name, ".erl ", T])),
    os:cmd(Cmd),
    ok.

script(Name, T) ->
    Path = script_path(),
    Cmd = lists:flatten(lists:concat(["escript ", Path, "../src/make/script/", Name, "_script.erl ", T])),
    os:cmd(Cmd),
    ok.

script_path() ->
    Name = escript:script_name(),
    string:sub_string(Name, 1, max(string:rstr(Name, "/"), string:rstr(Name, "\\"))).

%% @doc update all include
update_include() ->
    %% src/debug dir by default
    Name = escript:script_name(),
    Path = string:sub_string(Name, 1, max(string:rstr(Name, "/"), string:rstr(Name, "\\"))),
    update_include(Path ++ "user_default.erl", Path, "../../include/").
update_include(FilePath, ScriptPath, IncludePath) ->
    %% list all file
    {ok, LineList} = file:list_dir_all(ScriptPath ++ IncludePath),
    %% extract file name from file path
    Name = filename:basename(FilePath, ".erl"),
    %% construct include line
    Include = ["-include(\"" ++ IncludePath ++ Line ++ "\").\n" || Line <- LineList, string:str(Line, ".hrl") =/= 0],
    IncludePattern = "(?m)(^-include.+?)(?=\\.$)\\.\n?",
    %% construct data and pattern
    %% module declare
    Module = "-module(" ++ Name ++ ").\n",
    ModulePattern = "-module\\(" ++ Name ++ "\\)\\.\n",
    %% no warn declare
    NoWarn = "-compile(nowarn_export_all).\n",
    NoWarnPattern = "-compile\\(nowarn_export_all\\)\\.\n",
    %% export declare
    Export = "-compile(export_all).\n",
    ExportPattern = "-compile\\(export_all\\)\\.\n",
    %% read file data
    Data = binary_to_list(max(element(2, file:read_file(FilePath)), <<>>)),
    %% remove old data
    NewData = lists:foldr(fun(P, L) -> re:replace(L, P, "", [global, {return, list}]) end, Data, [ModulePattern, NoWarnPattern, ExportPattern, IncludePattern]),
    %% concat head include and other origin code
    file:write_file(FilePath, Module ++ NoWarn ++ Export ++ Include ++ NewData),
    ok.