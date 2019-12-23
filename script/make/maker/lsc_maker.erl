%%%------------------------------------------------------------------
%%% @doc
%%% module user load/save/clean maker
%%% @end
%%%------------------------------------------------------------------
-module(lsc_maker).
-export([start/4]).
%%%==================================================================
%%% API functions
%%%==================================================================
start(InFile, Loader, Saver, Cleaner) ->
    Result = analyse(InFile),
    {LoadCode, SaveCode, CleanCode} = make_code(Result, [], [], []),
    replace_code(Loader, "(?m)(?s)(?<!\\S)(^do_load.+?)(?=\\.$|\\%)\\.\\n?", LoadCode),
    replace_code(Saver, "(?m)(?s)(?<!\\S)(^do_save.+?)(?=\\.$|\\%)\\.\\n?", SaveCode),
    replace_code(Cleaner, "(?m)(?s)(?<!\\S)(^do_clean.+?)(?=\\.$|\\%)\\.\\n?", CleanCode),
    ok.

%%%==================================================================
%%% Internal functions
%%%==================================================================
%% analyse file code
analyse(File) ->
    {ok, Binary} = file:read_file(maker:prim_script_path() ++ File),
    {match, [String]} = re:run(Binary, "(?m)(?s)^-record\\(user\\s*,\\s*\\{.+?^((?!%).)*?\\}\s*\\)\\.(?=$|\\s|%)", [{capture, first, list}]),
    List = string:tokens(String, "\n"),
    analyse_row(List, []).

analyse_row([], List) ->
    lists:reverse(List);
analyse_row([Row | T], List) ->
    case re:run(Row, "\\(.*?\\)", [{capture, all, list}]) of
        {match, [String]} ->
            Expression = hd(string:tokens(Row, "%%")),
            Assignment = hd(string:tokens(Expression, "=")),
            Name = [X || X <- Assignment, X =/= $, andalso X =/= 32],
            LoadFlag = string:str(String, "load"),
            SaveFlag = string:str(String, "save"),
            CleanFlag = string:str(String, "clean"),
            case LoadFlag =/= 0 orelse SaveFlag =/= 0 orelse CleanFlag =/= 0 of
                true ->
                    analyse_row(T, [{Name, LoadFlag, SaveFlag, CleanFlag} | List]);
                false ->
                    analyse_row(T, List)
            end;
        _ ->
            analyse_row(T, List)
    end.

%% make code
make_code([], LoadCode, SaveCode, CleanCode) ->
    AllLoadCode = LoadCode ++ "do_load(_, User) ->\n    User.\n",
    AllSaveCode = SaveCode ++ "do_save(_, User) ->\n    User.\n",
    AllCleanCode = CleanCode ++ "do_clean(_, User) ->\n    User.\n",
    {AllLoadCode, AllSaveCode, AllCleanCode};

make_code([{Name, LoadFlag, SaveFlag, CleanFlag} | T], LoadCode, SaveCode, CleanCode) ->
    case LoadFlag of
        0 ->
            Load = [];
        _ ->
            Load = io_lib:format("do_load(#user.~s, User) ->\n    ~s:load(User);~n", [Name, Name])
    end,
    case SaveFlag of
        0 ->
            Save = [];
        _ ->
            Save = io_lib:format("do_save(#user.~s, User) ->\n    ~s:save(User);~n", [Name, Name])
    end,
    case CleanFlag of
        0 ->
            Clean = [];
        _ ->
            Clean = io_lib:format("do_clean(#user.~s, User) ->\n    ~s:clean(User);~n", [Name, Name])
    end,
    make_code(T, LoadCode ++ Load, SaveCode ++ Save, CleanCode ++ Clean).

%% replace code
replace_code(OutFile, Match, Code) ->
    {ok, Binary} = file:read_file(maker:prim_script_path() ++ OutFile),
    Data = re:replace(Binary, Match, Code, [{return, binary}]),
    file:write_file(maker:prim_script_path() ++ OutFile, Data).
