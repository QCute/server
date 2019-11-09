%%%------------------------------------------------------------------
%%% @doc
%%% module maker
%%% @end
%%%------------------------------------------------------------------
-module(maker).
-export([start/2, connect_database/0]).
-export([hump/1, lower_hump/1]).
-export([save_param_list/1, get_param_list/0, find_param/1, find_param/2, check_param/2]).
-export([script_path/0]).
-export([term/1]).
-export([insert/1, select/1, execute/1]).
%%%==================================================================
%%% API functions
%%%==================================================================
%% @doc script union entry
start(CallBack, List) ->
    %% hard match
    {ok, DB} = connect_database(),
    parse_list(CallBack, DB, List).

%% @doc start pool
connect_database() ->
    case catch escript:script_name() of
        {'EXIT', _} ->
            %% application/erlang shell mode
            File = "config/main.config";
        _ ->
            %% erlang script mode
            File = prim_script_path() ++ "config/main.config"
    end,
    connect_database(File).

%% @doc save param
save_param_list(Param) when length(Param) rem 2 == 0 ->
    put('SHELL_PARAM', param(Param, []));
save_param_list(Param) ->
    Msg = io_lib:format("invalid shell argument length: ~s", [string:join(Param, " ")]),
    erlang:error(binary_to_list(list_to_binary(Msg))).

%% @doc get param
get_param_list() ->
    get('SHELL_PARAM').

%% @doc find shell param
find_param(Type) ->
    find_param(Type, []).
find_param(Type, Default) ->
    case get_param_list() of
        undefined ->
            Default;
        [] ->
            Default;
        List ->
            case lists:keyfind(type:to_list(Type), 1, List) of
                {_, Param} ->
                    Param;
                _ ->
                    Default
            end
    end.

%% @doc check shell param
check_param(Type, Param) ->
    case find_param(Type) of
        Param ->
            true;
        _ ->
            false
    end.

%% @doc hump name
%% hump_name -> HumpName
hump(Binary) when is_binary(Binary) ->
    hump(binary_to_list(Binary));
hump(Atom) when is_atom(Atom) ->
    hump(atom_to_list(Atom));
hump(Name) ->
    lists:concat([[case 96 < H andalso H < 123 of true -> H - 32; _ -> H end | T] || [H | T] <- string:tokens(Name, "_")]).

%% @doc lower_hump
%% lower_hump/LowerHump -> lowerHump
lower_hump(Name) ->
    [Head | _] = String = type:to_list(Name),
    [string:to_lower(Head) | tl(maker:hump(String))].

%%%==================================================================
%%% sql part
%%%==================================================================
%% @doc insert
insert(Sql) ->
    execute(Sql, select).

%% @doc select
select(Sql) ->
    execute(Sql, select).

%% @doc execute
execute(Sql) ->
    execute(Sql, []).
execute(Sql, Method) ->
    %% do not pass name pool to execute fetch
    %% pid for match message use
    Result = mysql_connector:query(whereis(mysql_connector), iolist_to_binary(Sql)),
    mysql_connector:handle_result(Sql, Method, Result, fun erlang:error/1).

%% connect to database
connect_database(File) ->
    {ok, [Config]} = file:consult(File),
    Main = proplists:get_value(main, Config, []),
    List = proplists:get_value(mysql_connector, Main, []),
    {ok, Pid} = mysql_connector:start_link(List),
    %% register pool name for query use
    erlang:register(mysql_connector, Pid),
    %% return config database name
    {ok, proplists:get_value(database, List, "")}.
    
%%%==================================================================
%%% Internal functions
%%%==================================================================
%% split shell param
param([], List) ->
    lists:reverse(List);
param([K, V | T], List) ->
    param(T, [{K, V} | List]).

%% @doc erlang script path
prim_script_path() ->
    script_path() ++ "../../../".

%% @doc erlang script path
script_path() ->
    %% dir name without /,add it to tail
    filename:dirname(escript:script_name()) ++ "/".

%% @doc to term
term(Raw) when is_integer(Raw) ->
    Raw;
term(Raw) ->
    case scan(Raw) of
        {ok, Term} ->
            Term;
        _ ->
            Raw
    end.
scan(Binary) when is_binary(Binary) ->
    scan(binary_to_list(Binary));
scan(String) ->
    case erl_scan:string(String ++ ".") of
        {ok, Tokens, _} ->
            erl_parse:parse_term(Tokens);
        _ ->
            undefined
    end.

%%%==================================================================
%%% data part
%%%==================================================================
%% parse list
parse_list(_, _, []) ->
    ok;
parse_list(CallBack, DataBase, [H | T]) ->
    Data = CallBack(DataBase, H),
    parse_file(element(1, H), Data),
    parse_list(CallBack, DataBase, T);
parse_list(CallBack, DataBase, W) ->
    io:format("~w, ~w, ~w~n", [CallBack, DataBase, W]).

%% write data to file
parse_file([], _) ->
    ok;
parse_file(File, PatternList) ->
    FilePath = prim_script_path() ++ File,
    case file:read_file(FilePath) of
        {ok, Binary} ->
            OriginData = binary_to_list(Binary),
            WriteData = parse_data(OriginData, PatternList),
            file:write_file(FilePath, WriteData);
        _ ->
            %% new file
            OriginData = binary_to_list(<<>>),
            WriteData = parse_data(OriginData, PatternList),
            file:write_file(FilePath, WriteData)
    end.

%% replace with new data
parse_data(FileData, []) ->
    FileData;
parse_data(FileData, [[] | T]) ->
    %% empty set
    parse_data(FileData, T);
parse_data(FileData, [{[], Data} | T]) ->
    %% no replace pattern, append to tail
    NewFileData = FileData ++ Data,
    parse_data(NewFileData, T);
parse_data(_, [{"(?s).*", Data} | T]) ->
    %% replace all mode, discard old data(re are too slow, avoid it)
    parse_data(Data, T);
parse_data(FileData, [{Pattern, Data} | T]) ->
    %% no regex option
    parse_data(FileData, [{Pattern, Data, []} | T]);
parse_data(FileData, [{Pattern, Data, Option} | T]) ->
    %% add/replace with pattern
    case re:run(FileData, Pattern, lists:usort([global | Option])) of
        {match, _} ->
            %% old target, replace with new data
            NewFileData = re:replace(FileData, Pattern, Data, lists:usort([{return, list} | Option])),
            parse_data(NewFileData, T);
        _ when Data =/= [] ->
            %% new target append to file end
            NewFileData = FileData ++ Data,
            parse_data(NewFileData, T);
        _ ->
            parse_data(FileData, T)
    end.
