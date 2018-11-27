%%%-------------------------------------------------------------------
%%% @doc
%%% module maker
%%% @end
%%%-------------------------------------------------------------------
-module(maker).
-export([start/2]).
-export([save_param/1, get_param/0]).
%%%===================================================================
%%%
%%%===================================================================
%% @doc union entry
start(CallBack, List) ->
    case string:str(atom_to_list(node()), escript:script_name()) =/= 0 of
        true ->
            %% application/erlang shell mode
            File = "main.config";
        _ ->
            %% erlang script mode
            File = script_path() ++ "config/main.config"
    end,
    %% hard match
    {ok, DB} = start_pool(File),
    parse_list(CallBack, DB, List).

%% @doc save param
save_param(Param) ->
    put('SHELL_PARAM', param(Param, [])).

%% @doc get param
get_param() ->
    get('SHELL_PARAM').
%% ====================================================================
%% Internal functions
%% ====================================================================
%% split shell param
param([], List) ->
    lists:reverse(List);
param([K, V | T], List) ->
    param(T, [{K, V} | List]).

%% erlang script path
script_path() ->
    Name = lists:reverse(escript:script_name()),
    lists:reverse(trim_path(Name, [])) ++ "../../../".
trim_path([], List) ->
    List;
trim_path([$\\ | _] = List, _) ->
    List;
trim_path([$/ | _] = List, _) ->
    List;
trim_path([H | T], List) ->
    trim_path(T, [H | List]).

%% start database pool worker
start_pool(File) ->
    {ok, [[_, _, {_, Data}]]} = file:consult(File),
    {_, Cfg} = lists:keyfind(pool, 1, Data),
    {_, Host} = lists:keyfind(host, 1, Cfg),
    {_, Port} = lists:keyfind(port, 1, Cfg),
    {_, User} = lists:keyfind(user, 1, Cfg),
    {_, DataBase} = lists:keyfind(database, 1, Cfg),
    {_, Password} = lists:keyfind(password, 1, Cfg),
    {_, Encode} = lists:keyfind(encode, 1, Cfg),
    Pool = list_to_atom(DataBase),
    PoolArg = [{name, {local, Pool}}, {worker_module, mysql_conn}, {size, 4}, {max_overflow, 0}, {strategy, lifo}],
    poolboy:start_link(PoolArg, [Host, Port, User, Password, DataBase, fun(_, _, _, _) -> ok end, Encode, PoolArg]),
    {ok, Pool}.


%%% data part %%%
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
parse_file(File, PattenList) ->
    FilePath = script_path() ++ File,
    case file:read_file(FilePath) of
        {ok, Binary} ->
            OriginData = binary_to_list(Binary),
            WriteData = parse_data(OriginData, PattenList),
            file:write_file(FilePath, WriteData);
        _ ->
            %% new file
            OriginData = binary_to_list(<<>>),
            WriteData = parse_data(OriginData, PattenList),
            file:write_file(FilePath, WriteData)
    end.

%% replace with new data
parse_data(FileData, []) ->
    FileData;
parse_data(FileData, [[] | T]) ->
    parse_data(FileData, T);
parse_data(FileData, [{[], Data} | T]) ->
    NewFileData = FileData ++ Data,
    parse_data(NewFileData, T);
parse_data(FileData, [{Patten, Data} | T]) ->
    parse_data(FileData, [{Patten, Data, []} | T]);
parse_data(FileData, [{Patten, Data, Option} | T]) ->
    case re:run(FileData, Patten, [global]) of
        {match, _} ->
            %% old target, replace with new data
            NewFileData = re:replace(FileData, Patten, Data, [{return, list} | Option]),
            parse_data(NewFileData, T);
        _ when Data =/= [] ->
            %% new target append to file end
            NewFileData = FileData ++ Data,
            parse_data(NewFileData, T);
        _ ->
            parse_data(FileData, T)
    end.