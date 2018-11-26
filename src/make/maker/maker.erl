%%%-------------------------------------------------------------------
%%% @doc
%%% module maker
%%% @end
%%%-------------------------------------------------------------------
-module(maker).
-export([start/2]).
%%%===================================================================
%%%
%%%===================================================================
%% @doc union entry
start(CallBack, List) ->
    case string:str(atom_to_list(node()), escript:script_name()) =/= 0 of
        true ->
            %% application/erlang shell mode
            File = "../config/main.config";
        _ ->
            %% erlang script mode
            File = "../../config/main.config"
    end,
    %% hard match
    {ok, DB} = start_pool(File),
    parse_list(CallBack, DB, List).
%% ====================================================================
%% Internal functions
%% ====================================================================
%% start database pool worker
start_pool(File) ->
    {ok, [[_, _, {_, Data}]]} = file:consult(File),
    {_, Cfg} = lists:keyfind(database, 1, Data),
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
    case file:read_file(File) of
        {ok, Binary} ->
            OriginData = binary_to_list(Binary),
            WriteData = parse_data(OriginData, PattenList),
            file:write_file(File, WriteData);
        _ ->
            %% new file
            OriginData = binary_to_list(<<>>),
            WriteData = parse_data(OriginData, PattenList),
            file:write_file(File, WriteData)
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