%%%-------------------------------------------------------------------
%%% @doc
%%% module maker
%%% @end
%%%-------------------------------------------------------------------
-module(maker).
-export([start/2]).
-export([save_param/1, get_param/0, check_param/2]).
-export([script_path/0]).
-export([term/1]).
%%%===================================================================
%%%
%%%===================================================================
%% @doc script union entry
start(CallBack, List) ->
    case string:str(atom_to_list(node()), escript:script_name()) =/= 0 of
        true ->
            %% application/erlang shell mode
            File = "main.config";
        _ ->
            %% erlang script mode
            File = prim_script_path() ++ "config/main.config"
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

%% @doc check shell param
check_param(Type, Param) ->
    case get_param() of
        undefined ->
            false;
        [] ->
            false;
        List ->
            case lists:keyfind(type:to_list(Type), 1, List) of
                {_, Param} ->
                    true;
                _ ->
                    false
            end
    end.
%% ====================================================================
%% Internal functions
%% ====================================================================
%% split shell param
param([], List) ->
    lists:reverse(List);
param([K, V | T], List) ->
    param(T, [{K, V} | List]).

%% @doc erlang script path
prim_script_path() ->
    Name = escript:script_name(),
    string:sub_string(Name, 1, max(string:rstr(Name, "/"), string:rstr(Name, "\\"))) ++ "../../../".

%% @doc erlang script path
script_path() ->
    Name = escript:script_name(),
    string:sub_string(Name, 1, max(string:rstr(Name, "/"), string:rstr(Name, "\\"))).

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


%% start database pool worker
start_pool(File) ->
    {ok, [List]} = file:consult(File),
    {_, Data} = lists:keyfind(main, 1, List),
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
    FilePath = prim_script_path() ++ File,
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
    case re:run(FileData, Patten, lists:usort([global | Option])) of
        {match, _} ->
            %% old target, replace with new data
            NewFileData = re:replace(FileData, Patten, Data, lists:usort([{return, list} | Option])),
            parse_data(NewFileData, T);
        _ when Data =/= [] ->
            %% new target append to file end
            NewFileData = FileData ++ Data,
            parse_data(NewFileData, T);
        _ ->
            parse_data(FileData, T)
    end.


