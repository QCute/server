%%%-------------------------------------------------------------------
%%% @doc
%%% module maker
%%% @end
%%%-------------------------------------------------------------------
-module(maker).
-export([start/2, start_pool/0]).
-export([save_param_list/1, get_param_list/0, find_param/1, check_param/2]).
-export([script_path/0, add_path/0]).
-export([term/1]).
-export([insert/1, select/1, execute/1]).
%%%===================================================================
%%%
%%%===================================================================
%% @doc script union entry
start(CallBack, List) ->
    %% hard match
    {ok, DB} = start_pool(),
    parse_list(CallBack, DB, List).

%% @doc start pool
start_pool() ->
    case catch escript:script_name() of
        {'EXIT', _} ->
            %% application/erlang shell mode
            File = "config/main.config";
        _ ->
            %% erlang script mode
            File = prim_script_path() ++ "config/main.config"
    end,
    start_pool(File).

%% @doc save param
save_param_list(Param) when length(Param) rem 2 == 0 ->
    put('SHELL_PARAM', param(Param, []));
save_param_list(Param) ->
    Msg = io_lib:format("invail shell argument length: ~s", [string:join(Param, " ")]),
    erlang:error(binary_to_list(list_to_binary(Msg))).

%% @doc get param
get_param_list() ->
    get('SHELL_PARAM').

%% @doc find shell param
find_param(Type) ->
    case get_param_list() of
        undefined ->
            [];
        [] ->
            [];
        List ->
            case lists:keyfind(type:to_list(Type), 1, List) of
                {_, Param} ->
                    Param;
                _ ->
                    []
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

%% ====================================================================
%% sql part
%% ====================================================================
%% @doc insert
insert(Sql) ->
    execute(Sql, insert).

%% @doc select
select(Sql) ->
    execute(Sql, select).

%% @doc execute
execute(Sql) ->
    execute(Sql, []).
execute(Sql, Args) ->
    %% do not pass name pool to execute fetch
    %% pid for match message use
    Result = mysql_conn:fetch(whereis(pool), iolist_to_binary(Sql), self()),
    handle_result(Sql, Args, Result).

handle_result(_, _, {data, Result}) ->
    mysql:get_result_rows(Result);
handle_result(_, [], {update, _Result}) ->
    ok;
handle_result(_, insert, {updated, Result}) ->
    mysql:get_result_insert_id(Result);
handle_result(_, _, {updated, Result}) ->
    mysql:get_result_affected_rows(Result);
handle_result(Sql, _, {error, Result}) ->
    ErrorCode = mysql:get_result_err_code(Result),
    Reason = mysql:get_result_reason(Result),
    erlang:error({sql_error, {Sql, ErrorCode, Reason}}).

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
    {_, Encoding} = lists:keyfind(encode, 1, Cfg),
    {ok, Pid} = mysql_conn:start(Host, Port, User, Password, DataBase, fun(_, _, _, _) -> ok end, Encoding, pool),
    %% register pool name for query use
    erlang:register(pool, Pid),
    %% return config database name
    {ok, DataBase}.
    
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
    script_path() ++ "../../../".

%% @doc erlang script path
script_path() ->
    Name = escript:script_name(),
    string:sub_string(Name, 1, max(string:rstr(Name, "/"), string:rstr(Name, "\\"))).

%% @doc add beam path for erlang script
add_path() ->
    code:add_path(prim_script_path() ++ "beam/").

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

%% ====================================================================
%% data part
%% ====================================================================
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


