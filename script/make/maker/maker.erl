%%%-------------------------------------------------------------------
%%% @doc
%%% module maker
%%% @end
%%%-------------------------------------------------------------------
-module(maker).
-export([start/2]).
-export([parse_args/1]).
-export([connect_database/0, insert/1, select/1, query/1]).
-export([root_path/0, script_path/0, relative_path/1]).
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc script union entry
-spec start(Callback :: function(), List :: [term()]) -> ok.
start(CallBack, List) ->
    DB = connect_database(),
    parse_list(CallBack, DB, List).

%% @doc parse shell args
-spec parse_args(Args :: [string()]) -> [{string(), list()}].
parse_args(Args) ->
    lists:reverse(lists:foldl(fun([$-, $-], _) -> erlang:error("unknown option: --"); ([$-, $- | K], A) -> [Key | Value] = string:tokens("-" ++ K, "="), [{Key, Value} | A];(K = [$- | _], A) -> [{K, []} | A];(V, [{K, L} | T]) -> [{K, lists:reverse([V | lists:reverse(L)])} | T];(_, A) -> A end, [], Args)).

%%%===================================================================
%%% Database and SQL
%%%===================================================================
%% @doc connect database
-spec connect_database() -> atom().
connect_database() ->
    %% File = root_path() ++ "config/local.config",
    %% {ok, [Config]} = file:consult(File),
    %% Main = proplists:get_value(main, Config, []),
    %% List = proplists:get_value(mysql_connector, Main, []),
    List = [
        {host, config:mysql_connector_host()},
        {port, config:mysql_connector_port()},
        {user, config:mysql_connector_user()},
        {password, config:mysql_connector_password()},
        {database, config:mysql_connector_database()},
        {encoding, config:mysql_connector_encoding()}
    ],
    {ok, Pid} = mysql_connector:start_link(List),
    %% register pool name for query use
    erlang:register(mysql_connector, Pid),
    %% return config database name
    proplists:get_value(database, List, "").

%% @doc insert
-spec insert(Sql :: string()) -> term().
insert(Sql) ->
    query(Sql).

%% @doc select
-spec select(Sql :: string()) -> term().
select(Sql) ->
    query(Sql).

%% @doc query
-spec query(Sql :: string()) -> term().
query(Sql) ->
    mysql_connector:query(Sql, mysql_connector).

%%%===================================================================
%%% Script Assistant
%%%===================================================================
%% @doc project relative script path
-spec script_path() -> string().
script_path() ->
    %% dir name without /,add it to tail
    filename:dirname(escript:script_name()) ++ "/".

%% @doc project root path
-spec root_path() -> string().
root_path() ->
    script_path() ++ "../../../".

%% @doc project relative file path
-spec relative_path(Path :: string()) -> string().
relative_path(Path) ->
    root_path() ++ Path.

%%%===================================================================
%%% RegEx Parse File
%%%===================================================================
%% parse list
parse_list(_, _, []) ->
    ok;
parse_list(CallBack, DataBase, [H | T]) ->
    Data = CallBack(DataBase, H),
    parse_file(element(1, H), Data),
    parse_list(CallBack, DataBase, T);
parse_list(CallBack, DataBase, What) ->
    io:format("Unknown Args: ~w, ~w, ~w~n", [CallBack, DataBase, What]).

%% write data to file
parse_file([], _) ->
    ok;
parse_file(File, PatternList) ->
    FilePath = relative_path(File),
    case file:read_file(FilePath) of
        {ok, Binary} ->
            OriginData = binary_to_list(Binary),
            WriteData = parse_data(OriginData, PatternList),
            file:write_file(FilePath, WriteData);
        _ ->
            %% new file
            OriginData = binary_to_list(<<>>),
            WriteData = parse_data(OriginData, PatternList),
            filelib:ensure_dir(FilePath),
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
