%%%------------------------------------------------------------------
%%% @doc
%%% module maker
%%% @end
%%%------------------------------------------------------------------
-module(maker).
-export([start/2]).
-export([parse_args/1]).
-export([connect_database/0, insert/1, select/1, query/1]).
-export([root_path/0, script_path/0, relative_path/1, read_file/1, read_file/2, write_file/2, touch/1, touch/2]).
%%%==================================================================
%%% API functions
%%%==================================================================
%% @doc script union entry
-spec start(Callback :: function(), List :: [term()]) -> ok.
start(CallBack, List) ->
    DB = connect_database(),
    parse_list(CallBack, DB, List).

%% @doc parse shell args
-spec parse_args(Args :: [string()]) -> [{string(), list()}].
parse_args(Args) ->
    lists:reverse(lists:foldl(fun(K = [$- | _], A) -> [{K, []} | A];(V, [{K, L} | T]) -> [{K, lists:reverse([V | lists:reverse(L)])} | T];(_, A) -> A end, [], Args)).

%%%==================================================================
%%% Database and SQL
%%%==================================================================
%% @doc connect database
-spec connect_database() -> atom().
connect_database() ->
    File = root_path() ++ "config/main.config",
    {ok, [Config]} = file:consult(File),
    Main = proplists:get_value(main, Config, []),
    List = proplists:get_value(mysql_connector, Main, []),
    {ok, Pid} = mysql_connector:start_link(List),
    %% register pool name for query use
    erlang:register(mysql_connector, Pid),
    %% return config database name
    proplists:get_value(database, List, "").

%% @doc insert
-spec insert(Sql :: string()) -> term().
insert(Sql) ->
    execute(Sql, select).

%% @doc select
-spec select(Sql :: string()) -> term().
select(Sql) ->
    execute(Sql, select).

%% @doc query
-spec query(Sql :: string()) -> term().
query(Sql) ->
    execute(Sql, query).

%% execute sql
execute(Sql, Method) ->
    %% do not pass name pool to execute fetch
    %% pid for match message use
    Result = mysql_connector:query(whereis(mysql_connector), iolist_to_binary(Sql)),
    mysql_connector:handle_result(Sql, Method, Result, fun erlang:error/1).

%%%==================================================================
%%% Script Assistant
%%%==================================================================
%% @doc project root path
-spec root_path() -> string().
root_path() ->
    script_path() ++ "../../../".

%% @doc project relative script path
-spec script_path() -> string().
script_path() ->
    %% dir name without /,add it to tail
    filename:dirname(escript:script_name()) ++ "/".

%% @doc project relative file path
-spec relative_path(Path :: string()) -> string().
relative_path(Path) ->
    root_path() ++ Path.

%% @doc erlang script path
read_file(Name) ->
    read_file(Name, <<>>).
read_file(Name, Default) ->
    case file:read_file(root_path() ++ Name) of
        {ok, Binary} ->
            Binary;
        _ ->
            Default
    end.

%% @doc erlang script path
write_file(Name, Data) ->
    file:write_file(root_path() ++ Name, Data).

%% @doc erlang script path
touch(Name) ->
    touch(Name, <<>>).
touch(Name, Data) ->
    File = root_path() ++ Name,
    filelib:is_file(File) == false andalso file:write_file(File, Data) == ok.

%%%==================================================================
%%% RegEx Parse File
%%%==================================================================
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
    FilePath = root_path() ++ File,
    PathList = string:tokens(filename:dirname(File), "/"),
    [file:make_dir(root_path() ++ string:join(lists:sublist(PathList, Number), "/")) || Number <- lists:seq(1, length(PathList))],
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
