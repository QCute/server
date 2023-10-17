%%%-------------------------------------------------------------------
%%% @doc
%%% makers common tool
%%% @end
%%%-------------------------------------------------------------------
-module(maker).
-export([start/2]).
-export([parse_args/1]).
-export([config/0]).
-export([connect_database/0, connect_database/1]).
-export([root_path/0, script_path/0, relative_path/1]).
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc script union entry
-spec start(Callback :: function(), List :: [term()]) -> ok.
start(CallBack, List) ->
    connect_database(),
    lists:foreach(fun(I = #{file := File}) -> parse_file(File, CallBack(I)); (What) -> erlang:throw(lists:flatten(io_lib:format("Unknown Args: ~w, ~w~n", [CallBack, What]))) end, List).

%% @doc parse shell args
-spec parse_args(Args :: [string()]) -> [{string(), list()}].
parse_args(List) ->
    lists:reverse(lists:foldl(fun([$-], _) -> erlang:throw("unknown option: -"); ([$-, $-], _) -> erlang:throw("unknown option: --"); ([$-, $- | K], A) -> [Key | Value] = string:tokens(K, "="), [{Key, Value} | A];([$- | K], A) -> [{K, []} | A];(V, [{Key, L} | T]) -> [{Key, L ++ [V]} | T];(O, _) -> erlang:throw(lists:flatten(io_lib:format("unknown option: ~ts", [O]))) end, [], List)).

%%%===================================================================
%%% Database and SQL
%%%===================================================================
%% @doc get config file
-spec config() -> term().
config() ->
    %% find local src file
    List = [begin {ok, [Config]} = file:consult(File), Config end || File <- filelib:wildcard(relative_path("config/src/*.config"))],
    [Config | _] = [Config || Config <- List, proplists:get_value(node_type, proplists:get_value(main, Config, [])) == local],
    Config.

%% @doc connect database
-spec connect_database() -> {ok, pid()} | {error, term()}.
connect_database() ->
    File = config(),
    connect_database(File).

%% @doc connect database
-spec connect_database(Config :: term()) -> {ok, pid()} | {error, term()}.
connect_database(Config) ->
    Main = proplists:get_value(main, Config, []),
    PoolArgs = proplists:get_value(mysql_connector_pool, Main, []),
    ConnectorArgs = proplists:get_value(mysql_connector, Main, []),
    volley:start_link(),
    db:start(PoolArgs, ConnectorArgs).

%%%===================================================================
%%% Script Assistant
%%%===================================================================
%% @doc project relative script path
-spec script_path() -> string().
script_path() ->
    %% dir name without /,add it to tail
    lists:concat([filename:dirname(escript:script_name()), "/"]).

%% @doc project root path
-spec root_path() -> string().
root_path() ->
    lists:concat([script_path(), "../../../"]).

%% @doc project relative file path
-spec relative_path(Path :: string()) -> string().
relative_path(Path) ->
    lists:concat([root_path(), Path]).

%%%===================================================================
%%% RegEx Parse File
%%%===================================================================
%% write data to file
parse_file(File, PatternList) ->
    FilePath = relative_path(File),
    %% make dir
    filelib:ensure_dir(FilePath),
    %% touch file
    not filelib:is_file(FilePath) andalso file:write_file(FilePath, <<>>),
    {ok, Binary} = file:read_file(FilePath),
    WriteData = parse_data(Binary, PatternList),
    file:write_file(FilePath, WriteData).

%% replace with new data
parse_data(FileData, []) ->
    FileData;
parse_data(FileData, [#{pattern := [], code := Data} | T]) ->
    %% no replace pattern, append to tail
    parse_data(<<FileData/binary, Data/binary>>, T);
parse_data(_, [#{pattern := "(?s).*", code := Data} | T]) ->
    %% replace all mode, discard old data(re are too slow, avoid it)
    parse_data(Data, T);
parse_data(FileData, [H = #{pattern := Pattern, code := Data} | T]) ->
    %% add/replace with pattern
    Option = maps:get(option, H, []),
    case re:run(FileData, Pattern, lists:usort([global | Option])) of
        {match, _} ->
            %% old target, replace with new data
            NewFileData = re:replace(FileData, Pattern, Data, lists:usort([global, {return, binary} | Option])),
            parse_data(NewFileData, T);
        _ ->
            %% new target append to file end
            parse_data(<<FileData/binary, (iolist_to_binary(Data))/binary>>, T)
    end.
