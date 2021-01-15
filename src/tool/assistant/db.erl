%%%-------------------------------------------------------------------
%%% @doc
%%% database query/sql execute tool
%%% @end
%%%-------------------------------------------------------------------
-module(db).
%% API
-export([start/0, start/2]).
-export([version/0]).
-export([select_one/1, select_row/1, select_column/1]).
-export([select/1, insert/1, update/1, delete/1, query/1]).
-export([id/0, limit/0, initialize/0, get_auto_increment/1, set_auto_increment/2]).
-export([quote_string/1, quote_string/2]).
%% Includes
-include("common.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc start database connector pool
-spec start() -> {ok, Pid :: pid()} | {error, Reason :: term()}.
start() ->
    %% read pool args from application config
    PoolArgs = config:mysql_connector_pool(),
    %% read connector args from application config
    ConnectorArgs = config:mysql_connector(),
    %% use volley process pool manager to start connector pool
    start(PoolArgs, ConnectorArgs).

%% @doc start database connector pool
-spec start(PoolArgs :: proplists:proplist(), ConnectorArgs :: proplists:proplist()) -> {ok, Pid :: pid()} | {error, Reason :: term()}.
start(PoolArgs, ConnectorArgs) ->
    %% use volley process pool manager to start connector pool
    volley:start_pool(mysql_connector, [{worker, {mysql_connector, start_link, [ConnectorArgs]}} | PoolArgs]).

%% @doc version
-spec version() -> binary().
version() ->
    select_one("SELECT VERSION();").

%% @doc select one
-spec select_one(Sql :: list() | binary()) -> term().
select_one(Sql) ->
    case select(Sql) of
        [[One | _] | _] ->
            One;
        [] ->
            []
    end.

%% @doc select row
-spec select_row(Sql :: list() | binary()) -> term().
select_row(Sql) ->
    case select(Sql) of
        [Row | _] ->
            Row;
        [] ->
            []
    end.

%% @doc select column
-spec select_column(Sql :: list() | binary()) -> term().
select_column(Sql) ->
    [Head || [Head | _] <- select(Sql)].

%% @doc select
-spec select(Sql :: list() | binary()) -> term().
select(Sql) ->
    query(Sql).

%% @doc insert
-spec insert(Sql :: list() | binary()) -> term().
insert(Sql) ->
    query(Sql).

%% @doc update
-spec update(Sql :: list() | binary()) -> term().
update(Sql) ->
    query(Sql).

%% @doc delete
-spec delete(Sql :: list() | binary()) -> term().
delete(Sql) ->
    query(Sql).

%% @doc fix
-ifdef(DEBUG).
-define(QUERY(Sql, Worker), try mysql_connector:query(Sql, Worker, ?MINUTE_MILLISECONDS) catch ?EXCEPTION(_Class, Reason, _Stacktrace) -> misc:fix_sql(Reason) end).
-else.
-define(QUERY(Sql, Worker), mysql_connector:query(Sql, Worker, ?MINUTE_MILLISECONDS)).
-endif.

%% @doc query
%% @doc execute sql and fetch result
-spec query(Sql :: list() | binary()) -> term().
query(<<>>) ->
    ok;
query([]) ->
    ok;
query(Sql) ->
    case volley:get(mysql_connector) of
        {ok, Worker} ->
            ?QUERY(Sql, Worker);
        {error, Reason} ->
            erlang:exit({pool_error, {mysql_connector, Reason}})
    end.

%%%===================================================================
%%% database management
%%%===================================================================
%% @doc auto increment explain
%% 1001000000001
%% ↑  ↑        ↑
%% ↑  ↑        number sequence
%% ↑  server sequence
%% group sequence
%%

%% @doc get initialization auto increment id
-spec id() -> non_neg_integer().
id() ->
    ServerId = config:server_id(),
    ServerId * 1000000000.
    %% bigint 8(byte)/64(bit)
    %% 31536000000 = 1000 * 86400 * 365
    %% 1000000000000 / 31536000000 ~= 31.709791983764585
    %% maximize option ChannelId * 1000000000000000000 + ServerId * 1000000000000000.

%% @doc auto increment limit
-spec limit() -> non_neg_integer() | infinity.
limit() ->
    infinity.

%% @doc start initialization database
-spec initialize() -> ok.
initialize() ->
    try
        AutoIncrement = id() + 1,
        %% MySQL 8.x need
        %% set information_schema_stats_expiry = 0 in mysql.ini
        catch query("SET @@SESSION.`information_schema_stats_expiry` = 0;"),
        %% the AUTO_INCREMENT field after create is null, but insert some data after truncate it is 1
        TableList = select(<<"SELECT information_schema.`TABLES`.`TABLE_NAME` FROM information_schema.`TABLES` INNER JOIN information_schema.`COLUMNS` ON information_schema.`TABLES`.`TABLE_NAME` = information_schema.`COLUMNS`.`TABLE_NAME` WHERE information_schema.`TABLES`.`AUTO_INCREMENT` IN (1, NULL) AND information_schema.`TABLES`.`TABLE_SCHEMA` = DATABASE() AND information_schema.`COLUMNS`.`TABLE_SCHEMA` = DATABASE() AND information_schema.`COLUMNS`.`COLUMN_KEY` = 'PRI' AND information_schema.`COLUMNS`.`EXTRA` = 'auto_increment'">>),
        lists:foreach(fun([Table]) -> set_auto_increment(Table, AutoIncrement) end, TableList)
    catch ?EXCEPTION(_Class, Reason, Stacktrace) ->
        ?STACKTRACE(Reason, ?GET_STACKTRACE(Stacktrace))
    end.

%% @doc get auto increment
-spec get_auto_increment(Table :: atom() | string()) -> non_neg_integer().
get_auto_increment(Table) ->
    select_one(parser:format(<<"SELECT AUTO_INCREMENT FROM information_schema.`TABLES` WHERE `TABLE_SCHEMA` = DATABASE() AND `TABLE_NAME` = '~s'">>, [Table])).

%% @doc set auto increment
-spec set_auto_increment(Table :: atom() | string(), AutoIncrement :: non_neg_integer()) -> ok.
set_auto_increment(Table, AutoIncrement) ->
    %% maximize
    query(parser:format(<<"ALTER TABLE `~s` AUTO_INCREMENT = ~w">>, [Table, AutoIncrement])).

%% @doc sql quote string
-spec quote_string(Binary :: binary()) -> binary().
quote_string(Binary) ->
    quote_string(quote_string(Binary, single), double).

%% @doc sql quote string
-spec quote_string(Binary :: binary(), Type :: single | double) -> binary().
quote_string(Binary, single) ->
    binary:replace(Binary, <<"'">>, <<"\\'">>, [global]);
quote_string(Binary, double) ->
    binary:replace(Binary, <<"\"">>, <<"\\\"">>, [global]).

%%%===================================================================
%%% Internal functions
%%%===================================================================
