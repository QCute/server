%%%-------------------------------------------------------------------
%%% @doc
%%% database query/sql execute tool
%%% @end
%%%-------------------------------------------------------------------
-module(db).
-compile({inline, [query/1]}).
%% API
-export([start/0, start/2]).
-export([version/0]).
-export([select_one/1, select_row/1, select_column/1]).
-export([select_one/2, select_row/2, select_column/2]).
-export([select/1, insert/1, update/1, delete/1]).
-export([select/2, insert/2, update/2, delete/2]).
-export([query/1]).
-export([id/0, limit/0, initialize/0, get_auto_increment/1, set_auto_increment/2]).
%% Includes
-include("time.hrl").
-include("journal.hrl").
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
-spec start(PoolArgs :: [volley:config()], ConnectorArgs :: [mysql_connector:config()]) -> {ok, Pid :: pid()} | {error, Reason :: term()}.
start(PoolArgs, ConnectorArgs) ->
    %% use volley process pool manager to start connector pool
    volley:start_pool(?MODULE, [{worker, {mysql_connector, start_link, [ConnectorArgs]}} | PoolArgs]).

%% @doc version
-spec version() -> binary().
version() ->
    select_one("SELECT VERSION();").

%% @doc select one
-spec select_one(Sql :: mysql_connector:sql()) -> mysql_connector:data() | [].
select_one(Sql) ->
    case select(Sql) of
        [[One | _] | _] ->
            One;
        [] ->
            []
    end.

%% @doc select one
-spec select_one(Sql :: mysql_connector:sql(), Binding :: tuple() | list()) -> mysql_connector:data() | [].
select_one(Sql, Binding) ->
    case select(Sql, Binding) of
        [[One | _] | _] ->
            One;
        [] ->
            []
    end.

%% @doc select row
-spec select_row(Sql :: mysql_connector:sql()) -> [mysql_connector:data()] | [].
select_row(Sql) ->
    case select(Sql) of
        [Row | _] ->
            Row;
        [] ->
            []
    end.

%% @doc select row
-spec select_row(Sql :: mysql_connector:sql(), Binding :: tuple() | list()) -> [mysql_connector:data()] | [].
select_row(Sql, Binding) ->
    case select(Sql, Binding) of
        [Row | _] ->
            Row;
        [] ->
            []
    end.

%% @doc select column
-spec select_column(Sql :: mysql_connector:sql()) -> [mysql_connector:data()] | [].
select_column(Sql) ->
    [Head || [Head | _] <- select(Sql)].

%% @doc select column
-spec select_column(Sql :: mysql_connector:sql(), Binding :: tuple() | list()) -> [mysql_connector:data()] | [].
select_column(Sql, Binding) ->
    [Head || [Head | _] <- select(Sql, Binding)].

%% @doc select
-spec select(Sql :: mysql_connector:sql()) -> mysql_connector:rows_data() | [].
select(Sql) ->
    query(Sql).

%% @doc insert
-spec select(Sql :: mysql_connector:sql(), Binding :: tuple() | list()) -> mysql_connector:rows_data() | [].
select(Sql, Binding) ->
    query(parser:format(Sql, Binding)).

%% @doc insert
-spec insert(Sql :: mysql_connector:sql()) -> mysql_connector:affected_rows() | mysql_connector:insert_id() | mysql_connector:rows_data() | [].
insert(Sql) ->
    query(Sql).

%% @doc insert
-spec insert(Sql :: mysql_connector:sql(), Binding :: tuple() | list()) -> mysql_connector:affected_rows() | mysql_connector:insert_id() | mysql_connector:rows_data() | [].
insert(Sql, Binding) ->
    query(parser:format(Sql, Binding)).

%% @doc update
-spec update(Sql :: mysql_connector:sql()) -> mysql_connector:affected_rows() | [].
update(Sql) ->
    query(Sql).

%% @doc update
-spec update(Sql :: mysql_connector:sql(), Binding :: tuple() | list()) -> mysql_connector:affected_rows() | [].
update(Sql, Binding) ->
    query(parser:format(Sql, Binding)).

%% @doc delete
-spec delete(Sql :: mysql_connector:sql()) -> mysql_connector:affected_rows() | mysql_connector:rows_data() | [].
delete(Sql) ->
    query(Sql).

%% @doc delete
-spec delete(Sql :: mysql_connector:sql(), Binding :: tuple() | list()) -> mysql_connector:affected_rows() | mysql_connector:rows_data() | [].
delete(Sql, Binding) ->
    query(parser:format(Sql, Binding)).

%% @doc try fix
-ifdef(DEBUG).
-define(QUERY(Query), try Query catch ?EXCEPTION(_Class, Reason, _Stacktrace) -> misc:fix_sql(Reason) end).
-else.
-define(QUERY(Query), Query).
-endif.

%% @doc query
%% @doc execute sql and fetch result
-spec query(Sql :: mysql_connector:sql()) -> mysql_connector:affected_rows() | mysql_connector:insert_id() | mysql_connector:rows_data() | [].
query("") ->
    [];
query(<<>>) ->
    [];
query(Sql) ->
    Worker = volley:get(?MODULE),
    ?QUERY(mysql_connector:query(Sql, Worker, ?MINUTE_MILLISECONDS)).

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
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?STACKTRACE(Class, Reason, ?GET_STACKTRACE(Stacktrace))
    end.

%% @doc get auto increment
-spec get_auto_increment(Table :: atom() | string()) -> non_neg_integer().
get_auto_increment(Table) ->
    select_one(parser:format(<<"SELECT AUTO_INCREMENT FROM information_schema.`TABLES` WHERE `TABLE_SCHEMA` = DATABASE() AND `TABLE_NAME` = '~s'">>, [Table])).

%% @doc set auto increment
-spec set_auto_increment(Table :: atom() | string(), AutoIncrement :: non_neg_integer()) -> non_neg_integer().
set_auto_increment(Table, AutoIncrement) ->
    %% maximize
    query(parser:format(<<"ALTER TABLE `~s` AUTO_INCREMENT = ~w">>, [Table, AutoIncrement])).

%%%===================================================================
%%% Internal functions
%%%===================================================================
