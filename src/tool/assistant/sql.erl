%%%-------------------------------------------------------------------
%%% @doc
%%% module sql execute tool
%%% @end
%%%-------------------------------------------------------------------
-module(sql).
%% API
-export([start/0]).
-export([version/0]).
-export([select_one/1, select_row/1]).
-export([select_one/3, select_row/3]).
-export([select/1, insert/1, update/1, delete/1, query/1]).
-export([select/3, insert/3, update/3, delete/3, query/3]).
-export([id/0, initialize/0, get_auto_increment/1, set_auto_increment/2]).
%% Includes
-include("common.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc start database connector pool
-spec start() -> {ok, Pid :: pid()} | {error, Reason :: term()}.
start() ->
    %% read connector args from application config
    {ok, ConnectorArgs} = application:get_env(mysql_connector),
    %% read pool args from application config
    {ok, ConnectorPoolArgs} = application:get_env(mysql_connector_pool),
    %% use volley process pool manager to start connector pool
    volley:start_pool(mysql_connector, [{worker, {mysql_connector, start_link, [ConnectorArgs]}} | ConnectorPoolArgs]).

%% @doc version
-spec version() -> binary().
version() ->
    select_one("SELECT VERSION();").

%% @doc select one
-spec select_one(Sql :: list() | binary()) -> term().
select_one(Sql) ->
    select_one(mysql_connector, table, Sql).
-spec select_one(Connector :: atom(), Table :: atom(), Sql :: list() | binary()) -> term().
select_one(Connector, Table, Sql) ->
    case select(Connector, Table, Sql) of
        [[One]] ->
            One;
        _ ->
            []
    end.

%% @doc select row
-spec select_row(Sql :: list() | binary()) -> term().
select_row(Sql) ->
    select_row(mysql_connector, table, Sql).
-spec select_row(Connector :: atom(), Table :: atom(), Sql :: list() | binary()) -> term().
select_row(Connector, Table, Sql) ->
    case select(Connector, Table, Sql) of
        [Row] ->
            Row;
        _ ->
            []
    end.

%% @doc select row
-spec select(Sql :: list() | binary()) -> term().
select(Sql) ->
    select(mysql_connector, table, Sql).
-spec select(Connector :: atom(), Table :: atom(), Sql :: list() | binary()) -> term().
select(Connector, Table, Sql) ->
    statistics(Table, select),
    execute(Connector, Sql, select).

%% @doc insert
-spec insert(Sql :: list() | binary()) -> term().
insert(Sql) ->
    insert(mysql_connector, table, Sql).
-spec insert(Connector :: atom(), Table :: atom(), Sql :: list() | binary()) -> term().
insert(Connector, Table, Sql) ->
    statistics(Table, insert),
    execute(Connector, Sql, insert).

%% @doc update
-spec update(Sql :: list() | binary()) -> term().
update(Sql) ->
    update(mysql_connector, table, Sql).
-spec update(Connector :: atom(), Table :: atom(), Sql :: list() | binary()) -> term().
update(Connector, Table, Sql) ->
    statistics(Table, update),
    execute(Connector, Sql, update).

%% @doc delete
-spec delete(Sql :: list() | binary()) -> term().
delete(Sql) ->
    delete(mysql_connector, table, Sql).
-spec delete(Connector :: atom(), Table :: atom(), Sql :: list() | binary()) -> term().
delete(Connector, Table, Sql) ->
    statistics(Table, delete),
    execute(Connector, Sql, delete).

%% @doc query
-spec query(Sql :: list() | binary()) -> term().
query(Sql) ->
    query(mysql_connector, table, Sql).
-spec query(Connector :: atom(), Table :: atom(), Sql :: list() | binary()) -> term().
query(Connector, Table, Sql) ->
    statistics(Table, query),
    execute(Connector, Sql, query).
%%%===================================================================
%%% connect pool adapter
%%%===================================================================
%% @doc execute sql and fetch result
-spec execute(Connector :: atom(), Sql :: list() | binary(), Method :: term()) -> term().
execute(_Connector, <<>>, _Method) ->
    ok;
execute(_Connector, [], _Method) ->
    ok;
execute(Connector, Sql, _Method) ->
    case volley:get(Connector) of
        {ok, Worker} ->
            %% match self to from, fetch/send_msg will never return ok
            %% result will be {data/updated/error, #mysql_result{}}
            Result = mysql_connector:query(Worker, Sql),
            mysql_connector:handle_result(Sql, Result);
        {error, Reason} ->
            %% interrupt operation
            erlang:throw({pool_error, {Connector, Reason}})
    end.

%% 统计数据表操作次数和频率
-spec statistics(Table :: atom(), Operation :: atom()) -> ok.
statistics(_Table, _Operation) ->
    ok.
%%%===================================================================
%%% database manage tool
%%%===================================================================
%% @doc get initialization auto increment id
-spec id() -> non_neg_integer().
id() ->
    ChannelId = config:channel_id(),
    ServerId = config:server_id(),
    %% bigint 8(byte)/64(bit)
    %% compact plan
    ChannelId * 1000000000000 + ServerId * 1000000000.
    %% ChannelId * 1000000000000000 + ServerId * 1000000000000.
    %% 1001000000000000
    %% 31536000000 = 1000 * 86400 * 365
    %% 1000000000000 / 31536000000 ~= 31.709791983764585
    %% maximize option ChannelId * 1000000000000000000 + ServerId * 1000000000000000.

%% @doc start initialization database
-spec initialize() -> ok.
initialize() ->
    try
        AutoIncrement = id() + 1,
        %% MySQL 8.x need
        %% set information_schema_stats_expiry = 0 in mysql.ini
        catch query("SET @@SESSION.`information_schema_stats_expiry` = 0;"),
        Database = config:mysql_connector_database(),
        %% the AUTO_INCREMENT field after create is null, but insert some data after truncate it is 1
        TableList = select(io_lib:format("SELECT information_schema.`TABLES`.`TABLE_NAME` FROM information_schema.`TABLES` INNER JOIN information_schema.`COLUMNS` ON information_schema.`TABLES`.`TABLE_NAME` = information_schema.`COLUMNS`.`TABLE_NAME` WHERE information_schema.`TABLES`.`AUTO_INCREMENT` IN (1, NULL) AND information_schema.`TABLES`.`TABLE_SCHEMA` = '~s' AND information_schema.`COLUMNS`.`TABLE_SCHEMA` = '~s' AND information_schema.`COLUMNS`.`COLUMN_KEY` = 'PRI' AND information_schema.`COLUMNS`.`EXTRA` = 'auto_increment'", [Database, Database])),
        lists:foreach(fun([Table]) -> set_auto_increment(Table, AutoIncrement) end, TableList)
    catch ?EXCEPTION(_Class, Reason, Stacktrace) ->
        ?STACKTRACE(Reason, ?GET_STACKTRACE(Stacktrace))
    end.

%% @doc get auto increment
-spec get_auto_increment(Table :: atom() | string()) -> non_neg_integer().
get_auto_increment(Table) ->
    select_one(parser:format(<<"SELECT AUTO_INCREMENT FROM information_schema.`TABLES` WHERE `TABLE_SCHEMA` = '~s' AND `TABLE_NAME` = '~s'">>, [config:mysql_connector_database(), Table])).

%% @doc set auto increment
-spec set_auto_increment(Table :: atom() | string(), AutoIncrement :: non_neg_integer()) -> ok.
set_auto_increment(Table, AutoIncrement) ->
    query(io_lib:format("ALTER TABLE `~s` AUTO_INCREMENT = ~w", [Table, AutoIncrement])).

%%%===================================================================
%%% Internal functions
%%%===================================================================
