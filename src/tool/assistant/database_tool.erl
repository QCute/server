%%%-------------------------------------------------------------------
%%% @doc
%%% module database tool
%%% initialize auto increment id
%%% truncate all table
%%% @end
%%%-------------------------------------------------------------------
-module(database_tool).
%% API
-export([initialize/0]).
-export([truncate/0]).
%% Includes
-include("common.hrl").
%%%===================================================================
%%% API
%%%===================================================================
%% @doc get initialization auto increment id
-spec id() -> non_neg_integer().
id() ->
    ChannelId = config:channel_id(),
    ServerId = config:server_id(),
    %% bigint 8(byte)/64(bit)
    ChannelId * 1000000000000000000 + ServerId * 1000000000000001.

%% @doc truncate all tables
-spec truncate() -> ok.
truncate() ->
    try
        Database = config:mysql_connector_database(),
        Sql = io_lib:format("SELECT CONCAT('TRUNCATE TABLE `', information_schema.`TABLES`.`TABLE_NAME`, '`;') FROM information_schema.`TABLES` WHERE information_schema.`TABLES`.`TABLE_SCHEMA` IN ('~s')", [Database]),
        TableList = sql:select(Sql),
        [sql:query(Truncate) || [Truncate] <- TableList],
        ok
    catch ?EXCEPTION(_Class, Reason, Stacktrace) ->
        ?STACKTRACE(Reason, ?GET_STACKTRACE(Stacktrace))
    end.

%% @doc start initialization database
-spec initialize() -> ok.
initialize() ->
    try
        Id = id(),
        set_stat_expiry(),
        Database = config:mysql_connector_database(),
        TableList = collect_auto_increment_table(Database),
        [alter_auto_increment(Id, Database, Table) || [Table] <- TableList],
        ok
    catch ?EXCEPTION(_Class, Reason, Stacktrace) ->
        ?STACKTRACE(Reason, ?GET_STACKTRACE(Stacktrace))
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================
%% sync table stat real time
set_stat_expiry() ->
    %% 8.x need
    %% set information_schema_stats_expiry = 0 in mysql.ini
    catch sql:query("SET @@SESSION.`information_schema_stats_expiry` = 0;").

%% collect all contain auto increment but no data tables
collect_auto_increment_table(Database) ->
    %% AUTO_INCREMENT after create is null
    %% AUTO_INCREMENT after insert some data and truncate it is 1
    Sql = io_lib:format("
	SELECT
		information_schema.`TABLES`.`TABLE_NAME`
	FROM
		information_schema.`TABLES`
	INNER JOIN
	  information_schema.`COLUMNS`
	ON
	  information_schema.`TABLES`.`TABLE_NAME` = information_schema.`COLUMNS`.`TABLE_NAME`
	WHERE
		information_schema.`TABLES`.`AUTO_INCREMENT` IN (1, null)
		AND
		information_schema.`TABLES`.`TABLE_SCHEMA` = '~s'
		AND
		information_schema.`COLUMNS`.`TABLE_SCHEMA` = '~s'
		AND
		information_schema.`COLUMNS`.`COLUMN_KEY` = 'PRI'
	    AND
		information_schema.`COLUMNS`.`EXTRA` = 'auto_increment'
	", [Database, Database]),
    sql:select(Sql).

%% alter table auto increment value
alter_auto_increment(Id, Database, Table) ->
    Sql = io_lib:format("ALTER TABLE ~s.`~s` AUTO_INCREMENT = ~w", [Database, Table, Id]),
    sql:query(Sql).
