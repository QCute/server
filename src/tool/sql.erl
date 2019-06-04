%%%-------------------------------------------------------------------
%%% @doc
%%% module sql execute tool
%%% @end
%%%-------------------------------------------------------------------
-module(sql).
%% API
-export([version/0]).
-export([select_one/1, select_row/1]).
-export([select_one/3, select_row/3]).
-export([select/1, insert/1, update/1, delete/1]).
-export([select/3, insert/3, update/3, delete/3]).
%% Macros
-define(MYSQL_CONNECTOR,                                 mysql_connector).
%% ====================================================================
%% API functions
%% ====================================================================
%% @doc version
-spec version() -> binary().
version() ->
    select_one("SELECT VERSION()").

%% @doc select one
-spec select_one(Sql :: iolist()) -> term().
select_one(Sql) ->
    select_one(?MYSQL_CONNECTOR, table, Sql).
-spec select_one(Connector :: atom(), Table :: atom(), Sql :: iolist()) -> term().
select_one(Connector, Table, Sql) ->
    case select(Connector, Table, Sql) of
        [[One]] ->
            One;
        _ ->
            []
    end.

%% @doc select row
-spec select_row(Sql :: iolist()) -> term().
select_row(Sql) ->
    select_row(?MYSQL_CONNECTOR, table, Sql).
-spec select_row(Connector :: atom(), Table :: atom(), Sql :: iolist()) -> term().
select_row(Connector, Table, Sql) ->
    case select(Connector, Table, Sql) of
        [Row] ->
            Row;
        _ ->
            []
    end.

%% @doc select row
-spec select(Sql :: iolist()) -> term().
select(Sql) ->
    select(?MYSQL_CONNECTOR, table, Sql).
-spec select(Connector :: atom(), Table :: atom(), Sql :: iolist()) -> term().
select(Connector, Table, Sql) ->
    statistics(Table, select),
    execute(Connector, Sql).

%% @doc insert
-spec insert(Sql :: iolist()) -> term().
insert(Sql) ->
    insert(?MYSQL_CONNECTOR, table, Sql).
-spec insert(Connector :: atom(), Table :: atom(), Sql :: iolist()) -> term().
insert(Connector, Table, Sql) ->
    statistics(Table, insert),
    execute(Connector, Sql, insert).

%% @doc update
-spec update(Sql :: iolist()) -> term().
update(Sql) ->
    update(?MYSQL_CONNECTOR, table, Sql).
-spec update(Connector :: atom(), Table :: atom(), Sql :: iolist()) -> term().
update(Connector, Table, Sql) ->
    statistics(Table, update),
    execute(Connector, Sql).

%% @doc delete
-spec delete(Sql :: iolist()) -> term().
delete(Sql) ->
    delete(?MYSQL_CONNECTOR, table, Sql).
-spec delete(Connector :: atom(), Table :: atom(), Sql :: iolist()) -> term().
delete(Connector, Table, Sql) ->
    statistics(Table, delete),
    execute(Connector, Sql).

%% ====================================================================
%% Internal functions
%% ====================================================================
%% execute sql directly
-spec execute(Connector :: atom(), Sql :: iolist()) -> term().
execute(_Connector, <<>>) ->
    ok;
execute(_Connector, []) ->
    ok;
execute(Connector, Sql) ->
    execute(Connector, Sql, []).

-spec execute(Connector :: atom(), Sql :: iolist(), Method :: term()) -> term().
execute(_Connector, <<>>, _Method) ->
    ok;
execute(_Connector, [], _Method) ->
    ok;
execute(Connector, Sql, Method) ->
    case volley:get(Connector) of
        {ok, Worker} ->
            %% match self to from, fetch/send_msg will never return ok
            %% result will be {data/updated/error, #mysql_result{}}
            Result = mysql_connector:query(Worker, Sql, Method),
            mysql_connector:handle_result(Sql, Method, Result);
        {error, Reason} ->
            %% interrupt operation
            erlang:throw({pool_error, {Connector, Reason}})
    end.

%% 统计数据表操作次数和频率
-spec statistics(Table :: atom(), Operation :: atom()) -> ok.
statistics(_Table, _Operation) ->
    ok.
