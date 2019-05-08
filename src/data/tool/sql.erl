%%%-------------------------------------------------------------------
%%% @doc
%%% module sql execute tool
%%% @end
%%%-------------------------------------------------------------------
-module(sql).
%% API
-export([select_one/1, select_row/1]).
-export([select_one/3, select_row/3]).
-export([select/1, insert/1, update/1, delete/1]).
-export([select/3, insert/3, update/3, delete/3]).
%% Includes
-include("common.hrl").
%% Macros
-define(MYSQL_DRIVER,                                 mysql_driver).
%% ====================================================================
%% API functions
%% ====================================================================
%% @doc select one
-spec select_one(Sql :: iolist()) -> term().
select_one(Sql) ->
    select_one(?MYSQL_DRIVER, table, Sql).
-spec select_one(Driver :: atom(), Table :: atom(), Sql :: iolist()) -> term().
select_one(Driver, Table, Sql) ->
    case select(Driver, Table, Sql) of
        [[One]] ->
            One;
        _ ->
            []
    end.

%% @doc select row
-spec select_row(Sql :: iolist()) -> term().
select_row(Sql) ->
    select_row(?MYSQL_DRIVER, table, Sql).
-spec select_row(Driver :: atom(), Table :: atom(), Sql :: iolist()) -> term().
select_row(Driver, Table, Sql) ->
    case select(Driver, Table, Sql) of
        [Row] ->
            Row;
        _ ->
            []
    end.

%% @doc select row
-spec select(Sql :: iolist()) -> term().
select(Sql) ->
    select(?MYSQL_DRIVER, table, Sql).
-spec select(Driver :: atom(), Table :: atom(), Sql :: iolist()) -> term().
select(Driver, Table, Sql) ->
    statistics(Table, select),
    execute(Driver, Sql).

%% @doc insert
-spec insert(Sql :: iolist()) -> term().
insert(Sql) ->
    insert(?MYSQL_DRIVER, table, Sql).
-spec insert(Driver :: atom(), Table :: atom(), Sql :: iolist()) -> term().
insert(Driver, Table, Sql) ->
    statistics(Table, insert),
    execute(Driver, Sql, insert).

%% @doc update
-spec update(Sql :: iolist()) -> term().
update(Sql) ->
    update(?MYSQL_DRIVER, table, Sql).
-spec update(Driver :: atom(), Table :: atom(), Sql :: iolist()) -> term().
update(Driver, Table, Sql) ->
    statistics(Table, update),
    execute(Driver, Sql).

%% @doc delete
-spec delete(Sql :: iolist()) -> term().
delete(Sql) ->
    delete(?MYSQL_DRIVER, table, Sql).
-spec delete(Driver :: atom(), Table :: atom(), Sql :: iolist()) -> term().
delete(Driver, Table, Sql) ->
    statistics(Table, delete),
    execute(Driver, Sql).

%% ====================================================================
%% Internal functions
%% ====================================================================
%% execute sql directly
-spec execute(Driver :: atom(), Sql :: iolist()) -> term().
execute(_Driver, <<>>) ->
    ok;
execute(_Driver, []) ->
    ok;
execute(Driver, Sql) ->
    execute(Driver, Sql, []).

-spec execute(Driver :: atom(), Sql :: iolist(), Args :: term()) -> term().
execute(_Driver, <<>>, _Args) ->
    ok;
execute(_Driver, [], _Args) ->
    ok;
execute(Driver, Sql, Args) ->
    case poolboy:checkout(Driver) of
        {ok, Worker} ->
            %% match self to from, fetch/send_msg will never return ok
            %% result will be {data/updated/error, #mysql_result{}}
            Result = mysql_driver:fetch(Worker, [Sql]),
            %% return checkout worker
            poolboy:checkin(Driver, Worker),
            %% handle mysql result
            mysql_driver:handle_result(Sql, Args, Result);
        {error, full} ->
            %% interrupt operation
            erlang:throw({pool_error, {Driver, full}})
    end.

%% 统计数据表操作次数和频率
-spec statistics(Table :: atom(), Operation :: atom()) -> ok.
statistics(_Table, _Operation) ->
    ok.
