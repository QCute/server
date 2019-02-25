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
%% includes
-include("common.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
%% @doc select one
-spec select_one(Sql :: string()) -> term().
select_one(Sql) ->
    select_one(?POOL, table, Sql).
-spec select_one(PoolId :: atom(), Table :: atom(), Sql :: string()) -> term().
select_one(PoolId, Table, Sql) ->
    case select(PoolId, Table, Sql) of
        [[One]] ->
            One;
        _ ->
            []
    end.

%% @doc select row
-spec select_row(Sql :: string()) -> term().
select_row(Sql) ->
    select_row(?POOL, table, Sql).
-spec select_row(PoolId :: atom(), Table :: atom(), Sql :: string()) -> term().
select_row(PoolId, Table, Sql) ->
    case select(PoolId, Table, Sql) of
        [Row] ->
            Row;
        _ ->
            []
    end.

%% @doc select row
-spec select(Sql :: string()) -> term().
select(Sql) ->
    select(?POOL, table, Sql).
-spec select(PoolId :: atom(), Table :: atom(), Sql :: string()) -> term().
select(PoolId, Table, Sql) ->
    statistics(Table, select),
    execute(PoolId, Sql).

%% @doc insert
-spec insert(Sql :: string()) -> term().
insert(Sql) ->
    insert(?POOL, table, Sql).
-spec insert(PoolId :: atom(), Table :: atom(), Sql :: string()) -> term().
insert(PoolId, Table, Sql) ->
    statistics(Table, insert),
    execute(PoolId, Sql, insert).

%% @doc update
-spec update(Sql :: string()) -> term().
update(Sql) ->
    update(?POOL, table, Sql).
-spec update(PoolId :: atom(), Table :: atom(), Sql :: string()) -> term().
update(PoolId, Table, Sql) ->
    statistics(Table, update),
    execute(PoolId, Sql).

%% @doc delete
-spec delete(Sql :: string()) -> term().
delete(Sql) ->
    delete(?POOL, table, Sql).
-spec delete(PoolId :: atom(), Table :: atom(), Sql :: string()) -> term().
delete(PoolId, Table, Sql) ->
    statistics(Table, delete),
    execute(PoolId, Sql).

%% ====================================================================
%% Internal functions
%% ====================================================================
%% execute sql directly
-spec execute(PoolId :: atom(), Sql :: string()) -> term().
execute(_PoolId, []) ->
    ok;
execute(PoolId, Sql) ->
    execute(PoolId, Sql, []).
-spec execute(PoolId :: atom(), Sql :: string(), Args :: term()) -> term().
execute(PoolId, Sql, Args) ->
    case poolboy:checkout(PoolId) of
        {ok, Worker} ->
            %% match self to from, fetch/send_msg will never return ok
            %% result will be {data/updated/error, #mysql_result{}}
            Result = mysql_conn:fetch(Worker, [Sql], self()),
            %% return checkout worker
            poolboy:checkin(PoolId, Worker),
            %% handle mysql result
            handle_result(Sql, Args, Result);
        {error, full} ->
            %% interrupt operation
            erlang:throw({pool_error, {PoolId, full}})
    end.

-spec handle_result(Sql :: string(), Args :: term(), Result :: term()) -> term().
handle_result(_, _, {data, Result}) ->
    mysql:get_result_rows(Result);
handle_result(_, [], {updated, _Result}) ->
    ok;
handle_result(_, insert, {updated, Result}) ->
    mysql:get_result_insert_id(Result);
handle_result(_, _, {updated, Result}) ->
    mysql:get_result_affected_rows(Result);
handle_result(Sql, _, {error, Result}) ->
    ErrorCode = mysql:get_result_err_code(Result),
    Reason = mysql:get_result_reason(Result),
    %% format exit stack trace info
    erlang:throw({sql_error, {Sql, ErrorCode, Reason}}).


%% 统计数据表操作次数和频率
-spec statistics(Table :: atom(), Operation :: atom()) -> ok.
statistics(_Table, _Operation) ->
    ok.
