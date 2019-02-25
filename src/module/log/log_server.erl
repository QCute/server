%%%-------------------------------------------------------------------
%%% @doc
%%% module log server
%%% @end
%%%-------------------------------------------------------------------
-module(log_server).
-behaviour(gen_server).
%% export function
-export([start/0, start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([log/2]).
-include("common.hrl").
%%%===================================================================
%%% API
%%%===================================================================
%% @doc start
start() ->
    process:start(?MODULE).

%% @doc start
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc log
log(Type, Message) ->
    process:cast(?MODULE, {log, Type, Message}).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    {ok, []}.
handle_call(_Request, _From, State) ->
    {reply, ok, State}.
handle_cast({log, Type, Log}, State) ->
    %% cache data
    {noreply, lists:keystore(Type, 1, State, Log)};
handle_cast(_Request, State) ->
    {noreply, State}.
handle_info(timeout, State) ->
    save(State),
    %% save data loop
    {noreply, [], ?MINUTE_SECONDS * 1 * 1000};
handle_info(_Info, State) ->
    {noreply, State}.
terminate(_Reason, State) ->
    %% save data when terminate
    save(State),
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% save all cache data
save(List) ->
    [save(Type, DataList) || {Type, DataList} <- List],
    ok.
save(Type, DataList) ->
    Sql = format(Type, DataList),
    sql:insert(Sql),
    ok.
%% format data and make sql
format(Type, DataList) ->
    {Sql, Format} = log_sql:sql(Type),
    lists:concat([Sql, format(DataList, Format, [])]).
%% format data
format([], _Format, StringDataList) ->
    string:join(lists:reverse(StringDataList), ",");
format([Data | T], Format, StringDataList) ->
    StringData = io_lib:format(Format, Data),
    format(T, Format, [StringData | StringDataList]).

