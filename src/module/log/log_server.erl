%%%-------------------------------------------------------------------
%%% @doc
%%% module log server
%%% @end
%%%-------------------------------------------------------------------
-module(log_server).
-behaviour(gen_server).
%% API
-export([log/2]).
-export([start/0, start_link/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% Includes
-include("common.hrl").
%%%===================================================================
%%% API
%%%===================================================================
%% @doc start
-spec start() -> {ok, Pid :: pid()} | {error, term()}.
start() ->
    process:start(?MODULE).

%% @doc server start
-spec start_link() -> {ok, Pid :: pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc log
-spec log(Type :: atom(), Data :: term()) -> ok.
log(Type, Data) ->
    process:cast(?MODULE, {log, Type, Data}).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    {ok, []}.
handle_call(_Request, _From, State) ->
    {reply, ok, State}.
handle_cast({log, Type, Data}, State) ->
    %% cache data
    {_, List} = listing:key_find(Type, 1, State, {Type, []}),
    {noreply, lists:keystore(Type, 1, State, {Type, [Data | List]})};
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
    [save_one(Type, DataList) || {Type, DataList} <- List],
    ok.

save_one(Type, DataList) ->
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

