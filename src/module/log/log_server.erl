%%%------------------------------------------------------------------
%%% @doc
%%% module log server
%%% @end
%%%------------------------------------------------------------------
-module(log_server).
-behaviour(gen_server).
%% API
-export([log/2]).
-export([start/0, start_link/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% Includes
-include("common.hrl").
%%%==================================================================
%%% API functions
%%%==================================================================
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
    gen_server:cast(?MODULE, {log, Type, Data}).
%%%==================================================================
%%% gen_server callbacks
%%%==================================================================
init([]) ->
    process_flag(trap_exit, true),
    %% next time loop
    erlang:send_after(?MINUTE_SECONDS * 1000, self(), loop),
    {ok, []}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({log, Type, Data}, State) ->
    %% cache data
    NewList = listing:key_append(Type, State, Data),
    {noreply, NewList};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(loop, State) ->
    %% next time loop
    erlang:send_after(?MINUTE_SECONDS * 1000, self(), loop),
    %% save data
    save_loop(State),
    %% clean log at morning 4 every day
    Now = time:ts(),
    case time:cross(day, 4, Now - ?MINUTE_SECONDS, Now) of
        true ->
            %% clean data
            clean(log_sql_clean:sql());
        false ->
            skip
    end,
    {noreply, []};
handle_info({clean, List}, State) ->
    %% clean data
    clean(List),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    %% save data when terminate
    save_loop(State),
    {ok, []}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%==================================================================
%%% Internal functions
%%%==================================================================
%% save all cache data
save_loop([]) ->
    ok;
save_loop([{Type, DataList} | T]) ->
    try
        %% save data
        sql:insert(parser:collect(lists:reverse(DataList), log_sql:sql(Type)))
    catch ?EXCEPTION(_Class, Reason, Stacktrace) ->
        ?STACKTRACE(Reason, ?GET_STACKTRACE(Stacktrace))
    end,
    save_loop(T).

%% clean all expire data
clean(List) ->
    clean_loop(List, []).

clean_loop([], []) ->
    ok;
clean_loop([], List) ->
    %% may be remain data, rerun clean after 1~60 second
    erlang:send_after(randomness:rand(1, 60) * 1000, self(), {clean, List}),
    ok;
clean_loop([{Sql, ExpireTime} | T], List) ->
    try
        %% clean data
        case sql:delete(parser:format(Sql, [time:zero() - ExpireTime])) of
            Number when Number =< 1000 ->
                %% no clean data
                clean_loop(T, List);
            _ ->
                %% may be remain data
                clean_loop(T, [{Sql, ExpireTime} | List])
        end
    catch ?EXCEPTION(_Class, Reason, Stacktrace) ->
        ?STACKTRACE(Reason, ?GET_STACKTRACE(Stacktrace)),
        clean_loop(T, List)
    end.
