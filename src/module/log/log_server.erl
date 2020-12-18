%%%-------------------------------------------------------------------
%%% @doc
%%% log server
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
%%% API functions
%%%===================================================================
%% @doc start
-spec start() -> {ok, pid()} | {error, term()}.
start() ->
    process:start(?MODULE).

%% @doc server start
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc log
-spec log(Type :: atom(), Data :: term()) -> ok.
log(Type, Data) ->
    gen_server:cast(?MODULE, {log, Type, Data}).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
%% @doc init
-spec init(Args :: term()) -> {ok, State :: list()}.
init([]) ->
    process_flag(trap_exit, true),
    %% time loop
    erlang:send_after(?MINUTE_MILLISECONDS, self(), {loop, time:now()}),
    {ok, []}.

%% @doc handle_call
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: list()) -> {reply, Reply :: term(), NewState :: list()}.
handle_call(Request, From, State) ->
    try
        do_call(Request, From, State)
    catch ?EXCEPTION(_Class, Reason, Stacktrace) ->
        ?STACKTRACE(Reason, ?GET_STACKTRACE(Stacktrace)),
        {reply, ok, State}
    end.

%% @doc handle_cast
-spec handle_cast(Request :: term(), State :: list()) -> {noreply, NewState :: list()}.
handle_cast(Request, State) ->
    try
        do_cast(Request, State)
    catch ?EXCEPTION(_Class, Reason, Stacktrace) ->
        ?STACKTRACE(Reason, ?GET_STACKTRACE(Stacktrace)),
        {noreply, State}
    end.

%% @doc handle_info
-spec handle_info(Request :: term(), State :: list()) -> {noreply, NewState :: list()}.
handle_info(Info, State) ->
    try
        do_info(Info, State)
    catch ?EXCEPTION(_Class, Reason, Stacktrace) ->
        ?STACKTRACE(Reason, ?GET_STACKTRACE(Stacktrace)),
        {noreply, State}
    end.

%% @doc terminate
-spec terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()), State :: list()) -> {ok, NewState :: list()}.
terminate(_Reason, State) ->
    %% save data when terminate
    save_loop(State),
    {ok, State}.

%% @doc code_change
-spec code_change(OldVsn :: (term() | {down, term()}), State :: list(), Extra :: term()) -> {ok, NewState :: list()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
do_call(_Request, _From, State) ->
    {reply, ok, State}.

-ifdef(DEBUG).
%% save directly
-define(SAVE(NewList), save_loop(NewList), {noreply, []}).
-else.
%% cache it
-define(SAVE(NewList), {noreply, NewList}).
-endif.

do_cast({log, Type, Data}, State) ->
    %% cache data
    NewList = listing:key_append(Type, State, Data),
    ?SAVE(NewList);
do_cast(_Request, State) ->
    {noreply, State}.

do_info({loop, Before}, State) ->
    Now = time:now(),
    %% next time loop
    erlang:send_after(?MINUTE_MILLISECONDS, self(), {loop, Now}),
    %% save data
    save_loop(State),
    %% clean log at morning 4 every day
    _ = time:is_cross_day(Before, 4, Now) andalso clean() == ok,
    {noreply, []};
do_info({clean, List}, State) ->
    %% clean data
    clean_loop(List, config:log_retain_file(), []),
    %% garbage collect
    erlang:garbage_collect(self()),
    {noreply, State};
do_info(_Info, State) ->
    {noreply, State}.

%% save all cache data
save_loop([]) ->
    ok;
save_loop([{Type, DataList} | T]) ->
    try
        %% save data
        db:insert(parser:collect(lists:reverse(DataList), log_sql_save:sql(Type)))
    catch ?EXCEPTION(_Class, Reason, Stacktrace) ->
        ?STACKTRACE(Reason, ?GET_STACKTRACE(Stacktrace))
    end,
    save_loop(T).

%% clean
clean() ->
    case log_sql_retain:sql() of
        [] ->
            %% clean
            clean_loop(log_sql_clean:sql(), [], []);
        File ->
            %% clean and retain
            clean_loop(log_sql_retain:sql(), File, [])
    end.

clean_loop([], _, []) ->
    ok;
clean_loop([], _, List) ->
    %% may be remained data, rerun clean after 1~60 second
    erlang:send_after(?MINUTE_MILLISECONDS(randomness:rand(1, 60)), self(), {clean, List}),
    ok;
clean_loop([H = {Sql, ExpireTime} | T], [], List) ->
    try
        %% clean data
        case db:delete(parser:format(Sql, [time:zero() - ExpireTime])) of
            Number when Number < 1000 ->
                %% no clean data
                clean_loop(T, [], List);
            _ ->
                %% may be remained data
                clean_loop(T, [], [H | List])
        end
    catch ?EXCEPTION(_Class, Reason, Stacktrace) ->
        ?STACKTRACE(Reason, ?GET_STACKTRACE(Stacktrace)),
        clean_loop(T, [], List)
    end;
clean_loop([H = {SelectSql, ReplaceSql, DeleteSql, ExpireTime} | T], File, List) ->
    try
        %% query data
        case db:select(parser:format(SelectSql, [time:zero() - ExpireTime])) of
            [] ->
                %% no clean data
                clean_loop(T, File, List);
            DataList ->
                %% make sql sentence
                Binary = parser:collect(lists:reverse(DataList), ReplaceSql),
                %% save to file
                file:write_file(File, <<Binary/binary, "\n">>, [append]),
                %% clean data auto_increment in first field
                db:delete(parser:collect([[No] || [No | _] <- DataList], DeleteSql)),
                %% may be remained data
                clean_loop(T, File, [H | List])
        end
    catch ?EXCEPTION(_Class, Reason, Stacktrace) ->
        ?STACKTRACE(Reason, ?GET_STACKTRACE(Stacktrace)),
        clean_loop(T, File, List)
    end.
