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
-include("time.hrl").
-include("journal.hrl").
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
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?STACKTRACE(Class, Reason, ?GET_STACKTRACE(Stacktrace)),
        {reply, ok, State}
    end.

%% @doc handle_cast
-spec handle_cast(Request :: term(), State :: list()) -> {noreply, NewState :: list()}.
handle_cast(Request, State) ->
    try
        do_cast(Request, State)
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?STACKTRACE(Class, Reason, ?GET_STACKTRACE(Stacktrace)),
        {noreply, State}
    end.

%% @doc handle_info
-spec handle_info(Request :: term(), State :: list()) -> {noreply, NewState :: list()}.
handle_info(Info, State) ->
    try
        do_info(Info, State)
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?STACKTRACE(Class, Reason, ?GET_STACKTRACE(Stacktrace)),
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
    _ = time:is_cross_day(Before, 4, Now) andalso erlang:send_after(?SECOND_MILLISECONDS(randomness:rand(1, 60)), self(), clean),
    {noreply, []};
do_info(clean, State) ->
    %% clean data
    case config:log_retain_file() of
        false ->
            ok;
        [] ->
            %% delete directly
            clean_loop(db:select(<<"SHOW TABLES LIKE '%_log'">>), [], <<>>, []);
        File ->
            %% delete and dump data
            clean_loop(db:select(<<"SHOW TABLES LIKE '%_log'">>), File, <<>>, [])
    end,
    {noreply, State};
do_info({clean, List, File}, State) ->
    %% clean data
    clean_loop(List, File, <<>>, []),
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
        log_save:save(Type, DataList)
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?STACKTRACE(Class, Reason, ?GET_STACKTRACE(Stacktrace))
    end,
    save_loop(T).

%% clean all expire data
clean_loop([], File, Binary, List) ->
    %% write file
    byte_size(Binary) > 0 andalso file:write_file(File, <<Binary/binary, "\n">>, [append]),
    %% may be remained data, rerun clean after 1~60 second
    List =/= [] andalso erlang:send_after(?SECOND_MILLISECONDS(randomness:rand(1, 60)), self(), {clean, List, File}),
    ok;
clean_loop([[Table] | T], File, <<>>, List) ->
    try
        %% delete data
        case log_delete:delete(binary_to_atom(Table)) of
            Number when Number < 1000 ->
                %% no remain data
                clean_loop(T, File, <<>>, List);
            _ ->
                %% may be remained data
                clean_loop(T, File, <<>>, [[Table] | List])
        end
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?STACKTRACE(Class, Reason, ?GET_STACKTRACE(Stacktrace)),
        clean_loop(T, File, <<>>, List)
    end;
clean_loop([[Table] | T], File, Binary, List) ->
    try
        %% delete and return data
        case log_delete_return:delete_return(binary_to_atom(Table)) of
            [] ->
                %% no clean data
                clean_loop(T, File, Binary, List);
            DataList ->
                %% make sql sentence
                NewBinary = log_replace:replace(binary_to_atom(Table), lists:reverse(DataList)),
                case length(DataList) < 1000 of
                    true ->
                        %% no remained data
                        clean_loop(T, File, <<Binary/binary, "\n", NewBinary/binary>>, List);
                    false ->
                        %% may be remained data
                        clean_loop(T, File, <<Binary/binary, "\n", NewBinary/binary>>, [[Table] | List])
                end
        end
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?STACKTRACE(Class, Reason, ?GET_STACKTRACE(Stacktrace)),
        clean_loop(T, File, Binary, List)
    end.
