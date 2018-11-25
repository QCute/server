%% Poolboy - A hunky Erlang worker pool factory

-module(poolboy).
-behaviour(gen_server).

-export([checkout/1, checkout/2, checkout/3, checkout/4, checkin/2, transaction/2,
    transaction/3, child_spec/2, child_spec/3, start/3, start/2,
    start_link/3, start_link/2, stop/1, status/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).
-export_type([pool/0]).

-define(TIMEOUT, 5000).

-type pid_queue() :: queue:queue().


-type pool() ::
Name :: (atom() | pid()) |
{Name :: atom(), node()} |
{local, Name :: atom()} |
{global, GlobalName :: any()} |
{via, Module :: atom(), ViaName :: any()}.

% Copied from gen:start_ret/0
-type start_ret() :: {'ok', pid()} | 'ignore' | {'error', term()}.

-record(state, {
    supervisor :: pid(),
    workers :: [pid()],
    waiting :: pid_queue(),
    monitors :: ets:tid(),
    size = 5 :: non_neg_integer(),
    overflow = 0 :: non_neg_integer(),
    max_overflow = 10 :: non_neg_integer(),
    strategy = lifo :: lifo | fifo,
    prepares = gb_trees:empty(),
    work_args = []
}).

-spec checkout(Pool :: pool()) -> pid().
checkout(Pool) ->
    checkout(Pool, true).

-spec checkout(Pool :: pool(), Block :: boolean()) -> pid() | full.
checkout(Pool, Block) ->
    checkout(Pool, Block, ?TIMEOUT).

-spec checkout(Pool :: pool(), Block :: boolean(), Timeout :: timeout()) -> pid() | full.
checkout(Pool, Block, Timeout) ->
    CRef = make_ref(),
    try
        gen_server:call(Pool, {checkout, CRef, Block}, Timeout)
    catch
        Class:Reason ->
            gen_server:cast(Pool, {cancel_waiting, CRef}),
            erlang:raise(Class, Reason, erlang:get_stacktrace())
    end.
-spec checkout(Pool :: pool(), Name :: term(), Block :: boolean(), Timeout :: timeout())
        -> pid() | full.
checkout(Pool, Name, Block, Timeout) ->
    CRef = make_ref(),
    try
        gen_server:call(Pool, {checkout_prepare, Name, CRef, Block}, Timeout)
    catch
        Class:Reason ->
            gen_server:cast(Pool, {cancel_waiting, CRef}),
            erlang:raise(Class, Reason, erlang:get_stacktrace())
    end.


-spec checkin(Pool :: pool(), Worker :: pid()) -> ok.
checkin(Pool, Worker) when is_pid(Worker) ->
    gen_server:cast(Pool, {checkin, Worker}).

-spec transaction(Pool :: pool(), Fun :: fun((Worker :: pid()) -> any()))
        -> any().
transaction(Pool, Fun) ->
    transaction(Pool, Fun, ?TIMEOUT).

-spec transaction(Pool :: pool(), Fun :: fun((Worker :: pid()) -> any()),
    Timeout :: timeout()) -> any().
transaction(Pool, Fun, Timeout) ->
    Worker = poolboy:checkout(Pool, true, Timeout),
    try
        Fun(Worker)
    after
        ok = poolboy:checkin(Pool, Worker)
    end.

-spec child_spec(PoolId :: term(), PoolArgs :: proplists:proplist())
        -> supervisor:child_spec().
child_spec(PoolId, PoolArgs) ->
    child_spec(PoolId, PoolArgs, []).

-spec child_spec(PoolId :: term(),
    PoolArgs :: proplists:proplist(),
    WorkerArgs :: proplists:proplist())
        -> supervisor:child_spec().
child_spec(PoolId, PoolArgs, WorkerArgs) ->
    {PoolId, {poolboy, start_link, [PoolArgs, WorkerArgs]},
        permanent, 5000, worker, [poolboy]}.

-spec start(PoolId :: term(), PoolArgs :: proplists:proplist())
        -> start_ret().
start(PoolId, PoolArgs) ->
    start(PoolId, PoolArgs, PoolArgs).

-spec start(PoolId :: term(), PoolArgs :: proplists:proplist(),
    WorkerArgs:: proplists:proplist())
        -> start_ret().
start(PoolId, PoolArgs, WorkerArgs) ->
    start_pool(start, PoolId, PoolArgs, WorkerArgs).

-spec start_link(PoolID :: term(), PoolArgs :: proplists:proplist())
        -> start_ret().
start_link(PoolID, PoolArgs)  ->
    %% for backwards compatibility, pass the pool args as the worker args as well
    start_link(PoolID, PoolArgs, PoolArgs).

-spec start_link(PoolID :: term(), PoolArgs :: proplists:proplist(), WorkerArgs:: proplists:proplist())
        -> start_ret().
start_link(PoolID, PoolArgs, WorkerArgs)  ->
    start_pool(start_link, PoolID, PoolArgs, WorkerArgs).

-spec stop(Pool :: pool()) -> ok.
stop(Pool) ->
    gen_server:call(Pool, stop).

-spec status(Pool :: pool()) -> {atom(), integer(), integer(), integer()}.
status(Pool) ->
    gen_server:call(Pool, status).

init({PoolArgs, WorkerArgs}) ->
    process_flag(trap_exit, true),
    Waiting = queue:new(),
    Monitors = ets:new(monitors, [private]),
    init(PoolArgs, WorkerArgs, #state{waiting = Waiting, monitors = Monitors}).

init([{worker_module, Mod} | Rest], WorkerArgs, State) when is_atom(Mod) ->
    {ok, Sup} = poolboy_sup:start_link(Mod, WorkerArgs),
    init(Rest, WorkerArgs, State#state{supervisor = Sup, work_args = WorkerArgs});
init([{size, Size} | Rest], WorkerArgs, State) when is_integer(Size) ->
    init(Rest, WorkerArgs, State#state{size = Size});
init([{max_overflow, MaxOverflow} | Rest], WorkerArgs, State) when is_integer(MaxOverflow) ->
    init(Rest, WorkerArgs, State#state{max_overflow = MaxOverflow});
init([{strategy, lifo} | Rest], WorkerArgs, State) ->
    init(Rest, WorkerArgs, State#state{strategy = lifo});
init([{strategy, fifo} | Rest], WorkerArgs, State) ->
    init(Rest, WorkerArgs, State#state{strategy = fifo});
init([_ | Rest], WorkerArgs, State) ->
    init(Rest, WorkerArgs, State);
init([], _WorkerArgs, #state{size = Size, supervisor = Sup} = State) ->
    Workers = pre_populate(Size, Sup),
    {ok, State#state{workers = Workers}}.

handle_cast({checkin, Pid}, State = #state{monitors = Monitors}) ->
    case ets:lookup(Monitors, Pid) of
        [{Pid, _, MRef}] ->
            true = erlang:demonitor(MRef),
            true = ets:delete(Monitors, Pid),
            NewState = handle_checkin(Pid, State),
            {noreply, NewState};
        [] ->
            {noreply, State}
    end;

%%====================自己添加的代码============================begin
handle_cast({prepare, Name, Stmt}, State) ->
    Version1 =
        case gb_trees:lookup(Name, State#state.prepares) of
            {value, {_Stmt, Version}} ->
                Version + 1;
            none ->
                1
        end,
    {noreply, State#state{prepares =
    gb_trees:enter(Name, {Stmt, Version1},
        State#state.prepares)}};

handle_cast({unprepare, Name}, State) ->
    State1 =
        case gb_trees:lookup(Name, State#state.prepares) of
            none ->
                State;
            {value, _Stmt} ->
                State#state{prepares =
                gb_trees:delete(Name, State#state.prepares)}
        end,
    {noreply, State1};



%%====================自己添加的代码============================end

handle_cast({cancel_waiting, CRef}, State) ->
    case ets:match(State#state.monitors, {'$1', CRef, '$2'}) of
        [[Pid, MRef]] ->
            demonitor(MRef, [flush]),
            true = ets:delete(State#state.monitors, Pid),
            NewState = handle_checkin(Pid, State),
            {noreply, NewState};
        [] ->
            Cancel = fun({_, Ref, MRef}) when Ref =:= CRef ->
                demonitor(MRef, [flush]),
                false;
                (_) ->
                    true
            end,
            Waiting = queue:filter(Cancel, State#state.waiting),
            {noreply, State#state{waiting = Waiting}}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call({checkout, CRef, Block}, {FromPid, _} = From, State) ->
    #state{supervisor = Sup,
        workers = Workers,
        monitors = Monitors,
        overflow = Overflow,
        max_overflow = MaxOverflow} = State,
    case Workers of
        [Pid | Left] ->
            MRef = erlang:monitor(process, FromPid),
            true = ets:insert(Monitors, {Pid, CRef, MRef}),
            {reply, {ok, Pid}, State#state{workers = Left}};
        [] when MaxOverflow > 0, Overflow < MaxOverflow ->
            case try_start_new_overload_work(Sup, FromPid) of
                {ok, Pid, MRef} ->
                    true = ets:insert(Monitors, {Pid, CRef, MRef}),
                    {reply, {ok, Pid}, State#state{overflow = Overflow + 1}};
                _ ->
                    {reply, {error, full}, State}
            end;
        [] when Block =:= false ->
            {reply, {error, full}, State};
        [] ->
            MRef = erlang:monitor(process, FromPid),
            Waiting = queue:in({From, CRef, MRef}, State#state.waiting),
            {noreply, State#state{waiting = Waiting}}
    end;

%%============================自己加的代码==========================begin
handle_call({checkout_prepare, Name, CRef, _Block}, {FromPid, _} = _From, State) ->
    case gb_trees:lookup(Name, State#state.prepares) of
        none ->
            {reply, {error, {no_such_statement, Name}}, State};
        {value, {_Stmt, Version}} ->
            #state{supervisor = Sup,
                workers = Workers,
                monitors = Monitors,
                overflow = Overflow,
                max_overflow = MaxOverflow} = State,
            case Workers of
                [Pid | Left] ->
                    MRef = erlang:monitor(process, FromPid),
                    true = ets:insert(Monitors, {Pid, CRef, MRef}),
                    {reply, {ok, Pid, Version}, State#state{workers = Left}};
                [] when MaxOverflow > 0, Overflow < MaxOverflow ->
                    case try_start_new_overload_work(Sup, FromPid) of
                        {ok, Pid, MRef} ->
                            true = ets:insert(Monitors, {Pid, CRef, MRef}),
                            {reply, {ok, Pid, Version}, State#state{overflow = Overflow + 1}};
                        _ -> %%启动失败
                            {reply, {error, full}, State}
                    end;
                []  ->
                    {reply, {error, full}, State}
%%                 [] -> 为了让退出等待的代码不去判断是否有预执行安全查询不设置等待
%%                     MRef = erlang:monitor(process, FromPid),
%%                     Waiting = queue:in({From, CRef, MRef}, State#state.waiting),
%%                     {noreply, State#state{waiting = Waiting}}
            end;
        _ ->
            {reply, {error, {no_such_statement, Name}}, State}
    end;

handle_call({get_prepared, Name, Version}, _From, State) ->
    case gb_trees:lookup(Name, State#state.prepares) of
        none ->
            {reply, {error, {undefined, Name}}, State};
        {value, {_StmtBin, Version1}} when Version1 == Version ->
            {reply, {ok, latest}, State};
        {value, Stmt} ->
            {reply, {ok, Stmt}, State}
    end;

%%============================自己加的代码==========================end

handle_call(status, _From, State) ->
    #state{workers = Workers,
        monitors = Monitors,
        overflow = Overflow} = State,
    StateName = state_name(State),
    {reply, {StateName, length(Workers), Overflow, ets:info(Monitors, size)}, State};
handle_call(get_avail_workers, _From, State) ->
    Workers = State#state.workers,
    {reply, Workers, State};
handle_call(get_all_workers, _From, State) ->
    Sup = State#state.supervisor,
    WorkerList = supervisor:which_children(Sup),
    {reply, WorkerList, State};
handle_call(get_all_monitors, _From, State) ->
    Monitors = ets:select(State#state.monitors,
        [{{'$1', '_', '$2'}, [], [{{'$1', '$2'}}]}]),
    {reply, Monitors, State};



handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Msg, _From, State) ->
    Reply = {error, invalid_message},
    {reply, Reply, State}.

handle_info({'DOWN', MRef, _, _, _}, State) ->
    case ets:match(State#state.monitors, {'$1', '_', MRef}) of
        [[Pid]] ->
            true = ets:delete(State#state.monitors, Pid),
            NewState = handle_checkin(Pid, State),
            {noreply, NewState};
        [] ->
            Waiting = queue:filter(fun ({_, _, R}) -> R =/= MRef end, State#state.waiting),
            {noreply, State#state{waiting = Waiting}}
    end;
handle_info({'EXIT', Pid, _Reason}, State) ->
    #state{supervisor = Sup,
        monitors = Monitors} = State,
    case ets:lookup(Monitors, Pid) of
        [{Pid, _, MRef}] ->
            true = erlang:demonitor(MRef),
            true = ets:delete(Monitors, Pid),
            NewState = handle_worker_exit(Pid, State),
            {noreply, NewState};
        [] ->
            case lists:member(Pid, State#state.workers) of
                true ->
                    W = lists:filter(fun (P) -> P =/= Pid end, State#state.workers),
                    NewWorks = try_start_new_work(W, Sup, 1, 5000),
                    {noreply, State#state{workers = NewWorks}};
                false ->
                    {noreply, State}
            end
    end;

%%循环重新空闲情况
handle_info({start_loop_reconnect, Times ,Time}, State) ->
    #state{supervisor = Sup, workers = W} = State,
    NewWorks = try_start_new_work(W, Sup, Times ,Time),
    {noreply, State#state{workers = NewWorks}};

%%重新连接当有队列的情况
handle_info({start_wating_loop_reconnect, Times ,Time}, State) ->
    #state{supervisor = Sup,
        overflow = Overflow} = State,
    case queue:out(State#state.waiting) of
        {{value, {From, CRef, MRef}}, LeftWaiting} ->
            {ok, NewState} = try_start_new_work_waiting(State, Times ,Time, From, CRef, MRef, LeftWaiting),
            NewState;
        {empty, Empty} when Overflow > 0 ->
            State#state{overflow = Overflow - 1, waiting = Empty};
        {empty, Empty} ->
            Workers = try_start_new_work(State#state.workers, Sup, Times, Time),
            State#state{workers = Workers, waiting = Empty}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    ok = lists:foreach(fun (W) -> unlink(W) end, State#state.workers),
    true = exit(State#state.supervisor, shutdown),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

start_pool(StartFun, PoolID, PoolArgs, WorkerArgs) ->
    case proplists:get_value(name, PoolArgs) of
        undefined ->
            gen_server:StartFun({local, PoolID}, ?MODULE, {PoolArgs, WorkerArgs}, []);
        Name ->
            gen_server:StartFun(Name, ?MODULE, {PoolArgs, WorkerArgs}, [])
    end.

new_worker(Sup) ->
    {ok, Pid} = supervisor:start_child(Sup, []),
    true = link(Pid),
    Pid.

new_worker(Sup, FromPid) ->
    Pid = new_worker(Sup),
    Ref = erlang:monitor(process, FromPid),
    {Pid, Ref}.

dismiss_worker(Sup, Pid) ->
    true = unlink(Pid),
    supervisor:terminate_child(Sup, Pid).

pre_populate(N, _Sup) when N < 1 ->
    [];
pre_populate(N, Sup) ->
    pre_populate(N, Sup, []).

pre_populate(0, _Sup, Workers) ->
    Workers;
pre_populate(N, Sup, Workers) ->
    pre_populate(N-1, Sup, [new_worker(Sup) | Workers]).

handle_checkin(Pid, State) ->
    #state{supervisor = Sup,
        waiting = Waiting,
        monitors = Monitors,
        overflow = Overflow,
        strategy = Strategy} = State,
    case queue:out(Waiting) of
        {{value, {From, CRef, MRef}}, Left} ->
            true = ets:insert(Monitors, {Pid, CRef, MRef}),
            gen_server:reply(From, {ok, Pid}),
            State#state{waiting = Left};
        {empty, Empty} when Overflow > 0 ->
            ok = dismiss_worker(Sup, Pid),
            State#state{waiting = Empty, overflow = Overflow - 1};
        {empty, Empty} ->
            Workers = case Strategy of
                lifo -> [Pid | State#state.workers];
                fifo -> State#state.workers ++ [Pid]
            end,
            State#state{workers = Workers, waiting = Empty, overflow = 0}
    end.

handle_worker_exit(Pid, State) ->
    #state{supervisor = Sup,
        overflow = Overflow} = State,
    case queue:out(State#state.waiting) of
        {{value, {From, CRef, MRef}}, LeftWaiting} ->
            {ok, NewState} = try_start_new_work_waiting(State, 1 ,1000, From, CRef, MRef, LeftWaiting),
            NewState;
        {empty, Empty} when Overflow > 0 ->
            State#state{overflow = Overflow - 1, waiting = Empty};
        {empty, Empty} -> %%正常来说跑到这里下面的过滤是不必要的，如果存在说明已经出错了
            W = lists:filter(fun (P) -> P =/= Pid end, State#state.workers),
            Workers = try_start_new_work(W, Sup, 1, 5000),
            State#state{workers = Workers, waiting = Empty}
    end.

state_name(State = #state{overflow = Overflow}) when Overflow < 1 ->
    #state{max_overflow = MaxOverflow, workers = Workers} = State,
    case length(Workers) == 0 of
        true when MaxOverflow < 1 -> full;
        true -> overflow;
        false -> ready
    end;
state_name(#state{overflow = MaxOverflow, max_overflow = MaxOverflow}) ->
    full;
state_name(_State) ->
    overflow.



try_start_new_overload_work(Sup, FromPid) ->
    try
        {Pid, MRef} = new_worker(Sup, FromPid),
        {ok, Pid, MRef}
    catch
        _E:Reason ->
            error_logger:warning_msg("try_start_new_overload_work work process error : ~w  Reason : ~w", [_E, Reason]),
            error
    end.

try_start_new_work(WorkS, Sup, Times ,Time) ->
    try
        Pid = new_worker(Sup),
        [Pid | WorkS]
    catch
        _E:Reason ->
            error_logger:warning_msg("tart work process error : ~w  Reason : ~w", [_E, Reason]),
            if
                Times >= 10 -> %%10次之后不尝试重连
                    error_logger:warning_msg("tart work process times let 10 times : ", [Times]);
                true ->
                    erlang:send_after(Time, self(), {start_loop_reconnect, Times + 1 ,Time *2})%%为防止不停的尝试启动，尝试时间翻倍
            end,
            WorkS
    end.


try_start_new_work_waiting(State, Times ,Time, From, CRef, MRef, LeftWaiting) ->
    #state{supervisor = Sup, monitors = Monitors} = State,
    try
        NewWorker = new_worker(Sup),
        true = ets:insert(Monitors, {NewWorker, CRef, MRef}),
        gen_server:reply(From, {ok, NewWorker}),
        {ok, State#state{waiting = LeftWaiting}}
    catch
        _E:Reason ->
            error_logger:warning_msg("try_start_new_work_wating work process error : ~w  Reason : ~w", [_E, Reason]),
            if
                Times >= 10 -> %%10次之后不尝试重连
                    error_logger:warning_msg("tart work process times let 10 times : ", [Times]);
                true ->
                    erlang:send_after(Time, self(), {start_wating_loop_reconnect, Times + 1 ,Time *2})%%为防止不停的尝试启动，尝试时间翻倍
            end,
            {ok, State}
    end.
    


    
