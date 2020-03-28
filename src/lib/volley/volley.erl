%%%------------------------------------------------------------------
%%% @doc
%%% volley
%%% * a lightweight erlang process pool inspired from pool-boy, erl-pool, cue-sport, revolver
%%% * simple and fast
%%% * easy to integrate in your project
%%% @end
%%%------------------------------------------------------------------
-module(volley).
-behaviour(application).
-behaviour(supervisor).
%% API
-export([get/1, map/2]).
-export([start_pool/2, stop_pool/1]).
-export([pool_size/1, change_size/2]).
%% application interface
-export([start/0, stop/0]).
%% supervisor interface
-export([start_link/0, start_link/2]).
%% application callbacks
-export([start/2, stop/1]).
%% supervisor callback
-export([init/1, start_worker/3]).
%% export type
-export_type([pool_option/0]).
-type pool_option() :: {worker, {Module :: module(), Function :: atom(), Args :: [term()]}} | {size, non_neg_integer()} | {period, non_neg_integer()} | {intensity, non_neg_integer()} | {shutdown, supervisor:shutdown()} | {restart, supervisor:restart()}.
%%%==================================================================
%%% API functions
%%%==================================================================
%% @doc get a worker
-spec get(atom()) -> {ok, pid()} | {error, term()}.
get(PoolName) ->
    try
        {ok, PoolSize} = pool_size(PoolName),
        N = ets:update_counter(PoolName, seq, {2, 1, PoolSize, 1}),
        [{N, Worker}] = ets:lookup(PoolName, N),
        {ok, Worker}
    catch _:Error ->
        {error, Error}
    end.

%% @doc worker map
-spec map(atom(), fun((pid()) -> any())) -> [term()] | {error, term()}.
map(PoolName, F) ->
    try
        %% match spec generate by ets:fun2ms(fun({_, Pid}) when is_pid(Pid) -> Pid end).
        lists:map(F, ets:select(PoolName, [{{'_', '$1'}, [{is_pid, '$1'}], ['$1']}]))
    catch _:Error ->
        {error, Error}
    end.

%% @doc get pool size
-spec pool_size(atom()) -> {ok, non_neg_integer()} | {error, term()}.
pool_size(PoolName) ->
    try
        {ok, ets:lookup_element(PoolName, size, 2)}
    catch _:Error  ->
        {error, Error}
    end.

%% @doc add and start a pool
-spec start_pool(PoolName :: atom(), PoolArgs :: term()) -> {ok, pid()} | ignore | {error, {already_started, pid()} | term()}.
start_pool(PoolName, PoolArgs) ->
    ChildSpecs = {PoolName, {?MODULE, start_link, [PoolName,  PoolArgs]}, transient, infinity, supervisor, [PoolName]},
    supervisor:start_child(?MODULE, ChildSpecs).

%% @doc stop and remove a pool
-spec stop_pool(PoolName :: atom()) -> ok | {error, Error :: term()}.
stop_pool(PoolName) ->
    case supervisor:terminate_child(?MODULE, PoolName) of
        ok ->
            supervisor:delete_child(?MODULE, PoolName);
        Error ->
            Error
    end.

%% @doc change pool size
-spec change_size(PoolName :: atom(), NewSize :: non_neg_integer()) -> boolean() | term().
change_size(PoolName, NewSize) ->
    try
        SupervisorName = name(PoolName),
        case ets:lookup_element(PoolName, size, 2) of
            Size when Size < NewSize ->
                %% increase pool size
                {ok, ChildSpec} = supervisor:get_childspec(?MODULE, PoolName),
                %% use tuple, r16 or early
                %% {_, {_, _, [_,  PoolArgs]}, _, _, _, _} = ChildSpec
                %% use maps, otp 17 or later
                {_, _, [_, PoolArgs]} = erlang:map_get(start, ChildSpec),
                lists:foreach(fun(Id) -> supervisor:start_child(SupervisorName, make_worker(Id, PoolName, PoolArgs)) end, lists:seq(Size + 1, NewSize)),
                ets:update_element(PoolName, size, {2, NewSize});
            Size when 0 =< NewSize andalso NewSize < Size ->
                %% decrease pool size
                lists:foreach(fun(Id) -> supervisor:terminate_child(SupervisorName, Id) == ok andalso supervisor:delete_child(SupervisorName, Id) == ok andalso ets:delete(PoolName, Id) end, lists:seq(NewSize + 1, Size)),
                ets:update_element(PoolName, size, {2, NewSize});
            NewSize ->
                true;
            _ ->
                false
        end
    catch _:Error ->
        Error
    end.

%% @doc start volley application
-spec start() -> ok | {error, term()}.
start() ->
    application:start(?MODULE).

%% @doc stop volley application
-spec stop() -> ok.
stop() ->
    application:stop(?MODULE).

%% @doc volley supervisor start link
-spec start_link() -> {ok, pid()} | ignore | {error, {already_started, pid()} | term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc volley pool supervisor start link
-spec start_link(atom(), term()) -> {ok, pid()} | ignore | {error, {already_started, pid()} | term()}.
start_link(PoolName, PoolArgs) ->
    supervisor:start_link({local, name(PoolName)}, ?MODULE, [PoolName, PoolArgs]).

%% @doc volley supervisor name
-spec name(atom()) -> atom().
name(PoolName) ->
    list_to_atom(lists:concat([?MODULE, "_", PoolName])).

%%%==================================================================
%%% Application callbacks
%%%==================================================================
%% @doc volley application callback
-spec start(StartType :: term(), StartArgs :: term()) -> {ok, pid()} | {ok, pid(), term()} | {error, term()}.
start(_StartType, _StartArgs) ->
    start_link().

%% @doc volley application callback
-spec stop(State :: term()) -> term().
stop(_State) ->
    ok.

%%%==================================================================
%%% supervisor callback
%%%==================================================================
%% @doc volley and volley pool supervisor callback
-spec init(Args :: term()) -> {ok, {SupFlags :: supervisor:sup_flags(), [ChildSpec :: supervisor:child_spec()]}} | ignore.
init([]) ->
    {ok, {{one_for_one, 1000, 1}, []}};
init([PoolName, PoolArgs]) ->
    %% pool
    PoolSize        = proplists:get_value(size, PoolArgs, 1),
    Worker          = proplists:get_value(worker, PoolArgs),
    %% supervisor
    RestartPeriod   = proplists:get_value(period, PoolArgs, 1),
    Intensity       = proplists:get_value(intensity, PoolArgs, 1000),
    RestartStrategy = proplists:get_value(restart, PoolArgs, permanent),
    Shutdown        = proplists:get_value(shutdown, PoolArgs, infinity),
    %% pool table
    PoolTable = ets:new(PoolName, [named_table, public, set, {write_concurrency, true}, {read_concurrency, true}]),
    true = ets:insert(PoolTable, [{seq, 0}, {size, PoolSize}]),
    %% construct child
    Child = lists:map(fun(Id) -> make_worker(Id, PoolTable, Worker, RestartStrategy, Shutdown) end, lists:seq(1, PoolSize)),
    {ok, {{one_for_one, Intensity, RestartPeriod}, Child}}.

%% @doc start worker
-spec start_worker(Id :: non_neg_integer(), PoolTable :: atom(), Worker :: {Module :: module(), Function :: atom(), Args :: [term()]}) -> {ok, pid()}.
start_worker(Id, PoolTable, {Module, Function, Args}) ->
    {ok, Pid} = erlang:apply(Module, Function, Args),
    erlang:link(Pid),
    true = ets:insert(PoolTable, {Id, Pid}),
    {ok, Pid}.

%%%==================================================================
%%% Internal functions
%%%==================================================================
%% @doc make worker
-spec make_worker(Id :: non_neg_integer(), PoolTable :: atom() | ets:tab(), PoolArgs :: term()) -> supervisor:child_spec().
make_worker(Id, PoolTable, PoolArgs) ->
    %% pool
    Worker          = proplists:get_value(worker, PoolArgs),
    %% supervisor
    RestartStrategy = proplists:get_value(restart, PoolArgs, permanent),
    Shutdown        = proplists:get_value(shutdown, PoolArgs, infinity),
    %% make worker (supervisor spec)
    make_worker(Id, PoolTable, Worker, RestartStrategy, Shutdown).

%% @doc make worker
-spec make_worker(Id :: non_neg_integer(), PoolTable :: atom() | ets:tab(), Worker :: {module(), atom(), term()}, RestartStrategy :: atom(), Shutdown :: atom()) -> supervisor:child_spec().
make_worker(Id, PoolTable, Worker, RestartStrategy, Shutdown) ->
    {Id, {?MODULE, start_worker, [Id, PoolTable, Worker]}, RestartStrategy, Shutdown, worker, [?MODULE]}.
