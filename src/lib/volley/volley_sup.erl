%%%------------------------------------------------------------------
%%% @doc
%%% volley_sup
%%% volley_pool_sup manager
%%% @end
%%%------------------------------------------------------------------
-module(volley_sup).
-behaviour(supervisor).
-export([start_link/0, add_pool/2, remove_pool/1]).
-export([change_size/2]).
%% supervisor callback
-export([init/1]).
%%%==================================================================
%%% API functions
%%%==================================================================
%% @doc start link
-spec start_link() -> {ok, pid()} | ignore | {error, {already_started, pid()} | term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc add a pool
-spec add_pool(PoolName :: atom(), PoolArgs :: term()) -> {ok, pid()} | ignore | {error, {already_started, pid()} | term()}.
add_pool(PoolName, PoolArgs) ->
    ChildSpecs = {PoolName, {volley_pool_sup, start_link, [PoolName,  PoolArgs]}, transient, infinity, supervisor, [PoolName]},
    supervisor:start_child(?MODULE, ChildSpecs).

%% @doc remove pool
-spec remove_pool(PoolName :: atom()) -> ok | {error, Error :: term()}.
remove_pool(PoolName) ->
    case supervisor:terminate_child(?MODULE, PoolName) of
        ok ->
            supervisor:delete_child(?MODULE, PoolName);
        Error ->
            Error
    end.

%% @doc change pool size
-spec change_size(PoolName :: atom(), NewSize :: non_neg_integer()) -> boolean().
change_size(PoolName, NewSize) ->
    SupervisorName = volley_pool_sup:name(PoolName),
    case ets:lookup_element(PoolName, size, 2) of
         Size when Size < NewSize ->
             %% increase pool size
             {ok, ChildSpec} = supervisor:get_childspec(?MODULE, PoolName),
             %% use tuple, r16 or early
             %% {_, {_, _, [_,  PoolArgs]}, _, _, _, _} = ChildSpec
             %% use maps, otp 17 or later
             {_, _, [_, PoolArgs]} = erlang:map_get(start, ChildSpec),
             lists:foreach(fun(Id) -> supervisor:start_child(SupervisorName, volley_pool_sup:make_worker(Id, PoolName, PoolArgs)) end, lists:seq(Size + 1, NewSize)),
             ets:update_element(PoolName, size, {2, NewSize});
        Size when 0 =< NewSize andalso NewSize < Size ->
            %% decrease pool size
            lists:foreach(fun(Id) -> supervisor:terminate_child(SupervisorName, Id) == ok andalso supervisor:delete_child(SupervisorName, Id) == ok andalso ets:delete(PoolName, Id) end, lists:seq(NewSize + 1, Size)),
            ets:update_element(PoolName, size, {2, NewSize});
        NewSize ->
            true;
        _ ->
            false
    end.

%%%==================================================================
%%% supervisor callback
%%%==================================================================
-spec init(Args :: term()) -> {ok, term()}.
init([]) ->
    {ok, {{one_for_one, 1000, 1}, []}}.
