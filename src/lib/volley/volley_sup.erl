%% volley_sup manage volley_pool_sup
-module(volley_sup).
-behaviour(supervisor).
-export([start_link/0, add_pool/2, remove_pool/1]).
%% supervisor callback
-export([init/1]).

-spec start_link() -> {ok, pid()} | ignore | {error, {already_started, pid()} | term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec add_pool(PoolName :: atom(), PoolArgs :: term()) -> {ok, pid()} | ignore | {error, {already_started, pid()} | term()}.
add_pool(PoolName, PoolArgs) ->
    ChildSpecs = {PoolName, {volley_pool_sup, start_link, [PoolName,  PoolArgs]}, transient, infinity, supervisor, [PoolName]},
    supervisor:start_child(?MODULE, ChildSpecs).

-spec remove_pool(PoolName ::atom()) -> ok | {error, Error :: term()}.
remove_pool(PoolName) ->
    case supervisor:terminate_child(?MODULE, PoolName) of
        ok ->
            supervisor:delete_child(?MODULE, PoolName);
        Error ->
            Error
    end.

%%%===================================================================
%%% supervisor callback
%%%===================================================================
-spec init(Args :: term()) -> {ok, term()}.
init([]) ->
    {ok, {{one_for_one, 1000, 1}, []}}.
