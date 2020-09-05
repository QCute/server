%%%-------------------------------------------------------------------
%%% @doc
%%% start service process and monitor it
%%% @end
%%%-------------------------------------------------------------------
-module(service_supervisor).
-behaviour(supervisor).
%% API
-export([start_child/1, start_link/0]).
%% supervisor callbacks
-export([init/1]).
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc start child
-spec start_child(ChildSpec :: supervisor:child_spec()) -> supervisor:startchild_ret().
start_child(ChildSpec) ->
    supervisor:start_child(?MODULE, ChildSpec).

%% @doc start supervisor
-spec start_link() -> {'ok', pid()} | 'ignore' | {'error', term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).
%%%===================================================================
%%% supervisor callbacks
%%%===================================================================
-spec init(Args :: term()) -> {ok, {SupFlags :: supervisor:sup_flags(), [ChildSpec :: supervisor:child_spec()]}} | ignore.
init([]) ->
    {ok, {{one_for_one, 3, 10}, []}}.
%%%===================================================================
%%% Internal functions
%%%===================================================================
