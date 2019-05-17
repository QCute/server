%%%-------------------------------------------------------------------
%%% @doc
%%% module service supervisor, to start process and monitor it(for function server)
%%% @end
%%%-------------------------------------------------------------------
-module(service_supervisor).
-behaviour(supervisor).
%% API
-export([start_child/1, start_link/0]).
%% supervisor callbacks
-export([init/1]).
%%%===================================================================
%%% API
%%%===================================================================
%% @doc start child
start_child(ChildSpec) ->
    supervisor:start_child(?MODULE, ChildSpec).

%% @doc start supervisor
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).
%%%===================================================================
%%% supervisor callbacks
%%%===================================================================
init([]) ->
    {ok, {{one_for_one, 3, 10},[]}}.
%%%===================================================================
%%% Internal functions
%%%===================================================================
