%%%-------------------------------------------------------------------
%%% @doc
%%% module services
%%% @end
%%%-------------------------------------------------------------------
-module(services).
%% API
-export([start_io/0]).
-export([start_services/0]).
%%%===================================================================
%%% API
%%%===================================================================
%% @doc start io services
-spec start_io() -> {'ok', Pid :: pid()}.
start_io() ->
    %% server io listener/acceptor/receiver
    {ok, Pid} = main_supervisor:start_link(),
    listener:start_gen_tcp(),
    listener:start_ssl(),
    {ok, Pid}.

%% @doc start application services
-spec start_services() -> 'ok'.
start_services() ->
    %% server supervisor
    server_supervisor:start_link(),
    %% timer tick server
    time_server:start(),
    %% rand server
    rand_server:start(),
    %% database connect pool
    pool:start(),
    ok.
%%%===================================================================
%%% Internal functions
%%%===================================================================

