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
    %% general tcp
    {ok, _} = listener:start_gen_tcp(),
    %% tcp with ssl
    {ok, _} = listener:start_ssl(),
    {ok, Pid}.

%% @doc start application services
-spec start_services() -> 'ok'.
start_services() ->
    %% server supervisor
    {ok, _} = server_supervisor:start_link(),
    %% timer tick server
    {ok, _} = time:start(),
    %% rand server
    {ok, _} = rand:start(),
    %% database connect pool
    {ok, _} = data_pool:start(),
    %% guild
    {ok, _} = guild_server:start(),
    %% player manager
    {ok, _} = player_manager:start(),
    ok.
%%%===================================================================
%%% Internal functions
%%%===================================================================

