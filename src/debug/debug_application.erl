%%%-------------------------------------------------------------------
%%% @doc
%%% module record application debug use
%%% @end
%%%-------------------------------------------------------------------
-module(debug_application).
-behaviour(application).
-export([application_start/0]).
-export([debug/0,start/2,stop/1]).

%%%===================================================================
%%% API
%%%===================================================================
%% @doc for debug IDE
debug()->
    net_kernel:allow(['erlang@192.168.10.192']),
    main:start(),
    application:start(?MODULE),
    loop_sleep().

loop_sleep() ->
    timer:sleep(10000),
    loop_sleep().

%% @doc shell start callback
application_start()->
    net_kernel:allow(['erlang@192.168.10.192']),
    main:start(),
    application:start(?MODULE).
%%====================================================================
%% application callback
%%====================================================================
%% @doc application start callback
start(_StartType, _StareArgs)->
    SERVER = {beam, {beam, start_link, []}, permanent, 10000, supervisor, [beam]},
    case main_supervisor:start_child(SERVER) of
        {ok, Pid} ->
            {ok, Pid};
        _ ->
            shell_default:c("../src/debug/user_default.erl", [debug_info, {outdir, "../beam/"}]),
            supervisor:start_child(server_supervisor, SERVER)
    end.

%% @doc application stop callback
stop(_State)->
    ok.
%%%===================================================================
%%% Internal functions
%%%===================================================================