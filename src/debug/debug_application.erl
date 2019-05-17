%%%-------------------------------------------------------------------
%%% @doc
%%% module record application debug use
%%% @end
%%%-------------------------------------------------------------------
-module(debug_application).
-behaviour(application).
-export([debug/0, start/0]).
-export([start/2, stop/1]).

%%%===================================================================
%%% API
%%%===================================================================
%% @doc for debug IDE
debug() ->
    main:start(),
    application:start(?MODULE),
    loop_sleep().

loop_sleep() ->
    timer:sleep(10000),
    loop_sleep().

%% @doc shell start callback
start() ->
    main:start(),
    application:start(?MODULE).
%%====================================================================
%% application callback
%%====================================================================
%% @doc application start callback
start(_StartType, _StareArgs) ->
    BeamServer = {beam, {beam, start_link, []}, permanent, 10000, supervisor, [beam]},
    case service_supervisor:start_child(BeamServer) of
        {ok, Pid} ->
            {ok, Pid};
        _ ->
            shell_default:c("src/debug/user_default.erl", [debug_info, {outdir, "beam/"}]),
            service_supervisor:start_child(BeamServer)
    end.

%% @doc application stop callback
stop(_State) ->
    ok.
%%%===================================================================
%%% Internal functions
%%%===================================================================