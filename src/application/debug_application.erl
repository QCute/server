%%%-------------------------------------------------------------------
%%% @doc
%%% module debug application for intellij idea
%%% @end
%%%-------------------------------------------------------------------
-module(debug_application).
-behaviour(application).
-export([debug/0, start/0]).
-export([start/2, stop/1]).
%% Intellij Idea Debug Guild
%% Run -> Edit Configurations -> Add -> Erlang Application
%% Parameter set like :
%% Name               : Main
%% Module and function: debug_application debug
%% Function arguments :
%% Working Directory  : (project root path, to system absolute path)
%% Flags for 'erl'    : -hidden +pc unicode -pa beam -pa config -pa app -smp true +P 1024000 +t 10485760 +zdbbl 1024 -setcookie erlang -name main@127.0.0.1 -config config/main -boot start_sasl
%% Before launch      : (remove build option(default open))
%% Active tool window : true
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
            shell_default:c("src/tool/extension/user_default.erl", [debug_info, {outdir, "beam/"}]),
            service_supervisor:start_child(BeamServer)
    end.

%% @doc application stop callback
stop(_State) ->
    ok.
%%%===================================================================
%%% Internal functions
%%%===================================================================