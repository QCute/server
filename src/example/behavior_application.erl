-module(behavior_application).
-behaviour(application).
-export([application_start/0]).
-export([debug/0,start/2,stop/1]).
debug() ->
    application:start(?MODULE),
    loop_sleep().

loop_sleep() ->
    timer:sleep(10000),
    loop_sleep().

application_start() ->
    application:start(?MODULE).
%%====================================================================
%% application callback
%%====================================================================
start(_StartType, _StareArgs) ->
    case behavior_supervisor:start_link() of
        {ok, Pid}->
            %% gen_server
            Server = {behavior_gen_server, {behavior_gen_server, start_link, []}, permanent, 10000, worker, [behavior_gen_server]},
            supervisor:start_child(Pid, Server),
            %% gen_event
            Event = {behavior_gen_event, {behavior_gen_event, start_link, []}, permanent, 10000, worker, [behavior_gen_event]},
            supervisor:start_child(Pid, Event),
            %% gen_statem
            Statem = {behavior_gen_statem, {behavior_gen_statem, start_link, []}, permanent, 10000, worker, [behavior_gen_statem]},
            supervisor:start_child(Pid, Statem),
            {ok, Pid};
        Error->
            Error
    end.

stop(_State) ->
    ok.
