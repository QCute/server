-module(behavior_gen_server).
-behaviour(gen_server).
%% export function
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%%====================================================================
%% gen_server entry
%%====================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, [], [], []).
%%====================================================================
%% gen_server callback
%%====================================================================
init([]) ->
    {ok, []}.
handle_call(_Info, _From, State) ->
    {reply, ok, State}.
handle_cast(_Info, State) ->
    {noreply, State}.
handle_info(_Info, State) ->
    {noreply, State}.
terminate(normal, State) ->
    {ok, State}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
