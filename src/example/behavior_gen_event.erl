-module(behavior_gen_event).
-behaviour(gen_event).
%% export function
-export([start_link/0, add_handler/0]).
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).
%%====================================================================
%% event entry
%%====================================================================
start_link() ->
    gen_event:start_link(?MODULE).

add_handler() ->
    gen_event:add_handler(?MODULE, ?MODULE, []).
%%%===================================================================
%%% gen_event callbacks
%%%===================================================================
init([]) ->
    {ok, []}.
handle_event(_Event, State) ->
    {ok, State}.
handle_call(_Request, State) ->
    {ok, ok, State}.
handle_info(_Info, State) ->
    {ok, State}.
terminate(_Arg, State) ->
    {ok, State}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
