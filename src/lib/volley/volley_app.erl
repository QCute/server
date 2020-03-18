%%%------------------------------------------------------------------
%%% @doc
%%% volley_app
%%% volley application manager
%%% @end
%%%------------------------------------------------------------------
-module(volley_app).
-behaviour(application).
%% application callbacks
-export([start/2, stop/1]).
%%%==================================================================
%%% Application callbacks
%%%==================================================================
-spec start(StartType :: term(), StartArgs :: term()) -> {ok, pid()} | {ok, pid(), term()} | {error, term()}.
start(_StartType, _StartArgs) ->
    volley_sup:start_link().

-spec stop(State :: term()) -> term().
stop(_State) ->
    ok.
%%%==================================================================
%%% Internal functions
%%%==================================================================
