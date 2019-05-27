%%%-------------------------------------------------------------------
%%% @doc
%%% module path finder(async process pool support)
%%% @end
%%%-------------------------------------------------------------------
-module(path_finder).
-behaviour(gen_server).
%% API
-export([find/4]).
-export([start/0, start_link/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%%===================================================================
%%% API
%%%===================================================================
%% @doc find
find(Id, MapId, Start, End) ->
    case volley:get(?MODULE) of
        {ok, Worker} ->
            gen_server:cast(Worker, {find, self(), Id, MapId, Start, End});
        Error ->
            Error
    end.

start() ->
    volley:start_pool(?MODULE, [{size, 4}, {worker, {?MODULE, start_link, []}}]).

%% @doc start
start_link() ->
    gen_server:start(?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    erlang:process_flag(trap_exit, true),
    {ok, dict:new()}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({find, From, Id, MapId, Start, End}, State) ->
    case a_star:find(Start, End, MapId) of
        [_ | T] ->
            gen_server:cast(From, {path, Id, lists:sublist(T, 4)});
        _ ->
            gen_server:cast(From, {path, Id, []})
    end,
    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
