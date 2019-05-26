%%%-------------------------------------------------------------------
%%% @doc
%%% module increase server
%%% @end
%%%-------------------------------------------------------------------
-module(increase_server).
-behaviour(gen_server).
%% API
-export([id/1]).
-export([start/0, start_link/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%%===================================================================
%%% API
%%%===================================================================
id(Key) ->
    gen_server:call(process:pid(?MODULE), {fetch, Key}).

%% @doc start
start() ->
    process:start(?MODULE).

%% @doc gen_server entry
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    erlang:process_flag(trap_exit, true),
    {ok, dict:new()}.

handle_call({fetch, Key}, _From, State) ->
    case dict:find(Key, State) of
        {Key, Value} ->
            NewState = dict:update_counter(Key, Value + 1, State),
            {reply, Value + 1, NewState};
        _ ->
            NewState = dict:store(Key, {Key, 1}, State),
            {reply, 1, NewState}
    end;
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

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
