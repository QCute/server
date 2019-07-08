%%%-------------------------------------------------------------------
%%% @doc
%%% module increase server
%%% @end
%%%-------------------------------------------------------------------
-module(increase_server).
-behaviour(gen_server).
%% API
-export([id/1, ids/2]).
-export([start/0, start_link/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%%===================================================================
%%% API
%%%===================================================================
%% @doc id
-spec id(Key :: atom()) -> non_neg_integer().
id(Key) ->
    process:call(?MODULE, {id, Key}).

%% @doc ids
-spec ids(Key :: atom(), Number :: non_neg_integer()) -> [non_neg_integer()].
ids(Key, Number) ->
    process:call(?MODULE, {ids, Key, Number}).

%% @doc start
-spec start() -> {ok, Pid :: pid()} | {error, term()}.
start() ->
    process:start(?MODULE).

%% @doc server start
-spec start_link() -> {ok, Pid :: pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    erlang:process_flag(trap_exit, true),
    {ok, dict:new()}.

handle_call({id, Key}, _From, State) ->
    case dict:find(Key, State) of
        {ok, Value} ->
            NewState = dict:update_counter(Key,  1, State),
            {reply, Value + 1, NewState};
        _ ->
            NewState = dict:store(Key, 1, State),
            {reply, 1, NewState}
    end;
handle_call({ids, Key, Number}, _From, State) ->
    case dict:find(Key, State) of
        {ok, Value} ->
            NewState = dict:update_counter(Key, Number, State),
            {reply, lists:seq(Value + 1, Value + Number), NewState};
        _ ->
            NewState = dict:store(Key, Number, State),
            {reply, lists:seq(1, Number), NewState}
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
