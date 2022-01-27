%%%-------------------------------------------------------------------
%%% @doc
%%% sorter data host server
%%% @end
%%%-------------------------------------------------------------------
-module(sorter_server).
-behaviour(gen_server).
%% API
-export([start_link/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% Includes
-include("common.hrl").
-include("journal.hrl").
-include("sorter.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc server start
-spec start_link(Name :: atom(), Args :: [term()]) -> {ok, pid()} | {error, term()}.
start_link(Name, Args) ->
    gen_server:start_link({local, Name}, ?MODULE, Args, []).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
%% @doc init
-spec init(Args :: term()) -> {ok, State :: #sorter{}}.
init([Name, share, Type, Limit, Key, Value, Time, Rank, Data]) ->
    erlang:process_flag(trap_exit, true),
    %% make new sorter
    Sorter = sorter:new(Name, share, Type, Limit, Key, Value, Time, Rank, Data),
    {ok, Sorter}.

%% @doc handle_call
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: #sorter{}) -> {reply, Reply :: term(), NewState :: #sorter{}}.
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% @doc handle_cast
-spec handle_cast(Request :: term(), State :: #sorter{}) -> {noreply, NewState :: #sorter{}} | {stop, normal, NewState :: #sorter{}}.
handle_cast({update, Data}, Sorter = #sorter{}) ->
    try
        sorter:update(Data, Sorter)
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?STACKTRACE(Class, Reason, ?GET_STACKTRACE(Stacktrace))
    end,
    {noreply, Sorter};
handle_cast(stop, State) ->
    try
        sorter:drop(State)
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?STACKTRACE(Class, Reason, ?GET_STACKTRACE(Stacktrace))
    end,
    {stop, normal, State};
handle_cast(_Request, State) ->
    {noreply, State}.

%% @doc handle_info
-spec handle_info(Request :: term(), State :: #sorter{}) -> {noreply, NewState :: #sorter{}}.
handle_info(_Info, State) ->
    {noreply, State}.

%% @doc terminate
-spec terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()), State :: #sorter{}) -> {ok, NewState :: #sorter{}}.
terminate(_Reason, State) ->
    {ok, State}.

%% @doc code_change
-spec code_change(OldVsn :: (term() | {down, term()}), State :: #sorter{}, Extra :: term()) -> {ok, NewState :: #sorter{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
%%%===================================================================
%%% Internal functions
%%%===================================================================
