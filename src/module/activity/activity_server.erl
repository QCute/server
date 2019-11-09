%%%------------------------------------------------------------------
%%% @doc
%%% module activity server
%%% @end
%%%------------------------------------------------------------------
-module(activity_server).
-behaviour(gen_server).
%% API
-export([start/1, start_link/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% Includes
-include("common.hrl").
%%%==================================================================
%%% API functions
%%%==================================================================
%% @doc start
-spec start(Args :: term()) -> {ok, Pid :: pid()} | {error, term()}.
start(Args) ->
    process:start(?MODULE, [Args]).

%% @doc server start
-spec start_link(Args :: term()) -> {ok, Pid :: pid()} | {error, term()}.
start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

%%%==================================================================
%%% gen_server callbacks
%%%==================================================================
init(NodeType) ->
    erlang:process_flag(trap_exit, true),
    activity:server_start(node:type_to_integer(NodeType)).

handle_call(Request, From, State) ->
    try
        do_call(Request, From, State)
    catch ?EXCEPTION(_Class, Reason, Stacktrace) ->
        ?STACKTRACE(Reason, ?GET_STACKTRACE(Stacktrace)),
        {reply, ok, State}
    end.

handle_cast(Request, State) ->
    try
        do_cast(Request, State)
    catch ?EXCEPTION(_Class, Reason, Stacktrace) ->
        ?STACKTRACE(Reason, ?GET_STACKTRACE(Stacktrace)),
        {noreply, State}
    end.

handle_info(Info, State) ->
    try
        do_info(Info, State)
    catch ?EXCEPTION(_Class, Reason, Stacktrace) ->
        ?STACKTRACE(Reason, ?GET_STACKTRACE(Stacktrace)),
        {noreply, State}
    end.

terminate(_Reason, State) ->
    try
        {ok, State}
    catch ?EXCEPTION(_Class, Reason, Stacktrace) ->
        ?STACKTRACE(Reason, ?GET_STACKTRACE(Stacktrace)),
        ok
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%==================================================================
%%% Internal functions
%%%==================================================================
%% call
do_call(_Request, _From, State) ->
    {reply, ok, State}.

%% cast
do_cast(_Request, State) ->
    {noreply, State}.

%% info
do_info({daily, Time}, State) ->
    %% refresh activity daily
    activity:refresh(Time, State),
    {noreply, State};
do_info(_Info, State) ->
    {noreply, State}.
