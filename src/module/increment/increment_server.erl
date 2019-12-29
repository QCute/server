%%%------------------------------------------------------------------
%%% @doc
%%% module increment
%%% @end
%%%------------------------------------------------------------------
-module(increment_server).
-behaviour(gen_server).
%% API
-export([next/0, next/1, new/1, new/2]).
-export([start/0, start_link/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% Includes
-include("common.hrl").
%% Macros
-define(DEFAULT_TABLE, [{?MODULE, 0}, {map, 0}, {monster, 0}]).  %% default increment table
%%%==================================================================
%%% API functions
%%%==================================================================
%% @doc next id
-spec next() -> non_neg_integer().
next() ->
    %% Threshold is max long integer(64 bit)
    ets:update_counter(?MODULE, sequence, {2, 1, 16#FFFFFFFFFFFFFFFF, 1}).

%% @doc next id
-spec next(Name :: atom()) -> non_neg_integer().
next(Name) ->
    %% Threshold is max long integer(64 bit)
    ets:update_counter(Name, sequence, {2, 1, 16#FFFFFFFFFFFFFFFF, 1}).

%% @doc add new increase table
-spec new(Name :: atom()) -> ok.
new(Name) when is_atom(Name) ->
    gen_server:cast(?MODULE, {new, Name, 0}).

%% @doc add new increase table
-spec new(Name :: atom(), Begin :: non_neg_integer()) -> ok.
new(Name, Begin) when is_atom(Name) andalso is_integer(Begin) ->
    gen_server:cast(?MODULE, {new, Name, Begin}).

%% @doc start
-spec start() -> {ok, Pid :: pid()} | {error, term()}.
start() ->
    process:start(?MODULE).

%% @doc server start
-spec start_link() -> {ok, Pid :: pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
%%%==================================================================
%%% gen_server callbacks
%%%==================================================================
init([]) ->
    erlang:process_flag(trap_exit, true),
    %% all database data
    StoreList = [{type:to_atom(Name), Value} || [Name, Value | _] <- sql:select("SELECT * FROM `increment`")],
    %% with default table set
    UniqueList = listing:key_unique(1, StoreList ++ ?DEFAULT_TABLE),
    %% init table and value
    TableList = [set_table(Name, Value) || {Name, Value} <- UniqueList],
    {ok, TableList}.

handle_call({new, Name, Begin}, _From, State) ->
    try
        Table = set_table(Name, Begin),
        {reply, ok, [Table | State]}
    catch ?EXCEPTION(_Class, Reason, Stacktrace) ->
        ?STACKTRACE(Reason, ?GET_STACKTRACE(Stacktrace)),
        {reply, ok, State}
    end;
handle_call(_Info, _From, State) ->
    {reply, ok, State}.

handle_cast(_Info, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    try
        %% batch save only at server close
        Format = {<<"INSERT INTO `increment` (`name`, `value`) VALUES ">>, <<"('~s', '~w')">>, <<" ON DUPLICATE KEY UPDATE `value` = VALUES(`value`)">>},
        %% rename table, avoid other process update sequence after save value
        F = fun({Name, _}) -> NewName = type:to_atom(erlang:make_ref()), ets:rename(Name, NewName), Value = ets:lookup_element(NewName, sequence, 2), ets:delete(NewName), {Name, Value} end,
        {Sql, _} = parser:collect_into(lists:map(F, State), fun erlang:tuple_to_list/1, Format, 2),
        sql:insert(Sql)
    catch ?EXCEPTION(_Class, Reason, Stacktrace) ->
        ?STACKTRACE(Reason, ?GET_STACKTRACE(Stacktrace))
    end,
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%==================================================================
%%% Internal functions
%%%==================================================================
%% set table value
set_table(Name, Value) ->
    ets:new(Name, [named_table, public, set, {keypos, 1}, {write_concurrency, true}, {read_concurrency, true}]),
    ets:insert(Name, [{sequence, Value}]),
    {Name, Value}.
