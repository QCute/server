%%%-------------------------------------------------------------------
%%% @doc
%%% increment server
%%% @end
%%%-------------------------------------------------------------------
-module(increment_server).
-behaviour(gen_server).
%% API
-export([next/0, next/1, new/1, new/2, modify/2]).
-export([start/0, start_link/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% Includes
-include("common.hrl").
-include("journal.hrl").
%% Macros
-define(NOT_SAVE, 0). %% do not save key/value to database
-define(SAVE,     1). %% save key/value to database
%% default increment table
-define(DEFAULT_TABLE, [{?MODULE, 0, ?SAVE}, {map, 0, ?SAVE}, {monster, 0, ?SAVE}, {item, db:get_auto_increment(item) - 1, ?NOT_SAVE}, {mail, db:get_auto_increment(mail) - 1, ?NOT_SAVE}, {auction, db:get_auto_increment(auction) - 1, ?NOT_SAVE}]).
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc next id
-spec next() -> non_neg_integer().
next() ->
    %% Threshold is max long integer(64 bit)
    ets:update_counter(?MODULE, ?MODULE, {2, 1, 16#FFFFFFFFFFFFFFFF, 1}).

%% @doc next id
-spec next(Name :: atom()) -> non_neg_integer().
next(Name) ->
    %% Threshold is max long integer(64 bit)
    ets:update_counter(?MODULE, Name, {2, 1, 16#FFFFFFFFFFFFFFFF, 1}).

%% @doc add new increase table
-spec new(Name :: atom()) -> boolean().
new(Name) when is_atom(Name) ->
    new(Name, 0).

%% @doc add new increase table
-spec new(Name :: atom(), Begin :: non_neg_integer()) -> boolean().
new(Name, Begin) when is_atom(Name) andalso is_integer(Begin) ->
    new(Name, Begin, ?NOT_SAVE).

%% @doc add new increase table
-spec new(Name :: atom(), Begin :: non_neg_integer(), Flag :: 0 | 1) -> boolean().
new(Name, Begin, Flag) when is_atom(Name) andalso is_integer(Begin) andalso (Flag == 1 orelse Flag == 0) ->
    ets:insert(?MODULE, {Name, Begin, Flag}).

%% @doc modify table increment
-spec modify(Name :: atom(), Value :: non_neg_integer()) -> boolean().
modify(Name, Value) when is_atom(Name) andalso is_integer(Value) ->
    ets:update_element(?MODULE, Name, {2, Value}).

%% @doc start
-spec start() -> {ok, pid()} | {error, term()}.
start() ->
    process:start(?MODULE).

%% @doc server start
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
%% @doc init
-spec init(Args :: term()) -> {ok, State :: []}.
init([]) ->
    erlang:process_flag(trap_exit, true),
    %% all database data
    StoreList = [{type:to_atom(Name), Value, ?SAVE} || [Name, Value | _] <- db:select("SELECT * FROM `increment`")],
    %% with default table set
    UniqueList = listing:key_unique(1, listing:merge(StoreList, ?DEFAULT_TABLE)),
    %% init table and value
    ets:new(?MODULE, [named_table, public, set, {keypos, 1}, {read_concurrency, true}, {write_concurrency, true}]),
    ets:insert(?MODULE, UniqueList),
    {ok, []}.

%% @doc handle_call
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: []) -> {reply, Reply :: term(), NewState :: []}.
handle_call(_Info, _From, State) ->
    {reply, ok, State}.

%% @doc handle_cast
-spec handle_cast(Request :: term(), State :: []) -> {noreply, NewState :: []}.
handle_cast(_Info, State) ->
    {noreply, State}.

%% @doc handle_info
-spec handle_info(Request :: term(), State :: []) -> {noreply, NewState :: []}.
handle_info(_Info, State) ->
    {noreply, State}.

%% @doc terminate
-spec terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()), State :: []) -> {ok, NewState :: []}.
terminate(_Reason, State) ->
    try
        %% batch save only at server close
        Format = {<<"INSERT INTO `increment` (`name`, `value`) VALUES ">>, <<"('~s', ~w~i)">>, <<" ON DUPLICATE KEY UPDATE `value` = VALUES(`value`)">>},
        %% rename the table, prevent other process update sequence after save value
        NewName = type:to_atom(erlang:make_ref()),
        ets:rename(?MODULE, NewName),
        {Sql, _} = parser:collect_into(NewName, Format, 3),
        db:insert(Sql)
    catch ?EXCEPTION(_Class, Reason, Stacktrace) ->
        ?STACKTRACE(Reason, ?GET_STACKTRACE(Stacktrace))
    end,
    {ok, State}.

%% @doc code_change
-spec code_change(OldVsn :: (term() | {down, term()}), State :: [], Extra :: term()) -> {ok, NewState :: []}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
