%%%-------------------------------------------------------------------
%%% @doc
%%% increment server
%%% @end
%%%-------------------------------------------------------------------
-module(increment_server).
-compile({no_auto_import, [now/0]}).
-behaviour(gen_server).
%% API
-export([now/0, now/1, exist/0, exist/1, next/0, next/1, new/1, new/2, modify/2]).
-export([start/0, start_link/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% Includes
-include("journal.hrl").
%% save flag
-define(TEMPORARY,                                    0). %% do not save key/value to database
-define(PERMANENT,                                    1). %% save key/value to database
%% static increment table
-define(TABLE, [
    {?MODULE, 0, ?PERMANENT},
    {map, 0, ?PERMANENT},
    {monster, 0, ?PERMANENT},
    {item, db:get_auto_increment(item) - 1, ?TEMPORARY},
    {mail, db:get_auto_increment(mail) - 1, ?TEMPORARY},
    {auction, db:get_auto_increment(auction) - 1, ?TEMPORARY},
    {lucky_money, db:get_auto_increment(lucky_money) - 1, ?TEMPORARY}
]).

%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc now id
-spec now() -> non_neg_integer().
now() ->
    ets:lookup_element(?MODULE, ?MODULE, 2).

%% @doc now id
-spec now(Name :: term()) -> non_neg_integer().
now(Name) ->
    ets:lookup_element(?MODULE, Name, 2).

%% @doc exist
-spec exist() -> boolean().
exist() ->
    ets:member(?MODULE, ?MODULE).

%% @doc exist
-spec exist(Name :: term()) -> boolean().
exist(Name) ->
    ets:member(?MODULE, Name).

%% @doc next id
-spec next() -> non_neg_integer().
next() ->
    %% Threshold is max long integer(64 bit)
    ets:update_counter(?MODULE, ?MODULE, {2, 1, 16#FFFFFFFFFFFFFFFF, 1}).

%% @doc next id
-spec next(Name :: term()) -> non_neg_integer().
next(Name) ->
    %% Threshold is max long integer(64 bit)
    ets:update_counter(?MODULE, Name, {2, 1, 16#FFFFFFFFFFFFFFFF, 1}).

%% @doc add new increase table
-spec new(Name :: term()) -> boolean().
new(Name) ->
    new(Name, 0).

%% @doc add new increase table
-spec new(Name :: term(), Begin :: non_neg_integer()) -> boolean().
new(Name, Begin) when is_integer(Begin) ->
    new(Name, Begin, ?TEMPORARY).

%% @doc add new increase table
-spec new(Name :: term(), Begin :: non_neg_integer(), Flag :: 0 | 1) -> boolean().
new(Name, Begin, Flag) when is_integer(Begin) andalso (Flag == 1 orelse Flag == 0) ->
    ets:insert(?MODULE, {Name, Begin, Flag}).

%% @doc modify table increment
-spec modify(Name :: term(), Value :: non_neg_integer()) -> boolean().
modify(Name, Value) when is_integer(Value) ->
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
    StoreList = [{type:to_atom(Name), Value, ?PERMANENT} || [Name, Value | _] <- db:select("SELECT * FROM `increment`")],
    %% with default table set
    UniqueList = listing:key_unique(1, listing:merge(StoreList, ?TABLE)),
    %% init table and value
    ets:new(?MODULE, [named_table, public, set, {keypos, 1}, {read_concurrency, true}, {write_concurrency, true}]),
    ets:insert(?MODULE, UniqueList),
    {ok, []}.

%% @doc handle_call
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: []) -> {reply, Reply :: term(), NewState :: []}.
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% @doc handle_cast
-spec handle_cast(Request :: term(), State :: []) -> {noreply, NewState :: []}.
handle_cast(_Request, State) ->
    {noreply, State}.

%% @doc handle_info
-spec handle_info(Request :: term(), State :: []) -> {noreply, NewState :: []}.
handle_info(_Info, State) ->
    {noreply, State}.

%% @doc terminate
-spec terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()), State :: []) -> {ok, NewState :: []}.
terminate(_Reason, State) ->
    try
        %% rename the table, prevent other process update sequence after save value
        NewName = type:to_atom(erlang:make_ref()),
        ets:rename(?MODULE, NewName),
        db:save_into(<<"INSERT INTO `increment` (`name`, `value`) VALUES ">>, <<"(:1:, :2:)">>, <<" ON DUPLICATE KEY UPDATE `value` = VALUES(`value`)">>, NewName, 3)
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?STACKTRACE(Class, Reason, ?GET_STACKTRACE(Stacktrace))
    end,
    {ok, State}.

%% @doc code_change
-spec code_change(OldVsn :: (term() | {down, term()}), State :: [], Extra :: term()) -> {ok, NewState :: []}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
