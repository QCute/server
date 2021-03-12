%%%-------------------------------------------------------------------
%%% @doc
%%% key server
%%% @end
%%%-------------------------------------------------------------------
-module(key_server).
-behaviour(gen_server).
%% API
-export([start/0, start_link/0]).
-export([award/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% Includes
-include("common.hrl").
-include("journal.hrl").
-include("user.hrl").
-include("key.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc start
-spec start() -> {ok, pid()} | {error, term()}.
start() ->
    process:start(?MODULE).

%% @doc server start
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc award
-spec award(User :: #user{}, Key :: binary()) -> ok() | error().
award(User, Key) ->
    case key_award_data:award(key_data:get(Key)) of
        #key_award_data{is_unique = IsUnique, award = Award} ->
            award_request(User, Key, IsUnique, Award);
        _ ->
            {error, no_such_key}
    end.

award_request(User = #user{role_id = RoleId}, Key, true, Award) ->
    case catch gen_server:call(?MODULE, {receive_award, RoleId, Key}) of
        {ok, Result} ->
            {ok, NewUser} = item:add(User, Award, key_award),
            {ok, Result, NewUser};
        {'EXIT', {timeout, _}} ->
            {error, timeout};
        Error ->
            Error
    end;
award_request(User = #user{role_id = RoleId}, Key, false, Award) ->
    case catch key_sql:insert(#key{role_id = RoleId, key = Key}) of
        {'EXIT', _} ->
            {error, key_already_active};
        _ ->
            {ok, NewUser} = item:add(User, Award, key_award),
            {ok, ok, NewUser}
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
%% @doc init
-spec init(Args :: term()) -> {ok, State :: []}.
init(_) ->
    erlang:process_flag(trap_exit, true),
    {ok, []}.

%% @doc handle_call
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: []) -> {reply, Reply :: term(), NewState :: []}.
handle_call({receive_award, RoleId, Key}, _From, State) ->
    try
        {reply, receive_award(RoleId, Key), State}
    catch ?EXCEPTION(_Class, Reason, Stacktrace) ->
        ?STACKTRACE(Reason, ?GET_STACKTRACE(Stacktrace)),
        {reply, ok, State}
    end;
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
    {ok, State}.

%% @doc code_change
-spec code_change(OldVsn :: (term() | {down, term()}), State :: [], Extra :: term()) -> {ok, NewState :: []}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% receive award
receive_award(RoleId, Key) ->
    case key_sql:select_by_key(Key) of
        [] ->
            key_sql:insert(#key{role_id = RoleId, key = Key}),
            {ok, ok};
        _ ->
            {error, key_already_active}
    end.
