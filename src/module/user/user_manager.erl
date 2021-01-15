%%%-------------------------------------------------------------------
%%% @doc
%%% user manager
%%% @end
%%%-------------------------------------------------------------------
-module(user_manager).
-behaviour(gen_server).
%% API
-export([start/0, start_link/0]).
-export([add/1, remove/1]).
-export([online/0, online/1, is_online/1, get_user_pid/1]).
-export([lookup/1, lookup_element/2]).
-export([broadcast/1, broadcast/2]).
-export([foreach/1, foreach/2]).
-export([get_create_state/0, set_create_state/1]).
-export([get_server_state/0, set_server_state/1]).
-export([get_chat_state/0, set_chat_state/1]).
-export([update_notify/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% Includes
-include_lib("stdlib/include/ms_transform.hrl").
-include("common.hrl").
-include("online.hrl").
-include("user.hrl").
%% Macros
%% user online digest table
-define(ONLINE,        online_digest).
%% server state table
-define(STATE,         server_state).
%% default server state
-ifdef(DEBUG).
-define(DEFAULT_SERVER_STATE,  ?SERVER_STATE_NORMAL).
-else.
-define(DEFAULT_SERVER_STATE,  ?SERVER_STATE_REFUSE).
-endif.
%% create control
-record(create_state, {state = ?TRUE}).
%% server entry control
-record(server_state, {state = ?DEFAULT_SERVER_STATE}).
%% server chat control
-record(chat_state, {state = ?CHAT_STATE_UNLIMITED}).
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

%% @doc add
-spec add(OnlineInfo :: #online{}) -> true.
add(Info) ->
    ets:insert(?ONLINE, Info).

%% @doc remove
-spec remove(RoleId :: non_neg_integer()) -> true.
remove(Id) ->
    ets:delete(?ONLINE, Id).

%% @doc all online number
-spec online() -> non_neg_integer().
online() ->
    ets:info(?ONLINE, size).

%% @doc real online/hosting online number
-spec online(Type :: online | hosting) -> non_neg_integer().
online(Type) ->
    ets:select_count(?ONLINE, ets:fun2ms(fun(#online{state = State}) when State == Type -> true end)).

%% @doc user online
-spec is_online(RoleId :: non_neg_integer()) -> boolean().
is_online(RoleId) ->
    case ets:lookup(?ONLINE, RoleId) of
        [#online{pid = Pid}] when is_pid(Pid) ->
            erlang:is_process_alive(Pid);
        _ ->
            false
    end.

%% @doc get online user pid
-spec get_user_pid(RoleId :: non_neg_integer()) -> pid() | undefined.
get_user_pid(RoleId) ->
    case ets:lookup(?ONLINE, RoleId) of
        [#online{pid = Pid}] ->
            Pid;
        _ ->
            undefined
    end.

%% @doc lookup online user digest info
-spec lookup(RoleId :: non_neg_integer()) -> [#online{}].
lookup(RoleId) ->
    ets:lookup(?ONLINE, RoleId).

%% @doc lookup online user digest info
-spec lookup_element(RoleId :: non_neg_integer(), Position :: non_neg_integer()) -> term().
lookup_element(RoleId, Position) ->
    ets:lookup_element(?ONLINE, RoleId, Position).

%% @doc send data to all online role
-spec broadcast(Data :: binary()) -> ok.
broadcast(Data) ->
    spawn(fun() -> ess:foreach(fun([#online{sender_pid = SenderPid}]) when is_pid(SenderPid) -> user_sender:send(SenderPid, Data); (_) -> ok end, ?ONLINE) end), ok.

%% @doc send data to all online role except id
-spec broadcast(Data :: binary(), ExceptId :: non_neg_integer()) -> ok.
broadcast(Data, ExceptId) ->
    spawn(fun() -> ess:foreach(fun([#online{role_id = RoleId, sender_pid = Pid}]) when RoleId =/= ExceptId andalso is_pid(Pid) -> user_sender:send(Pid, Data); (_) -> ok end, ?ONLINE) end), ok.

%% @doc call f each online role
-spec foreach(F :: function()) -> ok.
foreach(F) ->
    spawn(fun() -> ess:foreach(fun([Online]) -> F(Online) end, ?ONLINE) end), ok.

%% @doc call f each online role except id
-spec foreach(Data :: function(), ExceptId :: non_neg_integer()) -> ok.
foreach(F, ExceptId) ->
    spawn(fun() -> ess:foreach(fun([Online = #online{role_id = RoleId}]) when RoleId =/= ExceptId -> F(Online); (_) -> ok end, ?ONLINE) end), ok.

%% @doc get user create control
-spec get_create_state() -> Status :: ?TRUE | ?FALSE.
get_create_state() ->
    ets:lookup_element(?STATE, create_state, #create_state.state).

%% @doc change user create control
-spec set_create_state(Status :: ?TRUE | ?FALSE) -> ok.
set_create_state(State) ->
    ets:insert(?STATE, #create_state{state = State}),
    ok.

%% @doc get user entry control
-spec get_server_state() -> Status :: ?SERVER_STATE_REFUSE | ?SERVER_STATE_MASTER | ?SERVER_STATE_INSIDER | ?SERVER_STATE_NORMAL.
get_server_state() ->
    ets:lookup_element(?STATE, server_state, #server_state.state).

%% @doc change user entry control
-spec set_server_state(Status :: ?SERVER_STATE_REFUSE | ?SERVER_STATE_MASTER | ?SERVER_STATE_INSIDER | ?SERVER_STATE_NORMAL) -> ok.
set_server_state(State) ->
    ets:insert(?STATE, #server_state{state = State}),
    ok.

%% @doc get user chat control
-spec get_chat_state() -> Status :: ?CHAT_STATE_UNLIMITED | ?CHAT_STATE_SILENT_WORLD | ?CHAT_STATE_SILENT_GUILD | ?CHAT_STATE_SILENT_PRIVATE.
get_chat_state() ->
    ets:lookup_element(?STATE, chat_state, #chat_state.state).

%% @doc change user chat control
-spec set_chat_state(Status :: ?CHAT_STATE_UNLIMITED | ?CHAT_STATE_SILENT_WORLD | ?CHAT_STATE_SILENT_GUILD | ?CHAT_STATE_SILENT_PRIVATE) -> ok.
set_chat_state(State) ->
    ets:insert(?STATE, #chat_state{state = State}),
    ok.

%% @doc refuse new login, stop old server
-spec update_notify() -> ok.
update_notify() ->
    %% refuse login
    set_server_state(?SERVER_STATE_REFUSE),
    %% stop all role server
    ess:foreach(fun([#online{pid = Pid}]) -> gen_server:cast(Pid, {stop, server_update}) end, ?ONLINE).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
%% @doc init
-spec init(Args :: term()) -> {ok, State :: []}.
init(_) ->
    erlang:process_flag(trap_exit, true),
    %% server open control
    ets:new(?STATE, [{keypos, 1}, named_table, public, set, {read_concurrency, true}]),
    StoreList = [{type:to_atom(Name), Value} || [Name, Value | _] <- db:select("SELECT * FROM `state`")],
    UniqueList = listing:key_unique(1, listing:merge(StoreList, [#create_state{}, #server_state{}, #chat_state{}])),
    ets:insert(?STATE, UniqueList),
    %% user digest
    ets:new(?ONLINE, [{keypos, #online.role_id}, named_table, public, set, {read_concurrency, true}, {write_concurrency, true}]),
    %% loop timer
    erlang:send_after(?MINUTE_MILLISECONDS, self(), {loop, time:now()}),
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
handle_info({loop, Before}, State) ->
    Now = time:now(),
    Hour = time:hour(Now),
    %% next loop
    erlang:send_after(?MINUTE_MILLISECONDS, self(), {loop, Now}),
    %% collect online digest
    log:online_log(online(), online(online), online(hosting), Hour, Now),
    %% all process garbage collect at morning 6 every day
    _ = time:is_cross_day(Before, 6, Now) andalso lists:foreach(fun(Pid) -> erlang:garbage_collect(Pid) end, erlang:processes()),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%% @doc terminate
-spec terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()), State :: []) -> {ok, NewState :: []}.
terminate(_Reason, State) ->
    try
        %% batch save only at server close
        Format = {<<"INSERT INTO `state` (`name`, `value`) VALUES ">>, <<"('~s', ~w)">>, <<" ON DUPLICATE KEY UPDATE `value` = VALUES(`value`)">>},
        %% rename the table, prevent other process update sequence after save value
        NewName = type:to_atom(erlang:make_ref()),
        ets:rename(?STATE, NewName),
        Sql = parser:collect(NewName, Format),
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
