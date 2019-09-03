%%%-------------------------------------------------------------------
%%% @doc
%%% module online manager
%%% @end
%%%-------------------------------------------------------------------
-module(user_manager).
-behaviour(gen_server).
%% API
-export([start/0, start_link/0]).
-export([add/1, remove/1]).
-export([online/0, online/1, is_online/1, get_user_pid/1]).
-export([lookup/1]).
-export([broadcast/1, broadcast/2]).
-export([get_server_state/0, set_server_state/1]).
-export([remote_set_server_state/2]).
-export([stop_all/0, stop_all/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% Includes
-include("common.hrl").
-include("user.hrl").
-include("online.hrl").
%% Macros
%% user online digest table
-define(ONLINE,        online_digest).
%% server state table
-define(STATE,         server_state).
%% default server state
-ifdef(DEBUG).
-define(DEFAULT_SERVER_STATE,  all).
-else.
-define(DEFAULT_SERVER_STATE,  refuse).
-endif.
%% server entry control
-record(server_state, {state = ?DEFAULT_SERVER_STATE}).
%%%===================================================================
%%% API
%%%===================================================================
%% @doc start
-spec start() -> {ok, Pid :: pid()} | {error, term()}.
start() ->
    process:start(?MODULE).

%% @doc server start
-spec start_link() -> {ok, Pid :: pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc add
-spec add(OnlineInfo :: #online{}) -> ok.
add(Info) ->
    process:cast(?MODULE, {'add', Info}).

%% @doc remove
-spec remove(RoleId :: non_neg_integer()) -> ok.
remove(Id) ->
    process:cast(?MODULE, {'remove', Id}).

%% @doc all online amount
-spec online() -> non_neg_integer().
online() ->
    ets:info(?ONLINE, size).

%% @doc real online/hosting online amount
-spec online(Type :: online | hosting) -> non_neg_integer().
online(Type) ->
    length(ets:select(?ONLINE, ets:fun2ms(fun(Online = #online{status = Status}) when Status =:= Type -> Online end))).

%% @doc user online
-spec is_online(RoleId :: non_neg_integer()) -> boolean().
is_online(RoleId) ->
    case ets:lookup(?ONLINE, RoleId) of
        #online{pid = Pid} when is_pid(Pid) ->
            erlang:is_process_alive(Pid);
        _ ->
            false
    end.

%% @doc get online user pid
-spec get_user_pid(RoleId :: non_neg_integer()) -> {boolean(), pid() | undefined}.
get_user_pid(RoleId) ->
    case ets:lookup(?ONLINE, RoleId) of
        #online{pid = Pid} when is_pid(Pid) ->
            {erlang:is_process_alive(Pid), Pid};
        _ ->
            {error, undefined}
    end.

%% @doc loop online user digest info
-spec lookup(RoleId :: non_neg_integer()) -> [tuple()].
lookup(RoleId) ->
    ets:lookup(?ONLINE, RoleId).

%% @doc send data to local server all online role
-spec broadcast(Data :: binary()) -> ok.
broadcast(Data) ->
    ess:foreach(fun(Pid) -> user_sender:send(Pid, Data) end, ?ONLINE, #online.pid).

-spec broadcast(Data :: binary(), ExceptId :: non_neg_integer()) -> ok.
broadcast(Data, ExceptId) ->
    ess:foreach(fun([#online{role_id = RoleId, sender_pid = Pid}]) -> RoleId =/= ExceptId andalso user_sender:send(Pid, Data) == ok end, ?ONLINE).

%% @doc get user entry control
-spec get_server_state() -> Status :: refuse | gm | insider | all.
get_server_state() ->
    ets:lookup_element(?STATE, ?STATE, #server_state.state).

%% @doc change user entry control
-spec set_server_state(Status :: refuse | gm | insider | all) -> ok.
set_server_state(State) ->
    [State] = ets:lookup(?STATE, ?STATE),
    ets:insert(?STATE, State#server_state{state = State}),
    ok.

%% @doc remote change user entry control
-spec remote_set_server_state(Nodes :: [atom()] | [list()], State :: refuse | gm | insider | all) -> true.
remote_set_server_state(NodeList, State) ->
    [net_adm:ping(Node) == pong andalso rpc:cast(type:to_atom(Node), ?MODULE, set_server_state, [State]) || Node <- NodeList].

%% @doc stop
-spec stop_all() -> ok.
stop_all() ->
    ess:foreach(fun(Pid) -> gen_server:cast(Pid, {'stop', server_update}) end, ?ONLINE, #online.pid).

%% @doc stop with wait for all stop flag
-spec stop_all(Wait :: boolean()) -> ok.
stop_all(true) ->
    stop_all();
stop_all(false) ->
    stop_all(),
    %% wait for all server exit
    listing:while(fun() -> timer:sleep(1000), online() end).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init(_) ->
    %% server open control
    ets:new(?STATE, [{keypos, 1}, named_table, public, set, {read_concurrency, true}]),
    ets:insert(?STATE, #server_state{}),
    %% user digest
    ets:new(?ONLINE, [{keypos, #online.role_id}, named_table, protected, set, {read_concurrency, true}]),
    %% loop
    erlang:send_after(?MINUTE_SECONDS * 1000, self(), loop),
    {ok, []}.

handle_call(_Info, _From, State) ->
    {reply, ok, State}.

handle_cast(_Info, State) ->
    {noreply, State}.

handle_info({'add', New = #online{}}, State) ->
    %% update online role info cache
    ets:insert(?ONLINE, New),
    {noreply, State};
handle_info({'remove', Id}, State) ->
    %% update online role info cache
    ets:delete(?ONLINE, Id),
    {noreply, State};
handle_info(loop, State) ->
    %% loop
    erlang:send_after(?MINUTE_SECONDS * 1000, self(), loop),
    %% collect online digest
    Now = time:ts(),
    All = online(),
    Online = online(online),
    Hosting = online(hosting),
    log:online_log(Now, All, Online, Hosting),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
%% ====================================================================
%% Internal functions
%% ====================================================================
