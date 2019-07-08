%%%-------------------------------------------------------------------
%%% @doc
%%% module online manager
%%% @end
%%%-------------------------------------------------------------------
-module(user_manager).
-behaviour(gen_server).
%% API
-export([start/0, start_link/0]).
-export([apply_call/2, apply_call/3, apply_cast/2, apply_cast/3]).
-export([add/1, remove/1]).
-export([is_online/1, get_user_pid/1]).
-export([lookup/1]).
-export([broadcast/1, broadcast/2]).
-export([change_server_state/1, change_server_mode/1, stop_all/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% Includes
-include("user.hrl").
-include("online.hrl").
%% macros
-define(ONLINE,  online).
-define(SERVER_STATE,  server_state).
%% server open flag
-ifdef(DEBUG).
-define(OPEN, true).
-else.
-define(OPEN, false).
-endif.
%% server entry control
-record(server_state, {mode = all, is_open = ?OPEN}).
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

-spec apply_call(Function :: atom() | function(), Args :: []) -> term().
apply_call(Function, Args) ->
    process:call(?MODULE, {'APPLY_CALL', Function, Args}).

-spec apply_call(Module :: atom(), Function :: atom() | function(), Args :: []) -> term().
apply_call(Module, Function, Args) ->
    process:call(?MODULE, {'APPLY_CALL', Module, Function, Args}).

%% @doc main async cast
-spec apply_cast(Function :: atom() | function(), Args :: []) -> term().
apply_cast(Function, Args) ->
    process:cast(?MODULE, {'APPLY_CAST', Function, Args}).

-spec apply_cast(Module :: atom(), Function :: atom() | function(), Args :: []) -> term().
apply_cast(Module, Function, Args) ->
    process:cast(?MODULE, {'APPLY_CAST', Module, Function, Args}).

%% @doc add
-spec add(OnlineInfo :: #online{}) -> ok.
add(Info) ->
    process:cast(?MODULE, {'add', Info}).

%% @doc remove
-spec remove(UserId :: non_neg_integer()) -> ok.
remove(Id) ->
    process:cast(?MODULE, {'remove', Id}).

%% @doc user online
-spec is_online(UserId :: non_neg_integer()) -> boolean().
is_online(UserId) ->
    case ets:lookup(?ONLINE, UserId) of
        #online{pid = Pid} when is_pid(Pid) ->
            erlang:is_process_alive(Pid);
        _ ->
            false
    end.

%% @doc get online user pid
-spec get_user_pid(UserId :: non_neg_integer()) -> {boolean(), pid() | undefined}.
get_user_pid(UserId) ->
    case ets:lookup(?ONLINE, UserId) of
        #online{pid = Pid} when is_pid(Pid) ->
            {erlang:is_process_alive(Pid), Pid};
        _ ->
            {error, undefined}
    end.

%% @doc loop online user digest info
-spec lookup(UserId :: non_neg_integer()) -> [tuple()].
lookup(UserId) ->
    ets:lookup(?ONLINE, UserId).

%% @doc send data to local server all online role
-spec broadcast(Data :: binary()) -> ok.
broadcast(Data) ->
    ess:foreach(fun(Pid) -> user_sender:send(Pid, Data) end, ?ONLINE, #online.pid).
-spec broadcast(Data :: binary(), ExceptId :: non_neg_integer()) -> ok.
broadcast(Data, ExceptId) ->
    ess:foreach(fun([#online{id = Id, pid_sender = Pid}]) -> Id =/= ExceptId andalso user_sender:send(Pid, Data) == ok end, ?ONLINE).

%% @doc change user entry
-spec change_server_state(IsOpen :: boolean()) -> ok.
change_server_state(IsOpen) ->
    [State] = ets:lookup(?SERVER_STATE, ?SERVER_STATE),
    ets:insert(?SERVER_STATE, State#server_state{is_open = IsOpen}),
    ok.

-spec change_server_mode(Mode :: gm | insider | all) -> ok.
change_server_mode(Mode) ->
    [State] = ets:lookup(?SERVER_STATE, ?SERVER_STATE),
    ets:insert(?SERVER_STATE, State#server_state{mode = Mode}),
    ok.

%% @doc stop
-spec stop_all() -> ok.
stop_all() ->
    ess:foreach(fun(Pid) -> gen_server:cast(Pid, {'stop', server_update}) end, ?ONLINE, #online.pid),
    ok.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init(_) ->
    %% server open control
    ets:new(?SERVER_STATE, [{keypos, 1}, named_table, public, set]),
    ets:insert(?SERVER_STATE, #server_state{}),
    %% user digest
    ets:new(?ONLINE, [{keypos, #online.id}, named_table, protected, set]),
    {ok, []}.

handle_call({'APPLY_CALL', Function, Args}, _From, State) ->
    %% alert !!! call it debug only
    {reply, catch erlang:apply(Function, Args), State};
handle_call({'APPLY_CALL', Module, Function, Args}, _From, State) ->
    %% alert !!! call it debug only
    {reply, catch erlang:apply(Module, Function, Args), State};
handle_call(_Info, _From, State) ->
    {reply, ok, State}.

handle_cast({'APPLY_CAST', Function, Args}, State) ->
    %% alert !!! call it debug only
    catch erlang:apply(Function, Args),
    {noreply, State};
handle_cast({'APPLY_CAST', Module, Function, Args}, State) ->
    %% alert !!! call it debug only
    catch erlang:apply(Module, Function, Args),
    {noreply, State};
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
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
%% ====================================================================
%% Internal functions
%% ====================================================================
