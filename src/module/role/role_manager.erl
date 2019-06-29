%%%-------------------------------------------------------------------
%%% @doc
%%% module online manager
%%% @end
%%%-------------------------------------------------------------------
-module(role_manager).
-behaviour(gen_server).
%% API
-export([start/0, start_link/0]).
-export([add/1, remove/1]).
-export([is_online/1, get_user_pid/1]).
-export([lookup/1]).
-export([broadcast/1, broadcast/2]).
-export([change_server_state/1, change_server_mode/1, stop_all/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% Includes
-include("user.hrl").
-include("role.hrl").
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
start() ->
    process:start(?MODULE).

%% @doc gen_server entry
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc add
-spec add(OnlineInfo :: #online{}) -> ok.
add(Info) ->
    gen_server:cast(process:pid(?MODULE), {'add', Info}).

%% @doc remove
-spec remove(UserId :: non_neg_integer()) -> ok.
remove(Id) ->
    gen_server:cast(process:pid(?MODULE), {'remove', Id}).

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
    ess:foreach(fun(Pid) -> role_sender:send(Pid, Data) end, ?ONLINE, #online.pid).
-spec broadcast(Data :: binary(), ExceptId :: non_neg_integer()) -> ok.
broadcast(Data, ExceptId) ->
    ess:foreach(fun([#online{id = Id, pid_sender = Pid}]) -> Id =/= ExceptId andalso role_sender:send(Pid, Data) == ok end, ?ONLINE).

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
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
%% ====================================================================
%% Internal functions
%% ====================================================================
