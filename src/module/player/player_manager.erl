%%%-------------------------------------------------------------------
%%% @doc
%%% module online manager
%%% @end
%%%-------------------------------------------------------------------
-module(player_manager).
-behaviour(gen_server).
%% export API function
-export([start/0, start_link/0]).
-export([is_online/1, get_user_pid/1, lookup/1, broadcast/1, broadcast/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% includes
-include("player.hrl").
-define(ONLINE,  online).
%%%===================================================================
%%% API
%%%===================================================================
%% @doc start
start() ->
    process:start(?MODULE).

%% @doc gen_server entry
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc user online
is_online(UserId) ->
    case ets:lookup(?ONLINE, UserId) of
        #online{pid = Pid} when is_pid(Pid) ->
            erlang:is_process_alive(Pid);
        _ ->
            false
    end.

%% @doc get online user pid
get_user_pid(UserId) ->
    case ets:lookup(?ONLINE, UserId) of
        #online{pid = Pid} when is_pid(Pid) ->
            {erlang:is_process_alive(Pid), Pid};
        _ ->
            {error, offline}
    end.

%% @doc loop online user digest info
lookup(UserId) ->
    ets:lookup(?ONLINE, UserId).

%% @doc send data to local server all online player
-spec broadcast(Data :: binary()) -> ok.
broadcast(Data) ->
    traverse(fun(Pid) -> player_server:send(Pid, Data) end, ?ONLINE, #online.pid).
-spec broadcast(Data :: binary(), ExceptId :: non_neg_integer()) -> ok.
broadcast(Data, ExceptId) ->
    traverse(fun(#online{id = Id, pid_sender = Pid}) when Id =/= ExceptId -> player_server:send(Pid, Data), ok; (_) -> ok end, ?ONLINE).

-spec traverse(F :: fun((Element :: term()) -> term()), Tab :: atom()) -> term().
traverse(F, Tab) ->
    traverse(F, Tab, 0).
-spec traverse(F :: fun((Element :: term()) -> term()), Tab :: atom(), Pos :: non_neg_integer()) -> term().
traverse(F, Tab, Pos) ->
    traverse(Tab, undefined, F, Pos).
traverse(_Tab, '$end_of_table', _F, _Pos) ->
    ok;
traverse(Tab, undefined, F, Pos) ->
    traverse(Tab, ets:first(Tab), F, Pos);
traverse(Tab, Key, F, 0) ->
    F(ets:lookup(Tab, Key)),
    traverse(Tab, ets:next(Tab, Key), F, 0);
traverse(Tab, Key, F, Pos) ->
    F(ets:lookup_element(Tab, Key, Pos)),
    traverse(Tab, ets:next(Tab, Key), F, Pos).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init(_) ->
    ets:new(?ONLINE, [{keypos, #online.id}, named_table, protected, set]),
    {ok, []}.

handle_call(_Info, _From, State)->
    {reply, ok, State}.

handle_cast(_Info, State) ->
    {noreply, State}.

handle_info({'update', New = #online{}}, State) ->
    %% update online player info cache
    ets:insert(?ONLINE, New),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra)->
    {ok, State}.
%% ====================================================================
%% Internal functions
%% ====================================================================
