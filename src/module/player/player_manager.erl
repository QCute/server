%%%-------------------------------------------------------------------
%%% @doc
%%% module online manager
%%% @end
%%%-------------------------------------------------------------------
-module(player_manager).
-behaviour(gen_server).
%% export API function
-export([start/0, start_link/1]).
-export([is_online/1, get_user_pid/1, lookup/1, broadcast/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include("common.hrl").
-include("player.hrl").
-define(ONLINE,  online).
%%%===================================================================
%%% API
%%%===================================================================
%% @doc start
start() ->
    process:start(?MODULE).

%% @doc gen_server entry
start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

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
broadcast(Data) ->
    List = ets:tab2list(?ONLINE),
    [player_server:send(Pid, Data) || #online{pid_sender = Pid} <- List],
    ok.
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init(_) ->
    ets:new(?ONLINE, [{keypos, #online.id}, named_table, protected, ordered_set]),
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
