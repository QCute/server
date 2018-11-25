%%%-------------------------------------------------------------------
%%% @doc
%%% module user
%%% @end
%%%-------------------------------------------------------------------
-module(player_sender).
-behaviour(gen_server).
%% includes
-include("common.hrl").
%% API
-export([start/4]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% user sender state
-record(state, {user_id, receiver_pid, socket, module = none, connect_lost = false}).
%%%===================================================================
%%% API
%%%===================================================================
%% @doc server start
start(UserId, ReceiverPid, Socket, Module) ->
    Name = process:sender_name(UserId),
    gen_server:start_link({local, Name}, ?MODULE, [UserId, ReceiverPid, Socket, Module], []).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([UserId, ReceiverPid, Socket, Module]) ->
    {ok, #state{user_id = UserId, receiver_pid = ReceiverPid, socket = Socket, module = Module}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({'SEND', Binary}, State = #state{module = gen_tcp, socket = Socket}) ->
    catch erts_internal:port_command(Socket, Binary, [force]),
    {noreply, State};
handle_cast({'SEND', Binary}, State = #state{module = ssl, socket = Socket}) ->
    catch ssl:send(Socket, Binary),
    {noreply, State};
handle_cast({'STOP'}, State) ->
    %% handle stop
    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({'SEND', Binary}, State = #state{module = gen_tcp, socket = Socket}) ->
    catch erts_internal:port_command(Socket, Binary, [force]),
    {noreply, State};
handle_info({'SEND', Binary}, State = #state{module = ssl, socket = Socket}) ->
    catch ssl:send(Socket, Binary),
    {noreply, State};
handle_info({'STOP'}, State) ->
    %% handle stop
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================