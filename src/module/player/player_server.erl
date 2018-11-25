%%%-------------------------------------------------------------------
%%% @doc
%%% module player server
%%% @end
%%%-------------------------------------------------------------------
-module(player_server).
-behaviour(gen_server).
%% includes
-include("common.hrl").
-include("player.hrl").
%% API
-export([start/2]).
-export([call/2, call/3, call/4, cast/2, cast/3, cast/4, send/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% user state
%%%===================================================================
%%% API
%%%===================================================================
%% @doc server start
start(UserId, Socket) ->
    Name = process:player_name(UserId),
    gen_server:start_link({local, Name}, ?MODULE, [UserId, Socket], []).

%% @doc main async call
call(Pid, Function) ->
    gen_server:cast(Pid, {'APPLY_CALL', Function}).
call(Pid, Function, Args) ->
    gen_server:cast(Pid, {'APPLY_CALL', Function, Args}).
call(Pid, Module, Function, Args) ->
    gen_server:cast(Pid, {'APPLY_CALL', Module, Function, Args}).

%% @doc alert !!! call it debug only
cast(Pid, Function) ->
    gen_server:cast(Pid, {'APPLY_CAST', Function}).
cast(Pid, Function, Args) ->
    gen_server:cast(Pid, {'APPLY_CAST', Function, Args}).
cast(Pid, Module, Function, Args) ->
    gen_server:cast(Pid, {'APPLY_CAST', Module, Function, Args}).

%% @doc send to client use link sender
send(#user{pid_sender = Pid}, Binary) ->
    send(Pid, Binary);
send(Pid, Binary) when is_pid(Pid) ->
    erlang:send(Pid, {'SEND', Binary}).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([UserId, Socket]) ->
    NewUser = player_login:login(#user{id = UserId, socket = Socket}),
    {ok, NewUser}.

handle_call(Request, From, User) ->
    ?STACK_TRACE(do_call(Request, From, User), {reply, ok, User}).

handle_cast(Request, User) ->
    ?STACK_TRACE(do_cast(Request, User), {noreply, User}).

handle_info(Info, User) ->
    ?STACK_TRACE(do_info(Info, User), {noreply, User}).

terminate(_Reason, User) ->
    ?STACK_TRACE(player_logout:logout(User)),
    ok.

code_change(_OldVsn, User, _Extra) ->
    {ok, User}.
%%-------------------------------------------------------------------
%% main sync player process call back
%%-------------------------------------------------------------------
do_call({'APPLY_CALL', Function}, _From, User) ->
    %% alert !!! call it debug only
    {reply, erlang:apply(Function, [User]), User};
do_call({'APPLY_CALL', Function, Args}, _From, User) ->
    %% alert !!! call it debug only
    {reply, erlang:apply(Function, [User | Args]), User};
do_call({'APPLY_CALL', Module, Function, Args}, _From, User) ->
    %% alert !!! call it debug only
    {reply, erlang:apply(Module, Function, [User | Args]), User};
do_call({'SOCKET_EVENT', Protocol, Data}, _From, User) ->
    %% alert !!! call it debug only
    NewUser = socket_event(User, Protocol, Data),
    {reply, ok, NewUser};
do_call(_Request, _From, User) ->
    {reply, ok, User}.

%%-------------------------------------------------------------------
%% main async player process call back
%%-------------------------------------------------------------------
do_cast({'APPLY_CAST', Function}, User) ->
    %% alert !!! call it debug only
    erlang:apply(Function, [User]),
    {noreply, User};
do_cast({'APPLY_CAST', Function, Args}, User) ->
    %% alert !!! call it debug only
    erlang:apply(Function, [User | Args]),
    {noreply, User};
do_cast({'APPLY_CAST', Module, Function, Args}, User) ->
    %% alert !!! call it debug only
    erlang:apply(Module, Function, [User | Args]),
    {noreply, User};
do_cast({'SOCKET_EVENT', Protocol, Data}, User) ->
    %% socket protocol
    NewUser = socket_event(User, Protocol, Data),
    {noreply, NewUser};
do_cast({'SELECT'}, User) ->
    %% handle early something on socket select
    {noreply, User};
do_cast({'STOP'}, User) ->
    %% handle stop
    {noreply, User};
do_cast(_Request, User) ->
    {noreply, User}.

%% un recommend
do_info({'SEND', Protocol, Reply}, User) ->
    reply(User, Protocol, Reply),
    {noreply, User};
do_info({'SEND', Binary}, User = #user{pid_sender = Pid}) ->
    erlang:send(Pid, Binary),
    {noreply, User};
do_info('TIMER', User) ->
    %% 统一定时处理
    NewUser = player_logout:logout(User),
    {noreply, NewUser};
do_info(_Info, User) ->
    {noreply, User}.



%%%===================================================================
%%% Internal functions
%%%===================================================================
%% handle socket event
socket_event(Player, Protocol, Data) ->
    %% Result = do_routing(Player, Protocol, Data),
    case player_route:handle_routing(Player, Protocol, Data) of
        ok ->
            Player;
        {ok, NewPlayer} ->
            NewPlayer;
        {update, NewPlayer} ->
            NewPlayer;
        {reply, Reply} ->
            reply(Player, Protocol, Reply),
            Player;
        {relpy, Reply, NewPlayer} ->
            reply(Player, Protocol, Reply),
            NewPlayer;
        {error, unknow_command} ->
            Player;
        _ ->
            Player
    end.

%% push reply to client
reply(#user{pid_sender = Pid}, Protocol, Reply) ->
    reply(Pid, Protocol, Reply);
reply(Pid, Protocol, Reply) when is_pid(Pid) ->
    case player_route:write(Protocol, Reply) of
        {ok, Data} ->
            erlang:send(Pid, {'SEND', Data});
        _ ->
            {error, pack_data_error}
    end.