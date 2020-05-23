%%%-------------------------------------------------------------------
%%% @doc
%%% module robot
%%% @end
%%%-------------------------------------------------------------------
-module(robot).
-behavior(gen_server).
%% API
-export([event/3, send/3]).
-export([start/1, start_link/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include("../../../include/common.hrl").
-include("../../../include/protocol.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc role server socket event
-spec event(Id :: non_neg_integer(), Code :: non_neg_integer(), Data :: term()) -> ok.
event(Id, Code, Data) ->
    user_server:cast(Id, {socket_event, Code, Data}).

%% @doc tcp socket binary
-spec send(Account :: string(), Code :: non_neg_integer(), Binary :: binary()) -> ok.
send(Account, Protocol, Binary) ->
    erlang:send(type:to_atom(lists:concat([?MODULE, "_", Account])), {send, Protocol, Binary}).

%% @doc start role client
-spec start(Account :: string()) -> {ok, pid()} | {error, term()}.
start(Account) ->
    process:start(type:to_atom(lists:concat([?MODULE, "_", Account])), ?MODULE, [Account]).

%% @doc server start
-spec start_link(Args :: [term()]) -> {ok, pid()} | {error, term()}.
start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

%%%===================================================================
%%% general server
%%%===================================================================
%% @doc init
-spec init(Args :: term()) -> {ok, gen_tcp:socket()}.
init(Account) ->
    process_flag(trap_exit, true),
    erlang:send_after(1000, self(), {login, Account}),
    gen_tcp:connect("127.0.0.1", config:net_gen_tcp_start_port() + config:server_id(), [{mode, binary}, {packet, 0}]).

%% @doc handle_call
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: gen_tcp:socket()) -> {reply, Reply :: term(), State :: gen_tcp:socket()}.
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% @doc handle_cast
-spec handle_cast(Request :: term(), State :: gen_tcp:socket()) -> {noreply, NewState :: gen_tcp:socket()}.
handle_cast(_Request, State) ->
    {noreply, State}.

%% @doc handle_info
-spec handle_info(Request :: term(), State :: gen_tcp:socket()) -> {noreply, NewState :: gen_tcp:socket()}.
handle_info({login, Account}, State) ->
    ServerId = config:server_id(),
    Binary = erlang:list_to_binary(encoding:to_list(Account)),
    Data = protocol:pack(?PROTOCOL_ACCOUNT_LOGIN, <<ServerId:16, (byte_size(Binary)):16, Binary/binary>>),
    gen_tcp:send(State, Data),
    {noreply, State};
handle_info({tcp, State, <<_:16, ?PROTOCOL_ACCOUNT_LOGIN:16, 0:16>>}, State) ->
    io:format("login success~n"),
    handle_info(loop, State);
handle_info({tcp, State, <<_:16, ?PROTOCOL_ACCOUNT_LOGIN:16, _:16, Reason/binary>>}, State) ->
    io:format("login failed: ~s~n", [Reason]),
    handle_info(loop, State);
handle_info({tcp, State, <<_:16, Protocol:16, Data/binary>>}, State) ->
    io:format("Protocol:~p~nBinary~0p~n", [Protocol, Data]),
    {noreply, State};
handle_info(loop, State) ->
    erlang:send_after(30 * 1000, self(), loop),
    Data = protocol:pack(?PROTOCOL_ACCOUNT_HEARTBEAT, <<>>),
    gen_tcp:send(State, Data),
    io:format("loop~n"),
    {noreply, State};
handle_info({tcp_closed, State}, State) ->
    io:format("close~n"),
    {stop, normal, State};
handle_info({send, Protocol, Binary}, State) ->
    Data = protocol:pack(Protocol, Binary),
    gen_tcp:send(State, Data),
    {noreply, State};
handle_info(Request, State) ->
    io:format("~n~0p~n", [Request]),
    {noreply, State}.

%% @doc terminate
-spec terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()), State :: gen_tcp:socket()) -> {ok, NewState :: gen_tcp:socket()}.
terminate(_Reason, State) ->
    gen_tcp:close(State),
    {ok, State}.

%% @doc code_change
-spec code_change(OldVsn :: (term() | {down, term()}), State :: gen_tcp:socket(), Extra :: term()) -> {ok, NewState :: gen_tcp:socket()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
%%%===================================================================
%%% Internal functions
%%%===================================================================
