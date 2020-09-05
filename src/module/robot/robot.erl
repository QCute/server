%%%-------------------------------------------------------------------
%%% @doc
%%% robot
%%% @end
%%%-------------------------------------------------------------------
-module(robot).
-behavior(gen_server).
%% API
-export([event/3, send/3]).
-export([start/1, start_link/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include("../../../include/common.hrl").
-include("../../../include/protocol.hrl").
-record(state, {role_id = 0, role_name = <<>>, server_id = 0, account = <<>>, socket}).
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
    Name = type:to_atom(lists:concat([?MODULE, "_", Account])),
    process:start(Name, ?MODULE, [Name, Account]).

%% @doc server start
-spec start_link(Name :: atom(), Args :: [term()]) -> {ok, pid()} | {error, term()}.
start_link(Name, Args) ->
    gen_server:start_link({local, Name}, ?MODULE, Args, []).

%%%===================================================================
%%% general server
%%%===================================================================
%% @doc init
-spec init(Args :: term()) -> {ok, #state{}}.
init(Account) ->
    process_flag(trap_exit, true),
    erlang:send_after(1000, self(), query),
    {ok, Socket} = gen_tcp:connect("127.0.0.1", config:net_gen_tcp_start_port() + config:server_id(), [{mode, binary}, {packet, 0}]),
    {ok, #state{server_id = config:server_id(), account = erlang:list_to_binary(encoding:to_list(Account)), socket = Socket}}.

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
handle_info(query, State = #state{server_id = ServerId, account = Account, socket = Socket}) ->
    Data = protocol:pack(?PROTOCOL_ACCOUNT_QUERY, <<ServerId:16, (byte_size(Account)):16, Account/binary>>),
    gen_tcp:send(Socket, Data),
    {noreply, State};
handle_info({tcp, _, <<_:16, ?PROTOCOL_ACCOUNT_QUERY:16, 0:16>>}, State) ->
    handle_info(login, State);
handle_info({tcp, _, <<_:16, ?PROTOCOL_ACCOUNT_QUERY:16, _:16, _/binary>>}, State) ->
    handle_info(create, State);

handle_info(create, State = #state{server_id = ServerId, account = Account, socket = Socket}) ->
    %% Account, RoleName, ServerId, Sex, Classes, ChannelId, DeviceId, Mac, DeviceType
    Data = protocol:pack(?PROTOCOL_ACCOUNT_CREATE, <<ServerId:16, (byte_size(Account)):16, Account/binary, (byte_size(Account)):16, Account/binary,(randomness:rand(1, 2)):8, (randomness:rand(1, 6)):8, 4:16, "test", 0:16, 0:16, 6:16, "robot.">>),
    gen_tcp:send(Socket, Data),
    {noreply, State};
handle_info({tcp, _, <<_:16, ?PROTOCOL_ACCOUNT_CREATE:16, 0:16>>}, State) ->
    handle_info(login, State);
handle_info({tcp, _, <<_:16, ?PROTOCOL_ACCOUNT_CREATE:16, _:16, Reason/binary>>}, State) ->
    io:format("create failed: ~ts~n", [Reason]),
    {stop, normal, State};

handle_info(login, State = #state{server_id = ServerId, account = Account, socket = Socket}) ->
    Data = protocol:pack(?PROTOCOL_ACCOUNT_LOGIN, <<ServerId:16, (byte_size(Account)):16, Account/binary>>),
    gen_tcp:send(Socket, Data),
    {noreply, State};
handle_info({tcp, _, <<_:16, ?PROTOCOL_ACCOUNT_LOGIN:16, 0:16>>}, State) ->
    handle_info(loop, State);
handle_info({tcp, _, <<_:16, ?PROTOCOL_ACCOUNT_LOGIN:16, _:16, Reason/binary>>}, State) ->
    io:format("login failed: ~ts~n", [Reason]),
    {stop, normal, State};

handle_info({send, Protocol, Binary}, State = #state{socket = Socket}) ->
    gen_tcp:send(Socket, protocol:pack(Protocol, Binary)),
    {noreply, State};
handle_info({tcp, _, <<_:16, Protocol:16, Data/binary>>}, State) ->
    io:format("Protocol:~p~nBinary~0p~n", [Protocol, Data]),
    {noreply, State};

handle_info(loop, State = #state{socket = Socket}) ->
    erlang:send_after(30 * 1000, self(), loop),
    gen_tcp:send(Socket, protocol:pack(?PROTOCOL_ACCOUNT_HEARTBEAT, <<>>)),
    {noreply, State};

handle_info({tcp_closed, State}, State) ->
    io:format("close~n"),
    {stop, normal, State};

handle_info(Request, State) ->
    io:format("~n~0p~n", [Request]),
    {noreply, State}.

%% @doc terminate
-spec terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()), State :: #state{}) -> {ok, NewState :: #state{}}.
terminate(_Reason, State = #state{socket = Socket}) ->
    gen_tcp:close(Socket),
    {ok, State}.

%% @doc code_change
-spec code_change(OldVsn :: (term() | {down, term()}), State :: gen_tcp:socket(), Extra :: term()) -> {ok, NewState :: #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
%%%===================================================================
%%% Internal functions
%%%===================================================================
