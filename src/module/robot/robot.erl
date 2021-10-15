%%%-------------------------------------------------------------------
%%% @doc
%%% robot
%%% @end
%%%-------------------------------------------------------------------
-module(robot).
-behavior(gen_server).
%% API
-export([event/3, send/3]).
-export([start_link/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include("../../../include/common.hrl").
-include("../../../include/time.hrl").
-include("../../../include/journal.hrl").
-include("../../../include/protocol.hrl").
-record(state, {role_id = 0, role_name = <<>>, server_id = 0, account = <<>>, socket, packet = <<>>}).
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

%% @doc server start
-spec start_link(Account :: string()) -> {ok, pid()} | {error, term()}.
start_link(Account) ->
    gen_server:start_link({local, type:to_atom(lists:concat([?MODULE, "_", Account]))}, ?MODULE, Account, []).

%%%===================================================================
%%% general server
%%%===================================================================
%% @doc init
-spec init(Args :: term()) -> {ok, #state{}}.
init(Account) ->
    process_flag(trap_exit, true),
    erlang:send_after(1000, self(), query),
    {ok, Socket} = gen_tcp:connect("127.0.0.1", config:net_gen_tcp_start_port() + config:server_id(), [{mode, binary}, {packet, 0}]),
    {ok, #state{server_id = config:server_id(), account = erlang:list_to_binary(type:to_list(Account)), socket = Socket}}.

%% @doc handle_call
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: #state{}) -> {reply, Reply :: term(), State :: #state{}}.
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% @doc handle_cast
-spec handle_cast(Request :: term(), State :: #state{}) -> {noreply, NewState :: #state{}}.
handle_cast(_Request, State) ->
    {noreply, State}.

%% @doc handle_info
-spec handle_info(Request :: term(), State :: #state{}) -> {noreply, NewState :: #state{}} | {stop, term(), #state{}}.
handle_info({tcp, Socket, Stream}, State = #state{packet = Packet}) ->
    case <<Packet/binary, Stream/binary>> of
        <<Length:16, Protocol:16, Data:Length/binary, Rest/binary>> ->
            try
                case handle_info({data, Protocol, Data}, State#state{packet = <<>>}) of
                    {noreply, NewState} ->
                        handle_info({tcp, Socket, Rest}, NewState);
                    Result ->
                        Result
                end
            catch ?EXCEPTION(Class, Reason, Stacktrace) ->
                ?STACKTRACE(Class, Reason, ?GET_STACKTRACE(Stacktrace)),
                {stop, normal, State}
            end;
        Binary ->
            {noreply, State#state{packet = Binary}}
    end;



handle_info(query, State = #state{server_id = ServerId, account = Account, socket = Socket}) ->
    Data = protocol:pack(?PROTOCOL_ACCOUNT_QUERY, <<ServerId:16, (byte_size(Account)):16, Account/binary>>),
    gen_tcp:send(Socket, Data),
    {noreply, State};
handle_info({data, ?PROTOCOL_ACCOUNT_QUERY, <<0:16>>}, State) ->
    %% io:format("query empty: ~n"),
    handle_info(create, State);
handle_info({data, ?PROTOCOL_ACCOUNT_QUERY, <<_:16, Data/binary>>}, State) ->
    [{RoleId, RoleName} | _] = [{RoleId, RoleName} || <<RoleId:64, NameLength:16, RoleName:NameLength/binary>> <= Data],
    %% io:format("query success: ~p ~ts~n", [RoleId, RoleName]),
    handle_info(login, State#state{role_id = RoleId, role_name = RoleName});

handle_info(create, State = #state{server_id = ServerId, account = Account, socket = Socket}) ->
    %% RoleName, Account, ServerId, Sex, Classes, ChannelId, DeviceId, Mac, DeviceType
    %% WordList = lists:seq($0, $9) ++ lists:seq($a, $z) ++ lists:seq($A, $Z),
    RoleName = list_to_binary([listing:random(listing:shuffle("0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ~!@#$%^&*_-+=/\\,.<>?:;'\"{}[]")) || _ <- lists:seq(1, 6)]),
    Data = protocol:pack(?PROTOCOL_ACCOUNT_CREATE, <<(byte_size(RoleName)):16,  RoleName/binary, ServerId:16, (byte_size(Account)):16, Account/binary, (randomness:rand(1, 2)):8, (randomness:rand(1, 6)):8, 4:16, "test", 0:16, 0:16, 6:16, "robot.">>),
    gen_tcp:send(Socket, Data),
    {noreply, State};
handle_info({data, ?PROTOCOL_ACCOUNT_CREATE, <<0:16, RoleId:64, _:16, RoleName/binary>>}, State) ->
    %% io:format("create success: ~p ~ts~n", [RoleId, RoleName]),
    handle_info(login, State#state{role_id = RoleId, role_name = RoleName});
handle_info({data, ?PROTOCOL_ACCOUNT_CREATE, <<Length:16, Reason:Length/binary, _/binary>>}, State) ->
    io:format("create failed: ~ts~n", [Reason]),
    {stop, normal, State};

handle_info(login, State = #state{role_id = RoleId, role_name = RoleName, server_id = ServerId, account = Account, socket = Socket}) ->
    Data = protocol:pack(?PROTOCOL_ACCOUNT_LOGIN, <<RoleId:64, (byte_size(RoleName)):16, RoleName/binary, ServerId:16, (byte_size(Account)):16, Account/binary>>),
    gen_tcp:send(Socket, Data),
    {noreply, State};
handle_info({data, ?PROTOCOL_ACCOUNT_LOGIN, <<0:16>>}, State) ->
    %% io:format("login success: ~n"),
    %% recharge
    RechargeId = randomness:rand(1, 10),
    Command = <<"recharge_", (integer_to_binary(RechargeId))/binary>>,
    erlang:send_after(?SECOND_MILLISECONDS(randomness:rand(1, ?DAY_SECONDS)), self(), {send, ?PROTOCOL_CHEAT_CHEAT, <<(byte_size(Command)):16, Command/binary>>}),
    handle_info(loop, State);
handle_info({data, ?PROTOCOL_ACCOUNT_LOGIN, <<_:16, Reason/binary>>}, State) ->
    io:format("login failed: ~ts~n", [Reason]),
    {stop, normal, State};

handle_info(loop, State = #state{socket = Socket}) ->
    erlang:send_after(45 * 1000, self(), loop),
    gen_tcp:send(Socket, protocol:pack(?PROTOCOL_ACCOUNT_HEARTBEAT, <<>>)),
    {noreply, State};
handle_info({data, ?PROTOCOL_ACCOUNT_LOGOUT, <<_:16, Reason/binary>>}, State) ->
    io:format("logout: ~ts~n", [Reason]),
    {stop, normal, State};

handle_info({send, Protocol, Binary}, State = #state{socket = Socket}) ->
    gen_tcp:send(Socket, protocol:pack(Protocol, Binary)),
    {noreply, State};
handle_info({data, _, _}, State) ->
    %% io:format("Protocol:~p~nBinary~p~n", [Protocol, Data]),
    {noreply, State};

handle_info({tcp_closed, _}, State) ->
    {stop, normal, State};

handle_info(Request, State) ->
    io:format("~n~p~n", [Request]),
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
