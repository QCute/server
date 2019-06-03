%%%-------------------------------------------------------------------
%%% @doc
%%% @date 2019-05-07
%%% mysql connect driver
%%% connect/query/handle by single process
%%% support version 4.1 or later
%%% arguments pass by tuple list(config file compatibility) instead maps(otp 17 early supported)
%%% version 1.0.0 (stable release)
%%% @end
%%%-------------------------------------------------------------------
-module(mysql_driver).
%% API
%% pool support
-export([start_pool/0, start_pool/1, start_pool/2, start_pool/4]).
%% server entry
-export([start_link/1, start/1]).
%% main query interface
-export([state/1, fetch/2]).
%% build-in result handler
-export([handle_result/3, handle_result/4]).
%% normal query interface
-export([select/2, insert/2, update/2, delete/2]).
%% get result/error info from result
-export([get_field/1, get_rows/1, get_affected/1, get_reason/1, get_code/1, get_error_state/1, get_insert_id/1]).
%%%-------------------------------------------------------------------
%%% Macros
%%%-------------------------------------------------------------------
%% MySQL Commands
-define(OP_SLEEP,                 16#00).
-define(OP_QUIT,                  16#01).
-define(OP_INIT_DB,               16#02).
-define(OP_QUERY,                 16#03).
-define(OP_FIELD_LIST,            16#04).
-define(OP_CREATE_DB,             16#05).
-define(OP_DROP_DB,               16#06).
-define(OP_REFRESH,               16#07).
-define(OP_SHUTDOWN,              16#08).
-define(OP_STATISTICS,            16#09).
-define(OP_PROCESS_INFO,          16#0a).
-define(OP_CONNECT,               16#0b).
-define(OP_PROCESS_KILL,          16#0c).
-define(OP_DEBUG,                 16#0d).
-define(OP_PING,                  16#0e).
-define(OP_TIME,                  16#0f).
-define(OP_DELAYED_INSERT,        16#10).
-define(OP_CHANGE_USER,           16#11).
-define(OP_BINLOG_DUMP,           16#12).
-define(OP_TABLE_DUMP,            16#13).
-define(OP_CONNECT_OUT,           16#14).
-define(OP_REGISTER_SLAVE,        16#15).
-define(OP_STMT_PREPARE,          16#16).
-define(OP_STMT_EXECUTE,          16#17).
-define(OP_STMT_SEND_LONG_DATA,   16#18).
-define(OP_STMT_CLOSE,            16#19).
-define(OP_STMT_RESET,            16#1a).
-define(OP_SET_OPTION,            16#1b).
-define(OP_STMT_FETCH,            16#1c).

%% -------------------------------------------------------------------
%% MySQL Authentication
%% Character sets
-define(UTF8MB3,                  16#21). %% utf8_general_ci
-define(UTF8MB4,                  16#2d). %% utf8mb4_general_ci

%% --- Capability flags ---
-define(CLIENT_LONG_PASSWORD,     16#00000001).
-define(CLIENT_FOUND_ROWS,        16#00000002).
-define(CLIENT_LONG_FLAG,         16#00000004).
-define(CLIENT_CONNECT_WITH_DB,   16#00000008).
-define(CLIENT_PROTOCOL_41,       16#00000200).
-define(CLIENT_SSL_SUPPORT,       16#00000800).
-define(CLIENT_TRANSACTIONS,      16#00002000).
-define(CLIENT_SECURE_CONNECTION, 16#00008000).
-define(CLIENT_MULTI_STATEMENTS,  16#00010000).
-define(CLIENT_MULTI_RESULTS,     16#00020000).
-define(CLIENT_PS_MULTI_RESULTS,  16#00040000).
-define(CLIENT_PLUGIN_SUPPORT,    16#00080000).
-define(CLIENT_MAX_PACKET_SIZE,   16#40000000).

%% -------------------------------------------------------------------
%% Response packet tag (first byte)
-define(OK,                       16#00).      %% 0
-define(EOF,                      16#fe).      %% 254
-define(ERROR,                    16#ff).      %% 255

%% time define
-define(TIMEOUT,                  5000).       %% query default timeout

%%%-------------------------------------------------------------------
%%% Records
%%%-------------------------------------------------------------------
%% mysql result info
-record(mysql_result, {
    type :: atom(),
    field = [],
    rows = [],
    affected_rows = 0,
    insert_id = 0,
    error_code = 0,
    error_message = "",
    error_state = ""
}).

%% handshake info
-record(handshake, {
    version :: binary(),
    id :: integer(),
    capabilities :: integer(),
    charset :: integer(),
    status :: integer(),
    salt :: binary(),
    plugin :: binary()
}).

%% mysql connection state
-record(state, {
    module :: atom(),
    socket :: port(),
    parent :: pid(),
    host :: string(),
    port :: non_neg_integer(),
    user :: string(),
    password :: string(),
    database :: string(),
    encoding :: string(),
    timeout = infinity,
    data = <<>>,
    handshake :: #handshake{},
    packet = <<>>,
    number = 0,
    fields = [],
    rows = []
}).

%%%===================================================================
%%% API
%%%===================================================================
%% @doc start pool with pool boy(args pass by application config)
-spec start_pool() -> {ok, Pid :: pid()} | {error, Reason :: term()}.
start_pool() ->
    %% set module name as default
    start_pool(?MODULE).

%% @doc start pool with pool boy(args pass by application config)
-spec start_pool(Name :: atom()) -> {ok, Pid :: pid()} | {error, Reason :: term()}.
start_pool(Name) ->
    %% read driver config from application env(config file)
    {ok, DriverArgs} = application:get_env(Name),
    start_pool(Name, DriverArgs).

%% @doc start pool with pool boy
-spec start_pool(Name :: atom(), DriverArgs :: list()) -> {ok, Pid :: pid()} | {error, Reason :: term()}.
start_pool(Name, DriverArgs) ->
    PoolArgs = [{worker, {?MODULE, start_link, [DriverArgs]}}, {size, 16}],
    %% use volley
    start_pool(volley, start_pool, Name, PoolArgs).

%% @doc start pool
-spec start_pool(Module :: atom(), Function :: atom(), PoolArg :: list(), DriverArgs :: list()) -> {ok, Pid :: pid()} | {error, Reason :: term()}.
start_pool(Module, Function, PoolArgs, DriverArgs) ->
    %% start pool with start args
    Module:Function(PoolArgs, DriverArgs).

%% mysql connect arguments supported
%% |---------------|---------------|--------------|
%% |   key         |   value       |  default     |
%% |---------------|---------------|--------------|
%% |   {host,      |   Host},      |  "127.0.0.1" |
%% |   {port       |   Port},      |  3306        |
%% |   {user,      |   User},      |  ""          |
%% |   {password,  |   Password},  |  ""          |
%% |   {database,  |   Database},  |  ""          |
%% |   {encoding,  |   Encoding}   |  ""          |
%% |---------------|---------------|--------------|
%% @doc start but not link any name, only compatible with some pool library
-spec start_link(Args :: list()) -> term().
start_link(Args) ->
    start(Args).

%% @doc start
-spec start(Args :: list()) -> term().
start(Args) ->
    Parent = self(),
    Pid = spawn(fun() -> init(Parent, Args) end),
    receive
        {Pid, connected} ->
            {ok, Pid};
        {Pid, {error, Reason}} ->
            {error, Reason};
        {Pid, Reason} ->
            {error, Reason}
    after ?TIMEOUT ->
        timeout
    end.

%% @doc get state
-spec state(pid()) -> term().
state(Pid) ->
    Pid ! {state, self()},
    receive
        {Pid, State} ->
            State
    after ?TIMEOUT ->
        timeout
    end.

%% @doc request
-spec fetch(pid(), list() | binary()) -> term().
fetch(Pid, Request) ->
    Pid ! {fetch, self(), Request},
    receive
        {Pid, Result = #mysql_result{}} ->
            Result;
        {Pid, {error, Reason}} ->
            Reason;
        {Pid, Reason} ->
            Reason
    after ?TIMEOUT ->
        timeout
    end.

-spec handle_result(Sql :: string(), Args :: term(), Result :: term()) -> term().
handle_result(Sql, Args, Result) ->
    handle_result(Sql, Args, Result, fun erlang:throw/1).

-spec handle_result(Sql :: string(), Args :: term(), Result :: term(), ErrorHandler :: function()) -> term().
handle_result(_, _, Result = #mysql_result{type = data}, _) ->
    get_rows(Result);
handle_result(_, insert, Result = #mysql_result{type = updated}, _) ->
    get_insert_id(Result);
handle_result(_, _, Result = #mysql_result{type = updated}, _) ->
    get_affected(Result);
handle_result(Sql, _, Result = #mysql_result{type = error}, ErrorHandler) ->
    ErrorCode = get_code(Result),
    Reason = get_reason(Result),
    %% format exit stack trace info
    ErrorHandler({sql_error, {Sql, ErrorCode, Reason}});
handle_result(Sql, _, Result, ErrorHandler) ->
    %% format exit stack trace info
    ErrorHandler({sql_error, {Sql, Result}}).

%% @doc select row
-spec select(Pid :: pid(), Sql :: string()) -> term().
select(Pid, Sql) ->
    case fetch(Pid, Sql) of
        #mysql_result{type = data, rows = []} ->
            {ok, [[]]};
        #mysql_result{type = data, rows = Rows} ->
            {ok, Rows};
        #mysql_result{type = error, error_code = Code, error_message = Error} ->
            {error, Code, Error};
        Error ->
            Error
    end.

%% @doc insert
-spec insert(Pid :: pid(), Sql :: string()) -> term().
insert(Pid, Sql) ->
    case fetch(Pid, Sql) of
        #mysql_result{type = updated, insert_id = Id} ->
            {ok, Id};
        #mysql_result{type = error, error_code = Code, error_message = Error} ->
            {error, Code, Error};
        Error ->
            Error
    end.

%% @doc update
-spec update(Pid :: pid(), Sql :: string()) -> term().
update(Pid, Sql) ->
    case fetch(Pid, Sql) of
        #mysql_result{type = updated, affected_rows = Affected} ->
            {ok, Affected};
        #mysql_result{type = error, error_code = Code, error_message = Error} ->
            {error, Code, Error};
        Error ->
            Error
    end.

%% @doc delete
-spec delete(Pid :: pid(), Sql :: string()) -> term().
delete(Pid, Sql) ->
    case fetch(Pid, Sql) of
        #mysql_result{type = updated, affected_rows = Affected} ->
            {ok, Affected};
        #mysql_result{type = error, error_code = Code, error_message = Error} ->
            {error, Code, Error};
        Error ->
            Error
    end.

%%   [{Table, Field, Length, Name}]
-spec get_field(Result :: #mysql_result{}) -> [{Table :: atom(), Field :: list(), Length :: non_neg_integer(), Name :: binary()}].
get_field(#mysql_result{field = FieldInfo}) ->
    FieldInfo.

%% @doc Extract the Rows from MySQL Result on data received
%%
%% @spec get_result_rows(MySQLRes::mysql_result()) -> [Row::list()]
-spec get_rows(Result :: #mysql_result{}) -> list().
get_rows(#mysql_result{rows = Rows}) ->
    Rows.

%% @doc Extract the Rows from MySQL Result on update
%%
%% @spec get_result_affected_rows(MySQLRes::mysql_result()) ->
%%           AffectedRows::integer()
-spec get_affected(Result :: #mysql_result{}) -> non_neg_integer().
get_affected(#mysql_result{affected_rows = AffectedRows}) ->
    AffectedRows.

%% @doc Extract the error Reason from MySQL Result on error
%%
%% @spec get_result_reason(MySQLRes::mysql_result()) ->
%%    Reason::string()
-spec get_reason(Result :: #mysql_result{}) -> string().
get_reason(#mysql_result{error_message = Reason}) ->
    Reason.

%% @doc Extract the error ErrCode from MySQL Result on error
%%
%% @spec get_result_err_code(MySQLRes::mysql_result()) ->
%%    ErrCode::integer()
-spec get_code(Result :: #mysql_result{}) -> non_neg_integer().
get_code(#mysql_result{error_code = ErrorCode}) ->
    ErrorCode.

%% @doc Extract the error ErrSqlState from MySQL Result on error
%%
%% @spec get_result_err_sql_state(MySQLRes::mysql_result()) ->
%%    ErrSqlState::string()
-spec get_error_state(Result :: #mysql_result{}) -> string().
get_error_state(#mysql_result{error_state = ErrorSqlState}) ->
    ErrorSqlState.

%% @doc Extract the Insert Id from MySQL Result on update
%%
%% @spec get_result_insert_id(MySQLRes::mysql_result()) ->
%%           InsertId::integer()
-spec get_insert_id(Result :: #mysql_result{}) -> non_neg_integer().
get_insert_id(#mysql_result{insert_id = InsertId}) ->
    InsertId.

%%%===================================================================
%%% Internal functions
%%%===================================================================
init(Parent, ArgList) ->
    case connect(Parent, ArgList) of
        {ok, State} ->
            %% login final
            process_flag(trap_exit, true),
            erlang:send(Parent, {self(), connected}),
            loop(State);
        Error ->
            erlang:send(Parent, {self(), Error})
    end.

%% tcp socket connect
connect(Parent, ArgList) ->
    Host     = proplists:get_value(host,     ArgList, "localhost"),
    Port     = proplists:get_value(port,     ArgList, 3306),
    User     = proplists:get_value(user,     ArgList, ""),
    Password = proplists:get_value(password, ArgList, ""),
    Database = proplists:get_value(database, ArgList, ""),
    Encoding = proplists:get_value(encoding, ArgList, ""),
    State = #state{host = Host, port = Port, parent = Parent, user = User, password = Password, database = Database, encoding = Encoding},
    case gen_tcp:connect(Host, Port, [binary, {packet, 0}, {keepalive, true}]) of
        {ok, Socket} ->
            %% login
            case login(State#state{module = gen_tcp, socket = Socket}) of
                {ok, NewState} ->
                    %% set database and charset
                    set_base(NewState);
                Error ->
                    erlang:send(Parent, {self(), Error})
            end;
        Error ->
            erlang:send(Parent, {self(), Error})
    end.

%% main loop
loop(State = #state{socket = Socket, parent = Parent, data = Data, timeout = Timeout}) ->
    receive
        {fetch, From, Request} ->
            {_, Result} = query(State#state{parent = From}, Request),
            erlang:send(From, {self(), Result}),
            loop(State);
        {state, From} ->
            erlang:send(From, {self(), State}),
            loop(State);
        {tcp, Socket, InData} ->
            NewData = list_to_binary([Data, InData]),
            loop(State#state{data = NewData});
        {tcp_error, Socket, Reason} ->
            erlang:send(Parent, {self(), {error, Reason}});
        {tcp_closed, Socket} ->
            erlang:send(Parent, {self(), {error, closed}})
    after Timeout ->
        erlang:send(Parent, {self(), {error, timeout}})
    end.

%%%====================================================================
%%%  login verify part
%%%====================================================================
%% login
login(State) ->
    case read(State) of
        {ok, NewState = #state{packet = Packet}} ->
            Handshake = decode_handshake(Packet),
            %% switch to ssl if server need
            NewestState = switch_to_ssl(NewState#state{handshake = Handshake}),
            %% switch to ssl handshake
            HandshakePacket = encode_handshake(NewestState),
            FinalState = send_packet(NewestState, HandshakePacket),
            %% enter verify step
            verify(FinalState);
        Error ->
            Error
    end.

%% switch to ssl
switch_to_ssl(State = #state{socket = Socket, handshake = #handshake{capabilities = Capabilities}}) ->
    case Capabilities band ?CLIENT_SSL_SUPPORT =/= 0 of
        true ->
            %% switch to ssl handshake
            Binary = encode_switch_handshake(State),
            NewState = send_packet(State, Binary),
            ssl:start(),
            %% force wrap gen_tcp socket success
            {ok, SSLSocket} = ssl:connect(Socket, [{verify, verify_none}, {versions, ['tlsv1']}], infinity),
            %% force handshake success
            %% ssl_connection:handshake(SSLSocket, infinity),
            NewState#state{module = ssl, socket = SSLSocket};
        false ->
            State
    end.

%% login verify
verify(State) ->
    case read(State) of
        {ok, NewState = #state{packet = <<?OK:8, _Rest/binary>>}} ->
            %% "New auth success
            %% {AffectedRows, Rest1} = decode_packet(Rest),
            %% {InsertId, Rest2} = decode_packet(Rest1),
            %% <<StatusFlags:16/little, WarningCount:16/little, Msg/binary>> = Rest2,
            %% check status, ignoring bit 16#4000, SERVER_SESSION_STATE_CHANGED
            %% and bit 16#0002, SERVER_STATUS_AUTOCOMMIT.
            {ok, NewState};
        {ok, #state{packet = <<?EOF:8>>}} ->
            %% "Old Authentication Method Switch Request Packet consisting of a
            %% single 0xfe byte. It is sent by server to request client to
            %% switch to Old Password Authentication if CLIENT_PLUGIN_AUTH
            %% capability is not supported (by either the client or the server)"
            %% MySQL 4.0 or earlier old auth already unsupported
            throw(unsupported_authentication);
        {ok, NewState = #state{packet = <<?EOF, SwitchData/binary>>, password = Password, handshake = Handshake}} ->
            %% "Authentication Method Switch Request Packet. If both server and
            %% client support CLIENT_PLUGIN_AUTH capability, server can send
            %% this packet to ask client to use another authentication method."
            [Plugin, Salt] = binary:split(SwitchData, <<0>>),
            Binary = hash_password(Password, Salt, Plugin),
            FinalState = send_packet(NewState, Binary),
            verify(FinalState#state{handshake = Handshake#handshake{plugin = Plugin}});
        {ok, NewState = #state{packet = <<1:8, 3:8, _/binary>>}} ->
            %% "Authentication password confirm do not need
            verify(NewState);
        {ok, NewState = #state{packet = <<1:8, 4:8, _/binary>>, password = Password}} ->
            %% "Authentication password confirm full
            verify(send_packet(NewState, <<(unicode:characters_to_binary(Password))/binary, 0:8>>));
        {ok, #state{packet = <<?ERROR:8, Rest/binary>>}} ->
            {error, decode_error_result(Rest)};
        {ok, #state{packet = Packet}} ->
            {error, binary_to_list(Packet)};
        Error ->
            Error
    end.

%%====================================================================
%% login password auth part
%%====================================================================
%% get verify greeting data
decode_handshake(<<10:8, Rest/binary>>) ->
    %% Protocol version 10.
    [Version, Rest1] = binary:split(Rest, <<0>>),
    <<Id:32/little, Salt1:8/binary-unit:8, 0:8, CapabilitiesLower:16/little, CharSet:8, Status:16/little, CapabilitiesUpper:16/little, _SaltLength:8, _Reserved:10/binary-unit:8, Rest2/binary>> = Rest1,
    Capabilities = CapabilitiesLower + 16#10000 * CapabilitiesUpper,
    %% lower half part salt
    [Salt2, Rest3] = binary:split(Rest2, <<0>>),
    %% plugin name
    %% MySQL server 5.5.8 has a bug where end byte is not send
    [Plugin | _] = binary:split(Rest3, <<0>>),
    #handshake{version = Version, id = Id, capabilities = Capabilities, charset = CharSet, status = Status, salt = <<Salt1/binary, Salt2/binary>>, plugin = Plugin}.

%% auth plugin switch response
encode_switch_handshake(#state{handshake = #handshake{capabilities = Capabilities, charset = Charset}}) ->
    Flag = plugin_support(Capabilities, ssl_support(Capabilities, basic_flag())),
    <<Flag:32/little, ?CLIENT_MAX_PACKET_SIZE:32/little, Charset:8, 0:23/unit:8>>.

%% new auth method mysql_native_password support mysql 5.x or later
encode_handshake(#state{user = User, password = Password, database = Database, handshake = #handshake{capabilities = Capabilities, charset = Charset, salt =  Salt, plugin =  Plugin}}) ->
    Hash = hash_password(Password, Salt, Plugin),
    %% connect without database, database capabilities is 0, database capabilities binary is <<<>>
    Flag = plugin_support(Capabilities, ssl_support(Capabilities, basic_flag())),
    <<Flag:32/little, ?CLIENT_MAX_PACKET_SIZE:32/little, Charset:8, 0:23/unit:8, (unicode:characters_to_binary(User))/binary, 0:8, (byte_size(Hash)):8, Hash/binary, (unicode:characters_to_binary(Database))/binary, 0:8, Plugin/binary, 0:8>>.

%% password plugin
%% construct password hash digest
%% https://dev.mysql.com/doc/internals/en/secure-password-authentication.html
hash_password([], _, <<"mysql_native_password">>) ->
    <<>>;
hash_password([], _, <<"caching_sha2_password">>) ->
    <<>>;
hash_password(Password, Salt, <<"mysql_native_password">>) ->
    %% MySQL 4.1 - 5.x default plugin
    Hash = <<HashBinary:160>> = crypto:hash(sha, unicode:characters_to_binary(Password)),
    DoubleHash = crypto:hash(sha, Hash),
    <<FinalBinary:160>> = crypto:hash_final(crypto:hash_update(crypto:hash_update(crypto:hash_init(sha), Salt), DoubleHash)),
    <<(HashBinary bxor FinalBinary):160>>;
hash_password(Password, Salt, <<"caching_sha2_password">>) ->
    %% MySQL 8.x or later default plugin
    Hash = <<HashBinary:256>> = crypto:hash(sha256, unicode:characters_to_binary(Password)),
    DoubleHash = crypto:hash(sha256, Hash),
    <<FinalBinary:256>> = crypto:hash_final(crypto:hash_update(crypto:hash_init(sha256), <<DoubleHash/binary, Salt/binary>>)),
    <<(HashBinary bxor FinalBinary):256>>;
hash_password(_, _, PluginName) ->
    %% unsupported plugin throw directly
    erlang:throw({unsupported_plugin, PluginName}).

%% capabilities flag
basic_flag() ->
    ?CLIENT_LONG_PASSWORD bor ?CLIENT_LONG_FLAG bor ?CLIENT_CONNECT_WITH_DB bor ?CLIENT_PROTOCOL_41 bor ?CLIENT_TRANSACTIONS bor ?CLIENT_SECURE_CONNECTION bor ?CLIENT_MULTI_STATEMENTS bor ?CLIENT_MULTI_RESULTS.

%% capabilities flag support ssl
ssl_support(Capabilities, Basic) ->
    flag_support(Capabilities, Basic, ?CLIENT_SSL_SUPPORT).

%% capabilities flag support plugin auth
plugin_support(Capabilities, Basic) ->
    flag_support(Capabilities, Basic, ?CLIENT_PLUGIN_SUPPORT).

flag_support(Capabilities, Basic, Flag) ->
    case Capabilities band Flag =/= 0 of
        true ->
            Basic bor Flag;
        false ->
            Basic
    end.

%%%====================================================================
%%%  database about part
%%%====================================================================
%% set base
set_base(State) ->
    %% change database/set charset
    case change_database(State) of
        {ok, _} ->
            case set_charset(State) of
                {ok, _} ->
                    {ok, State};
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

%% change database
change_database(#state{database = ""}) ->
    {ok, #mysql_result{type = ok}};
change_database(State = #state{database = Database}) ->
    Query = lists:concat(["use `", Database, "`"]),
    query(State, Query).

%% set charset
set_charset(#state{encoding = ""}) ->
    {ok, #mysql_result{type = ok}};
set_charset(State = #state{encoding = Encoding}) ->
    Query = lists:concat(["set names '", Encoding, "'"]),
    query(State, Query).

%%%====================================================================
%%%  io part
%%%====================================================================
%% send packet
send_packet(State = #state{module = Module, socket = Socket, number = Number}, Packet) ->
    send_packet(Module, Socket, Packet, Number + 1),
    State#state{number = Number + 1}.
send_packet(gen_tcp, Socket, Packet, SequenceNumber) when is_binary(Packet), is_integer(SequenceNumber) ->
    Data = <<(size(Packet)):24/little, SequenceNumber:8, Packet/binary>>,
    gen_tcp:send(Socket, Data);
send_packet(ssl, Socket, Packet, SequenceNumber) when is_binary(Packet), is_integer(SequenceNumber) ->
    Data = <<(size(Packet)):24/little, SequenceNumber:8, Packet/binary>>,
    ssl:send(Socket, Data).

%% read packet with default timeout
read(State = #state{number = Number}) ->
    read(State#state{number = Number + 1}, ?TIMEOUT).

%% first packet receive not check sequence number
read(State = #state{data = <<Length:24/little, 0:8, Packet:Length/binary-unit:8, Rest/binary>>}, _) ->
    NewState = State#state{data = Rest, packet = Packet, number = 0},
    {ok, NewState};
%% other pack must check sequence number
read(State = #state{data = <<Length:24/little, SequenceNumber:8, Packet:Length/binary-unit:8, Rest/binary>>, number = SequenceNumber}, _) ->
    NewState = State#state{data = Rest, packet = Packet, number = SequenceNumber},
    {ok, NewState};
%% read from stream
read(State = #state{socket = Socket, data = Data}, Timeout) ->
    receive
        {ssl, Socket, InData} ->
            read(State#state{data = <<Data/binary, InData/binary>>}, Timeout);
        {tcp, Socket, InData} ->
            read(State#state{data = <<Data/binary, InData/binary>>}, Timeout);
        {tcp_error, Socket, Reason} ->
            {error, Reason};
        {tcp_closed, Socket} ->
            {error, closed};
        Error ->
            {error, Error}
    after Timeout ->
        {error, timeout}
    end.

%%%====================================================================
%%%  query request part
%%%====================================================================
%% query
query(State, Query) ->
    Packet = <<?OP_QUERY, (iolist_to_binary(Query))/binary>>,
    %% query packet sequence number start with 0
    NewState = send_packet(State#state{number = -1}, Packet),
    %% get response now
    handle_query_result(NewState).

%% handle query result
handle_query_result(State) ->
    case read(State) of
        {ok, NewState = #state{packet = Packet}} ->
            {FieldCount, Rest} = decode_integer(Packet),
            case FieldCount of
                ?OK ->
                    %% No Tabular data
                    {AffectedRows, Rest2} = decode_integer(Rest),
                    {InsertId, _} = decode_integer(Rest2),
                    Result = #mysql_result{type = updated, affected_rows = AffectedRows, insert_id = InsertId},
                    {ok, Result};
                ?ERROR ->
                    {error, decode_error_result(Rest)};
                _ ->
                    %% Tabular data received
                    tabular(NewState)
            end;
        Error ->
            Error
    end.

%% receive tabular data
tabular(State) ->
    case decode_fields(State, []) of
        {ok, NewState = #state{fields = Fields}} ->
            case decode_rows(NewState, []) of
                {ok, #state{rows = Rows}} ->
                    Result = #mysql_result{type = data, field = Fields, rows = Rows},
                    {ok, Result};
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

%% get fields
decode_fields(State, List) ->
    case read(State) of
        {ok, NewState = #state{packet = <<?EOF:8>>}} ->
            {ok, NewState#state{fields = lists:reverse(List)}};
        {ok, NewState = #state{packet = <<?EOF:8, Rest/binary>>}} when size(Rest) < 8 ->
            {ok, NewState#state{fields = lists:reverse(List)}};
        {ok, NewState = #state{packet = Packet}} ->
            {_Catalog, Rest} = decode_packet(Packet),
            {_Database, Rest2} = decode_packet(Rest),
            {Table, Rest3} = decode_packet(Rest2),
            %% OrgTable is the real table name if Table is an alias
            {_OrgTable, Rest4} = decode_packet(Rest3),
            {Field, Rest5} = decode_packet(Rest4),
            %% OrgField is the real field name if Field is an alias
            {_OrgField, Rest6} = decode_packet(Rest5),
            %% extract packet
            <<_Metadata:8/little, _Charset:16/little, Length:32/little, Type:8/little, _Flags:16/little, _Decimals:8/little, _Rest7/binary>> = Rest6,
            %% collect one
            This = {Table, Field, Length, decode_type(Type)},
            decode_fields(NewState, [This | List]);
        Error ->
            Error
    end.

%% get rows
decode_rows(State = #state{fields = Fields}, List) ->
    case read(State) of
        {ok, NewState = #state{packet = <<?EOF:8, Rest/binary>>}} when size(Rest) < 8 ->
            {ok, NewState#state{rows = lists:reverse(List)}};
        {ok, #state{packet = <<?ERROR:8, Rest/binary>>}} ->
            {error, decode_error_result(Rest)};
        {ok, NewState = #state{packet = Packet}} ->
            {ok, This} = decode_one(Fields, Packet, []),
            decode_rows(NewState, [This | List]);
        Error ->
            Error
    end.

%% get rows
decode_one([], _Data, List) ->
    {ok, lists:reverse(List)};
decode_one([Field | OtherFields], Data, List) ->
    {Column, Rest} = decode_packet(Data),
    This = format_type(Column, Field),
    decode_one(OtherFields, Rest, [This | List]).

%%%====================================================================
%%%  decode packet part
%%%====================================================================
%% length-encoded-integer
decode_packet(<<251:8, Rest/binary>>) ->
    {null, Rest};
decode_packet(<<Value:8, Packet:Value/binary-unit:8, Other/binary>>) when Value < 251 ->
    {Packet, Other};
decode_packet(<<252:8, Value:16/little, Packet:Value/binary-unit:8, Other/binary>>) ->
    {Packet, Other};
decode_packet(<<253:8, Value:24/little, Packet:Value/binary-unit:8, Other/binary>>) ->
    {Packet, Other};
decode_packet(<<254:8, Value:64/little, Packet:Value/binary-unit:8, Other/binary>>) ->
    {Packet, Other};
decode_packet(<<?ERROR:8, Rest/binary>>) ->
    {?ERROR, Rest}.

%% length-encoded-integer
decode_integer(<<251:8, Rest/binary>>) ->
    {null, Rest};
decode_integer(<<Value:8, Rest/binary>>) when Value < 251 ->
    {Value, Rest};
decode_integer(<<252:8, Value:16/little, Rest/binary>>) ->
    {Value, Rest};
decode_integer(<<253:8, Value:24/little, Rest/binary>>) ->
    {Value, Rest};
decode_integer(<<254:8, Value:64/little, Rest/binary>>) ->
    {Value, Rest};
decode_integer(<<?ERROR:8, Rest/binary>>) ->
    {?ERROR, Rest}.

%% decode error msg
decode_error_message(Packet) ->
    <<Code:16/little, _M:8, State:5/binary, Message/binary>> = Packet,
    {Code, binary_to_list(State), binary_to_list(Message)}.

%% decode error msg
decode_error_result(Packet) ->
    {Code, State, Message} = decode_error_message(Packet),
    #mysql_result{type = error, error_message = Message, error_code = Code, error_state = State}.
%%%====================================================================
%%%  data tool part
%%%====================================================================
%% get type
decode_type(0)                      -> 'DECIMAL';
decode_type(1)                      -> 'TINY';
decode_type(2)                      -> 'SMALL';
decode_type(3)                      -> 'INT';
decode_type(4)                      -> 'FLOAT';
decode_type(5)                      -> 'DOUBLE';
decode_type(6)                      -> 'NULL';
decode_type(7)                      -> 'TIMESTAMP';
decode_type(8)                      -> 'BIG';
decode_type(9)                      -> 'INT24';
decode_type(10)                     -> 'DATE';
decode_type(11)                     -> 'TIME';
decode_type(12)                     -> 'DATE_TIME';
decode_type(13)                     -> 'YEAR';
decode_type(14)                     -> 'NEW_DATE';
decode_type(246)                    -> 'NEW_DECIMAL';
decode_type(247)                    -> 'ENUM';
decode_type(248)                    -> 'SET';
decode_type(249)                    -> 'TINY_BLOB';
decode_type(250)                    -> 'MEDIUM_BLOG';
decode_type(251)                    -> 'LONG_BLOG';
decode_type(252)                    -> 'BLOB';
decode_type(253)                    -> 'VAR_CHAR';
decode_type(254)                    -> 'CHAR';
decode_type(255)                    -> 'GEOMETRY'.

%% convert type
format_type(null,               _)  -> undefined;
format_type(Column,         Field)  -> convert_type(element(4, Field), Column).
%% integer format
convert_type('TINY',        Value)  -> list_to_integer(binary_to_list(Value));
convert_type('SMALL',       Value)  -> list_to_integer(binary_to_list(Value));
convert_type('INT',         Value)  -> list_to_integer(binary_to_list(Value));
convert_type('INT24',       Value)  -> list_to_integer(binary_to_list(Value));
convert_type('BIG',         Value)  -> list_to_integer(binary_to_list(Value));
convert_type('YEAR',        Value)  -> list_to_integer(binary_to_list(Value));
%% timestamp/data_time format
convert_type('TIMESTAMP',   Value)  -> element(2, io_lib:fread("~d-~d-~d ~d:~d:~d", binary_to_list(Value)));
convert_type('DATE_TIME',   Value)  -> element(2, io_lib:fread("~d-~d-~d ~d:~d:~d", binary_to_list(Value)));
%% time format
convert_type('TIME',        Value)  -> element(2, io_lib:fread("~d:~d:~d", binary_to_list(Value)));
%% date format
convert_type('DATE',        Value)  -> element(2, io_lib:fread("~d-~d-~d", binary_to_list(Value)));
%% float double decimal new decimal
convert_type('FLOAT',       Value)  -> binary_to_float(Value);
convert_type('DOUBLE',      Value)  -> binary_to_float(Value);
convert_type('DECIMAL',     Value)  -> convert_type_decimal(Value);
convert_type('NEW_DECIMAL', Value)  -> convert_type_decimal(Value);
%% other
convert_type(_Other,        Value)  -> Value.

%% read float/integer
convert_type_decimal(Value) ->
    convert_type_decimal(Value, <<>>).
convert_type_decimal(<<>>, Binary) ->
    binary_to_integer(Binary);
convert_type_decimal(<<$.:8, Rest/binary>>, Binary) ->
    binary_to_float(<<Binary/binary, $.:8, Rest/binary>>);
convert_type_decimal(<<C:8, Rest/binary>>, Binary) ->
    convert_type_decimal(Rest, <<Binary/binary, C:8>>).
%%%====================================================================
%%%  common tool part
%%%====================================================================
