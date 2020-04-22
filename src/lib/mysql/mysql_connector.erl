%%%-------------------------------------------------------------------
%%% @doc
%%% mysql-connector-erlang
%%% * erlang mysql connector in single file
%%% * connect/query/handle by single process
%%% * support version 4.1 or later (8.x caching_sha2_password plugin supported)
%%% * arguments pass by tuple list(config file compatibility) instead maps(otp 17 early supported)
%%% * nice pool compatibility
%%% * quick and easy to integrate in your project
%%% @end
%%%-------------------------------------------------------------------
-module(mysql_connector).
%% API
%% connector entry
-export([start_link/1]).
%% main query interface
-export([get_state/1, query/2, query/3]).
%% build-in query result handler
-export([handle_result/2, handle_result/3]).
%% normal query interface
-export([select/2, insert/2, update/2, delete/2]).
%% get query result info interface
-export([get_insert_id/1, get_affected_rows/1, get_fields_info/1, get_rows/1, get_error_code/1, get_error_state/1, get_error_message/1]).
%%%-------------------------------------------------------------------
%%% Macros
%%%-------------------------------------------------------------------
%% https://dev.mysql.com/doc/dev/mysql-server/latest/group__group__cs__capabilities__flags.html
%% --- capability flags ---
-define(CLIENT_LONG_PASSWORD,                     (1 bsl 0)).   %% /* new more secure passwords */
-define(CLIENT_FOUND_ROWS,                        (1 bsl 1)).   %% /* Found instead of affected rows */
-define(CLIENT_LONG_FLAG,                         (1 bsl 2)).   %% /* Get all column flags */
-define(CLIENT_CONNECT_WITH_DB,                   (1 bsl 3)).   %% /* One can specify db on connect */
-define(CLIENT_NO_SCHEMA,                         (1 bsl 4)).   %% /* Don't allow database.table.column */
-define(CLIENT_COMPRESS,                          (1 bsl 5)).   %% /* Can use compression protocol */
-define(CLIENT_ODBC,                              (1 bsl 6)).   %% /* Odbc client */
-define(CLIENT_LOCAL_FILES,                       (1 bsl 7)).   %% /* Can use LOAD DATA LOCAL */
-define(CLIENT_IGNORE_SPACE,                      (1 bsl 8)).   %% /* Ignore spaces before '(' */
-define(CLIENT_PROTOCOL_41,                       (1 bsl 9)).   %% /* New 4.1 protocol */
-define(CLIENT_INTERACTIVE,                       (1 bsl 10)).  %% /* This is an interactive client */
-define(CLIENT_SSL,                               (1 bsl 11)).  %% /* Switch to SSL after handshake */
-define(CLIENT_IGNORE_SIGPIPE,                    (1 bsl 12)).  %% /* IGNORE signal pipes */
-define(CLIENT_TRANSACTIONS,                      (1 bsl 13)).  %% /* Client knows about transactions */
-define(CLIENT_RESERVED,                          (1 bsl 14)).  %% /* Old flag for 4.1 protocol  */
-define(CLIENT_RESERVED_2,                        (1 bsl 15)).  %% /* Old flag for 4.1 authentication */
-define(CLIENT_MULTI_STATEMENTS,                  (1 bsl 16)).  %% /* Enable/disable multi-stmt support */
-define(CLIENT_MULTI_RESULTS,                     (1 bsl 17)).  %% /* Enable/disable multi-results */
-define(CLIENT_PS_MULTI_RESULTS,                  (1 bsl 18)).  %% /* Multi-results in PS-protocol */
-define(CLIENT_PLUGIN_AUTH,                       (1 bsl 19)).  %% /* Client supports plugin authentication */
-define(CLIENT_CONNECT_ATTRS,                     (1 bsl 20)).  %% /* Client supports connection attributes */

%% /* Enable authentication response packet to be larger than 255 bytes. */
-define(CLIENT_PLUGIN_AUTH_LEN_ENC_CLIENT_DATA,   (1 bsl 21)).
%% /* Don't close the connection for a connection with expired password. */
-define(CLIENT_CAN_HANDLE_EXPIRED_PASSWORDS,      (1 bsl 22)).

%% /**
%% Capable of handling server state change information. Its a hint to the
%% server to include the state change information in Ok packet.
%% */
-define(CLIENT_SESSION_TRACK,                     (1 bsl 23)).
%% /* Client no longer needs EOF packet */
-define(CLIENT_DEPRECATE_EOF,                     (1 bsl 24)).
-define(CLIENT_OPTIONAL_RESULT_SET_METADATA,      (1 bsl 25)).
-define(CLIENT_Z_STD_COMPRESSION_ALGORITHM,       (1 bsl 26)).
-define(CLIENT_SSL_VERIFY_SERVER_CERT,            (1 bsl 30)).
-define(CLIENT_REMEMBER_OPTIONS,                  (1 bsl 31)).

%% https://dev.mysql.com/doc/dev/mysql-server/latest/page_protocol_command_phase.html
%% -- command --
-define(COM_SLEEP,                 16#00).
-define(COM_QUIT,                  16#01).
-define(COM_INIT_DB,               16#02).
-define(COM_QUERY,                 16#03).
-define(COM_FIELD_LIST,            16#04).
-define(COM_CREATE_DB,             16#05).
-define(COM_DROP_DB,               16#06).
-define(COM_REFRESH,               16#07).
-define(COM_SHUTDOWN,              16#08).
-define(COM_STATISTICS,            16#09).
-define(COM_PROCESS_INFO,          16#0a).
-define(COM_CONNECT,               16#0b).
-define(COM_PROCESS_KILL,          16#0c).
-define(COM_DEBUG,                 16#0d).
-define(COM_PING,                  16#0e).
-define(COM_TIME,                  16#0f).
-define(COM_DELAYED_INSERT,        16#10).
-define(COM_CHANGE_USER,           16#11).
-define(COM_BINLOG_DUMP,           16#12).
-define(COM_TABLE_DUMP,            16#13).
-define(COM_CONNECT_OUT,           16#14).
-define(COM_REGISTER_SLAVE,        16#15).
-define(COM_STMT_PREPARE,          16#16).
-define(COM_STMT_EXECUTE,          16#17).
-define(COM_STMT_SEND_LONG_DATA,   16#18).
-define(COM_STMT_CLOSE,            16#19).
-define(COM_STMT_RESET,            16#1a).
-define(COM_SET_OPTION,            16#1b).
-define(COM_STMT_FETCH,            16#1c).

%%%-------------------------------------------------------------------
%% MySQL Authentication
%% https://dev.mysql.com/doc/dev/mysql-server/latest/page_protocol_basic_character_set.html
%% --- basic character set ---
-define(UTF8MB4_GENERAL_CI,        16#2D). %% utf8mb4_general_ci
-define(UTF8MB4_UNICODE_CI,        16#E0). %% utf8mb4_unicode_ci

%%%-------------------------------------------------------------------
%% https://dev.mysql.com/doc/dev/mysql-server/latest/page_protocol_basic_response_packets.html
%% base response packets
-define(OK,                        16#00). %% 0
-define(EOF,                       16#FE). %% 254
-define(ERROR,                     16#FF). %% 255

%%%-------------------------------------------------------------------
%%% Records
%%%-------------------------------------------------------------------
%% https://dev.mysql.com/doc/dev/mysql-server/latest/page_protocol_basic_ok_packet.html
-record(ok, {
    affected_rows = 0 :: non_neg_integer(),
    insert_id = 0 :: non_neg_integer(),
    status = 0 :: non_neg_integer(),
    warning_count = 0 :: non_neg_integer(),
    message = <<>> :: binary()
}).

%% https://dev.mysql.com/doc/dev/mysql-server/latest/page_protocol_com_query_response.html
-record(data, {
    fields_info = [] :: list(),
    rows = [] :: list()
}).

%% https://dev.mysql.com/doc/dev/mysql-server/latest/page_protocol_basic_eof_packet.html
-record(eof, {
    warning_count = 0 :: non_neg_integer(),
    status = 0 :: non_neg_integer()
}).

%% https://dev.mysql.com/doc/dev/mysql-server/latest/page_protocol_basic_err_packet.html
-record(error, {
    code = 0 :: non_neg_integer(),
    status = <<>> :: binary(),
    message = <<>> :: binary()
}).

%% https://dev.mysql.com/doc/dev/mysql-server/latest/page_protocol_connection_phase.html
%% handshake
-record(handshake, {
    version = <<>> :: binary(),
    id = 0 :: integer(),
    capabilities = 0 :: integer(),
    charset = 0 :: integer(),
    status = 0 :: integer(),
    salt = <<>> :: binary(),
    plugin = <<>> :: binary()
}).

%% mysql connection state
-record(state, {
    socket_type :: gen_tcp | ssl,
    socket :: gen_tcp:socket() | ssl:sslsocket(),
    data = <<>> :: binary(),
    number = 0 :: integer()
}).

%%%===================================================================
%%% API functions
%%%===================================================================

%% mysql connector arguments supported
%% +---------------------+---------------------+---------------------+
%% |    key              |      value          |  default            |
%% +---------------------+---------------------+---------------------+
%% |    host             |      Host           |  <<"localhost">>    |
%% |    port             |      Port           |  3306               |
%% |    user             |      User           |  <<>>               |
%% |    password         |      Password       |  <<>>               |
%% |    database         |      Database       |  <<>>               |
%% |    encoding         |      Encoding       |  <<>>               |
%% +---------------------+---------------------+---------------------+

%% @doc start link
-spec start_link(Args :: list()) -> {ok, pid()}.
start_link(Args) ->
    Parent = self(),
    erlang:process_flag(trap_exit, true),
    Pid = erlang:spawn_link(fun() -> init(Parent, Args) end),
    receive
        {Pid, connected} ->
            {ok, Pid};
        {'EXIT', _, Reason} ->
            erlang:exit(Reason);
        Result ->
            erlang:exit(Result)
    end.

%% @doc get state
-spec get_state(pid()) -> #state{}.
get_state(Pid) ->
    MonitorRef = erlang:monitor(process, Pid),
    erlang:send(Pid, {get_state, self(), MonitorRef}),
    receive
        {MonitorRef, State} ->
            State;
        {'DOWN', MonitorRef, _, _, Reason} ->
            erlang:exit(Reason)
    end.

%% query result
%% +-----------------------------++----------------------------------+
%% | type                        ||   value                          |
%% +-----------------------------++----------------------------------+
%% | tiny/small/medium/int/big   ||   integer                        |
%% | float/double/decimal        ||   integer/float                  |
%% | decimal/new decimal         ||   integer/float                  |
%% | year                        ||   integer                        |
%% | date                        ||   {Y, M, D}                      |
%% | time                        ||   {H, M, S}                      |
%% | datetime/timestamp          ||   {{Y, M, D}, {H, M, S}}         |
%% | char/var/text/blob          ||   binary                         |
%% | json/enum/set/bit/geometry  ||   binary                         |
%% | NULL                        ||   undefined                      |
%% +-----------------------------++----------------------------------+

%% @doc query
-spec query(pid(), list() | binary()) -> term().
query(Pid, Sql) ->
    query(Pid, Sql, infinity).

%% @doc query
-spec query(pid(), list() | binary(), non_neg_integer() | infinity) -> #ok{} | #data{} | #error{}.
query(Pid, Sql, Timeout) ->
    MonitorRef = erlang:monitor(process, Pid),
    erlang:send(Pid, {query, self(), MonitorRef, Sql}),
    receive
        {MonitorRef, Result} ->
            Result;
        {'DOWN', MonitorRef, _, _, Reason} ->
            erlang:exit(Reason)
    after Timeout ->
        #error{message = <<"timeout">>}
    end.

%% @doc handle query result
-spec handle_result(Sql :: list() | binary(), Result :: term()) -> term().
handle_result(Sql, Result) ->
    handle_result(Sql, Result, fun erlang:throw/1).

%% @doc handle query result with spec error handler
-spec handle_result(Sql :: list() | binary(), Result :: term(), ErrorHandler :: function()) -> non_neg_integer() | list() | term().
handle_result(_, #ok{affected_rows = AffectedRows, insert_id = 0}, _) ->
    AffectedRows;
handle_result(_, #ok{insert_id = InsertId}, _) ->
    InsertId;
handle_result(_, #data{rows = Rows}, _) ->
    Rows;
handle_result(Sql, #error{code = Code, message = Message}, ErrorHandler) ->
    %% format exit stack trace info
    ErrorHandler({sql_error, {Sql, Code, Message}});
handle_result(Sql, Result, ErrorHandler) ->
    %% format exit stack trace info
    ErrorHandler({sql_error, {Sql, Result}}).

%% @doc select
-spec select(Pid :: pid(), Sql :: list() | binary()) -> {ok, list()} | {error, non_neg_integer(), binary()} | term().
select(Pid, Sql) ->
    case query(Pid, Sql) of
        #data{rows = Rows} ->
            {ok, Rows};
        #error{code = Code, message = Message} ->
            {error, Code, Message};
        Error ->
            Error
    end.

%% @doc insert
-spec insert(Pid :: pid(), Sql :: list() | binary()) -> {ok, non_neg_integer()} | {error, non_neg_integer(), binary()} | term().
insert(Pid, Sql) ->
    case query(Pid, Sql) of
        #ok{insert_id = InsertId} ->
            {ok, InsertId};
        #error{code = Code, message = Message} ->
            {error, Code, Message};
        Error ->
            Error
    end.

%% @doc update
-spec update(Pid :: pid(), Sql :: list() | binary()) -> {ok, non_neg_integer()} | {error, non_neg_integer(), binary()} | term().
update(Pid, Sql) ->
    case query(Pid, Sql) of
        #ok{affected_rows = AffectedRows} ->
            {ok, AffectedRows};
        #error{code = Code, message = Message} ->
            {error, Code, Message};
        Error ->
            Error
    end.

%% @doc delete
-spec delete(Pid :: pid(), Sql :: list() | binary()) -> {ok, non_neg_integer()} | {error, non_neg_integer(), binary()} | term().
delete(Pid, Sql) ->
    case query(Pid, Sql) of
        #ok{affected_rows = AffectedRows} ->
            {ok, AffectedRows};
        #error{code = Code, message = Message} ->
            {error, Code, Message};
        Error ->
            Error
    end.

%% @doc Extract the Insert Id from MySQL Result on update
-spec get_insert_id(Result :: #ok{}) -> non_neg_integer().
get_insert_id(#ok{insert_id = InsertId}) ->
    InsertId.

%% @doc Extract the Rows from MySQL Result on update
-spec get_affected_rows(Result :: #ok{}) -> non_neg_integer().
get_affected_rows(#ok{affected_rows = AffectedRows}) ->
    AffectedRows.

%% @doc Extract the Fields info from MySQL Result on data received
-spec get_fields_info(Result :: #data{}) -> [{Field :: list(), Type :: non_neg_integer()}].
get_fields_info(#data{fields_info = FieldsInfo}) ->
    FieldsInfo.

%% @doc Extract the Rows from MySQL Result on data received
-spec get_rows(Result :: #data{}) -> list().
get_rows(#data{rows = Rows}) ->
    Rows.

%% @doc Extract the error code from MySQL Result on error
-spec get_error_code(Result :: #error{}) -> non_neg_integer().
get_error_code(#error{code = Code}) ->
    Code.

%% @doc Extract the error sql status from MySQL Result on error
-spec get_error_state(Result :: #error{}) -> binary().
get_error_state(#error{status = Status}) ->
    Status.

%% @doc Extract the error message from MySQL Result on error
-spec get_error_message(Result :: #error{}) -> binary().
get_error_message(#error{message = Message}) ->
    Message.

%%%===================================================================
%%% Internal functions
%%%===================================================================
init(Parent, Args) ->
    process_flag(trap_exit, true),
    %% connect and login
    State = connect(Args),
    %% login succeeded
    erlang:send(Parent, {self(), connected}),
    %% enter main loop
    loop(State#state{data = <<>>, number = 0}).

%% tcp socket connect
connect(Args) ->
    Host     = proplists:get_value(host,     Args, <<"localhost">>),
    Port     = proplists:get_value(port,     Args, 3306),
    User     = proplists:get_value(user,     Args, <<>>),
    Password = proplists:get_value(password, Args, <<>>),
    Database = proplists:get_value(database, Args, <<>>),
    Encoding = proplists:get_value(encoding, Args, <<>>),
    case gen_tcp:connect(Host, Port, [{mode, binary}, {packet, 0}, {active, false}]) of
        {ok, Socket} ->
            %% login
            NewState = login(#state{socket_type = gen_tcp, socket = Socket}, User, Password, Database),
            %% set database and encoding
            set_base(NewState, Database, Encoding),
            %% succeeded
            NewState;
        {error, Reason} ->
            erlang:exit(Reason)
    end.

%% main loop
loop(State) ->
    receive
        {query, From, MonitorRef, Request} ->
            %% normal query
            Result = handle_query(State, Request),
            erlang:send(From, {MonitorRef, Result}),
            loop(State);
        {get_state, From, MonitorRef} ->
            %% get state
            erlang:send(From, {MonitorRef, State}),
            loop(State);
        {'EXIT', _From, Reason} ->
            %% shutdown request, stop it
            quit(State),
            erlang:exit(Reason)
        after 60 * 1000 ->
            %% ping after 60 seconds without operation
            ping(State),
            loop(State)
    end.

%%%===================================================================
%%% login verify part
%%%===================================================================
%% login
login(State, User, Password, Database) ->
    {Packet, NewState} = read(State),
    %% the handshake packet
    Handshake = decode_handshake(Packet),
    %% switch to ssl if server need
    NewestState = switch_to_ssl(NewState, Handshake, User, Password, Database),
    %% switch to ssl handshake
    HandshakePacket = encode_handshake(NewestState, Handshake, User, Password, Database),
    FinalState = send_packet(NewestState, HandshakePacket),
    %% enter verify step
    verify(FinalState, Handshake, Password).

%% switch to ssl
switch_to_ssl(State = #state{socket = Socket}, Handshake = #handshake{capabilities = Capabilities}, User, Password, Database) ->
    case Capabilities band ?CLIENT_SSL =/= 0 of
        true ->
            %% switch to ssl handshake
            Binary = encode_switch_handshake(State, Handshake, User, Password, Database),
            NewState = send_packet(State, Binary),
            %% start ssl application
            ssl:start(),
            %% force wrap gen_tcp socket success
            {ok, SSLSocket} = ssl:connect(Socket, [{verify, verify_none}, {versions, [tlsv1]}]),
            %% force handshake success
            %% ssl_connection:handshake(SSLSocket, infinity),
            NewState#state{socket_type = ssl, socket = SSLSocket};
        false ->
            State
    end.

%% login verify
verify(State, Handshake, Password) ->
    case read(State) of
        {<<?OK:8, _Rest/binary>>, NewState} ->
            %% New auth success
            %% {AffectedRows, Rest1} = decode_packet(Rest),
            %% {InsertId, Rest2} = decode_packet(Rest1),
            %% <<StatusFlags:16/little, WarningCount:16/little, Msg/binary>> = Rest2,
            %% check status, ignoring bit 16#4000, SERVER_SESSION_STATE_CHANGED
            %% and bit 16#0002, SERVER_STATUS_AUTOCOMMIT.
            NewState;
        {<<?EOF:8>>, _NewState} ->
            %% Old Authentication Method Switch Request Packet consisting of a
            %% single 0xfe byte. It is sent by server to request client to
            %% switch to Old Password Authentication if CLIENT_PLUGIN_AUTH
            %% capability is not supported (by either the client or the server)
            %% MySQL 4.0 or earlier old auth already unsupported
            erlang:exit(unsupported_authentication_method);
        {<<?EOF, SwitchData/binary>>, NewState} ->
            %% Authentication Method Switch Request Packet. If both server and
            %% client support CLIENT_PLUGIN_AUTH capability, server can send
            %% this packet to ask client to use another authentication method.
            [Plugin, Salt] = binary:split(SwitchData, <<0>>),
            Binary = encrypt_password(Password, Salt, Plugin),
            FinalState = send_packet(NewState, Binary),
            verify(FinalState, Handshake#handshake{plugin = Plugin}, Password);
        {<<1:8, 3:8, _/binary>>, NewState} ->
            %% Authentication password confirm do not need
            verify(NewState, Handshake, Password);
        {<<1:8, 4:8, _/binary>>, NewState} ->
            %% Authentication password confirm full
            Binary = <<(unicode:characters_to_binary(Password))/binary, 0:8>>,
            FinalState = send_packet(NewState, Binary),
            verify(FinalState, Handshake, Password);
        {<<?ERROR:8, Rest/binary>>, _NewState} ->
            %% verify failed, user or password invalid
            erlang:exit(decode_error_packet(Rest));
        {Packet, _NewState} ->
            %% unknown packet
            erlang:exit({unknown_packet, Packet})
    end.

%%%===================================================================
%%% login password auth part
%%%===================================================================
%% get verify greeting data
decode_handshake(<<10:8, Rest/binary>>) ->
    %% Protocol version 10.
    [Version, Rest1] = binary:split(Rest, <<0>>),
    <<ConnectionId:32/little, Salt1:8/binary-unit:8, 0:8, CapabilitiesLower:16/little, CharSet:8, Status:16/little, CapabilitiesUpper:16/little, _SaltLength:8, _Reserved:10/binary-unit:8, Rest2/binary>> = Rest1,
    Capabilities = CapabilitiesLower + 16#10000 * CapabilitiesUpper,
    %% lower half part salt
    [Salt2, Rest3] = binary:split(Rest2, <<0>>),
    %% plugin name
    %% MySQL server 5.5.8 has a bug where end byte is not send
    [Plugin | _] = binary:split(Rest3, <<0>>),
    #handshake{version = Version, id = ConnectionId, capabilities = Capabilities, charset = CharSet, status = Status, salt = <<Salt1/binary, Salt2/binary>>, plugin = Plugin}.

%% authentication plugin switch response
encode_switch_handshake(#state{}, #handshake{capabilities = Capabilities, charset = Charset}, _, _, _) ->
    Flag = plugin_support(Capabilities, ssl_support(Capabilities, basic_flag())),
    <<Flag:32/little, ?CLIENT_SSL_VERIFY_SERVER_CERT:32/little, Charset:8, 0:23/unit:8>>.

%% new authentication method mysql_native_password support mysql 5.x or later
encode_handshake(#state{}, #handshake{capabilities = Capabilities, charset = Charset, salt =  Salt, plugin =  Plugin}, User, Password, Database) ->
    %% add authentication plugin support and ssl support if server need
    Flag = plugin_support(Capabilities, ssl_support(Capabilities, basic_flag())),
    %% user name
    UserBinary = <<(unicode:characters_to_binary(User))/binary, 0:8>>,
    %% database
    DatabaseBinary = <<(unicode:characters_to_binary(Database))/binary, 0:8>>,
    %% authentication plugin
    PluginBinary = <<(unicode:characters_to_binary(Plugin))/binary, 0:8>>,
    %% password encrypt
    PasswordBinary = encrypt_password(Password, Salt, Plugin),
    <<Flag:32/little, ?CLIENT_SSL_VERIFY_SERVER_CERT:32/little, Charset:8, 0:23/unit:8, UserBinary/binary, PasswordBinary/binary, DatabaseBinary/binary, PluginBinary/binary>>.

%% password authentication plugin
%% construct password hash digest
%% https://dev.mysql.com/doc/internals/en/secure-password-authentication.html
encrypt_password([], _, <<"mysql_native_password">>) ->
    encode_string(<<>>);
encrypt_password([], _, <<"caching_sha2_password">>) ->
    encode_string(<<>>);
encrypt_password(Password, Salt, <<"mysql_native_password">>) ->
    %% MySQL 4.1 - 5.x default plugin
    Hash = <<HashBinary:160>> = crypto:hash(sha, unicode:characters_to_binary(Password)),
    DoubleHash = crypto:hash(sha, Hash),
    <<FinalBinary:160>> = crypto:hash_final(crypto:hash_update(crypto:hash_update(crypto:hash_init(sha), Salt), DoubleHash)),
    %% hash length 8 bit
    encode_string(<<(HashBinary bxor FinalBinary):160>>);
encrypt_password(Password, Salt, <<"caching_sha2_password">>) ->
    %% MySQL 8.x or later default plugin
    Hash = <<HashBinary:256>> = crypto:hash(sha256, unicode:characters_to_binary(Password)),
    DoubleHash = crypto:hash(sha256, Hash),
    <<FinalBinary:256>> = crypto:hash_final(crypto:hash_update(crypto:hash_init(sha256), <<DoubleHash/binary, Salt/binary>>)),
    %% hash length 8 bit
    encode_string(<<(HashBinary bxor FinalBinary):256>>);
encrypt_password(_, _, PluginName) ->
    %% unsupported plugin exit directly
    erlang:exit({unsupported_plugin, PluginName}).

%% capabilities flag
basic_flag() ->
    ?CLIENT_LONG_PASSWORD bor ?CLIENT_LONG_FLAG bor ?CLIENT_CONNECT_WITH_DB bor ?CLIENT_PROTOCOL_41 bor ?CLIENT_TRANSACTIONS bor ?CLIENT_RESERVED_2 bor ?CLIENT_MULTI_STATEMENTS bor ?CLIENT_MULTI_RESULTS.

%% capabilities flag support ssl
ssl_support(Capabilities, Basic) ->
    flag_support(Capabilities, Basic, ?CLIENT_SSL).

%% capabilities flag support plugin auth
plugin_support(Capabilities, Basic) ->
    flag_support(Capabilities, Basic, ?CLIENT_PLUGIN_AUTH).

flag_support(Capabilities, Basic, Flag) ->
    case Capabilities band Flag =/= 0 of
        true ->
            Basic bor Flag;
        false ->
            Basic
    end.

%%%===================================================================
%%% database about part
%%%===================================================================
%% set base
set_base(State, Database, Encoding) ->
    %% change database
    ChangeDatabaseResult = change_database(State, Database),
    not is_record(ChangeDatabaseResult, ok) andalso erlang:exit(ChangeDatabaseResult),
    %% set encoding
    SetEncodingResult = set_encoding(State, Encoding),
    not is_record(ChangeDatabaseResult, ok) andalso erlang:exit(SetEncodingResult).

%% change database
change_database(_State, []) ->
    #ok{};
change_database(_State, <<>>) ->
    #ok{};
change_database(State, Database) ->
    Query = <<"USE `", (unicode:characters_to_binary(Database))/binary, "`">>,
    handle_query(State, Query).

%% set encoding
set_encoding(_State, []) ->
    #ok{};
set_encoding(_State, <<>>) ->
    #ok{};
set_encoding(State, Encoding) ->
    Query = <<"SET NAMES '", (unicode:characters_to_binary(Encoding))/binary, "'">>,
    handle_query(State, Query).

%%%===================================================================
%%% io part
%%%===================================================================
%% send packet
send_packet(State = #state{socket_type = Module, socket = Socket, number = Number}, Packet) ->
    send_packet(Module, Socket, Packet, Number + 1),
    State#state{number = Number + 1}.

%% send it
send_packet(gen_tcp, Socket, Packet, SequenceNumber) when is_binary(Packet), is_integer(SequenceNumber) ->
    Data = <<(byte_size(Packet)):24/little, SequenceNumber:8, Packet/binary>>,
    gen_tcp:send(Socket, Data);
send_packet(ssl, Socket, Packet, SequenceNumber) when is_binary(Packet), is_integer(SequenceNumber) ->
    Data = <<(byte_size(Packet)):24/little, SequenceNumber:8, Packet/binary>>,
    ssl:send(Socket, Data).

%% read packet with default timeout
read(State = #state{number = Number}) ->
    read(State#state{number = Number + 1}, infinity).

%% https://dev.mysql.com/doc/dev/mysql-server/latest/page_protocol_basic_packets.html
%% mysql packets
%% first packet receive not check sequence number
read(State = #state{data = <<Length:24/little, 0:8, Packet:Length/binary-unit:8, Rest/binary>>}, _) ->
    NewState = State#state{data = Rest, number = 0},
    {Packet, NewState};
%% other pack must check sequence number
read(State = #state{data = <<Length:24/little, SequenceNumber:8, Packet:Length/binary-unit:8, Rest/binary>>, number = SequenceNumber}, _) ->
    NewState = State#state{data = Rest, number = SequenceNumber},
    {Packet, NewState};
%% read from stream
read(State = #state{socket_type = gen_tcp, socket = Socket, data = Data}, Timeout) ->
    case gen_tcp:recv(Socket, 0, Timeout) of
        {ok, InData} ->
            read(State#state{data = <<Data/binary, InData/binary>>}, Timeout);
        {error, Reason} ->
            erlang:exit(Reason)
    end;
read(State = #state{socket_type = ssl, socket = Socket, data = Data}, Timeout) ->
    case ssl:recv(Socket, 0, Timeout) of
        {ok, InData} ->
            read(State#state{data = <<Data/binary, InData/binary>>}, Timeout);
        {error, Reason} ->
            erlang:exit(Reason)
    end.

%%%===================================================================
%%% ping
%%%===================================================================
%% ping
ping(State) ->
    Packet = <<?COM_PING>>,
    %% query packet sequence number start with 0
    NewState = send_packet(State#state{data = <<>>, number = -1}, Packet),
    %% get response now
    PingResult = handle_query_result(NewState),
    not is_record(PingResult, ok) andalso erlang:exit(PingResult).

%%%===================================================================
%%% quit
%%%===================================================================
%% quit
quit(State) ->
    Packet = <<?COM_QUIT>>,
    %% Server closes the connection or returns ERR_Packet.
    send_packet(State#state{data = <<>>, number = -1}, Packet).

%%%===================================================================
%%% query request part
%%%===================================================================
%% query
handle_query(State, Query) ->
    Packet = <<?COM_QUERY, (iolist_to_binary(Query))/binary>>,
    %% query packet sequence number start with 0
    NewState = send_packet(State#state{data = <<>>, number = -1}, Packet),
    %% get response now
    handle_query_result(NewState).

%% handle query result
handle_query_result(State) ->
    case read(State) of
        {<<?OK:8, Rest/binary>>, _} ->
            decode_ok_packet(Rest);
        {<<?EOF:8, Rest/binary>>, _} ->
            decode_eof_packet(Rest);
        {<<?ERROR:8, Rest/binary>>, _} ->
            decode_error_packet(Rest);
        {_, NewState} ->
            %% tabular data decode
            %% {FieldCount, <<>>} = decode_integer(Packet),
            {FieldsInfo, NewestState} = decode_fields_info(NewState, []),
            decode_rows(NewestState, FieldsInfo, [])
    end.

%% decode fields info, read n field packet, read an eof packet
decode_fields_info(State, List) ->
    case read(State) of
        {<<?EOF:8, _:4/binary>>, NewState} ->
            %% eof packet
            %% if (not capabilities & CLIENT_DEPRECATE_EOF)
            %% https://dev.mysql.com/doc/dev/mysql-server/latest/page_protocol_com_query_response_text_resultset.html
            {lists:reverse(List), NewState};
        {Packet, NewState} ->
            %% column definition
            %% https://dev.mysql.com/doc/dev/mysql-server/latest/page_protocol_com_query_response_text_resultset_column_definition.html
            {_Catalog, Rest} = decode_string(Packet),
            {_Database, Rest2} = decode_string(Rest),
            {_Table, Rest3} = decode_string(Rest2),
            %% OrgTable is the real table name if Table is an alias
            {_OriginTable, Rest4} = decode_string(Rest3),
            {Name, Rest5} = decode_string(Rest4),
            %% OrgField is the real field name if Field is an alias
            {_OriginField, Rest6} = decode_string(Rest5),
            %% extract packet
            %% character set
            %% https://dev.mysql.com/doc/dev/mysql-server/latest/page_protocol_basic_character_set.html
            %% flags
            %% https://dev.mysql.com/doc/dev/mysql-server/latest/group__group__cs__column__definition__flags.html
            <<_Metadata:8/little, _CharacterSet:16/little, _Length:32/little, Type:8/little, Flags:16/little, Decimals:8/little, _Rest7/binary>> = Rest6,
            %% collect one
            This = {Name, Type, Flags, Decimals},
            decode_fields_info(NewState, [This | List])
    end.

%% decode rows, read n field packet, read an eof packet
decode_rows(State, FieldsInfo, List) ->
    case read(State) of
        {<<?EOF:8, _:4/binary>>, _NewState} ->
            %% if capabilities & CLIENT_DEPRECATE_EOF
            %% ok packet
            %% else eof packet
            %% https://dev.mysql.com/doc/dev/mysql-server/latest/page_protocol_com_query_response_text_resultset.html
            #data{fields_info = FieldsInfo, rows = lists:reverse(List)};
        {<<?ERROR:8, Rest/binary>>, _NewState} ->
            decode_error_packet(Rest);
        {Packet, NewState} ->
            This = decode_fields(FieldsInfo, Packet, []),
            decode_rows(NewState, FieldsInfo, [This | List])
    end.

%% decode field
decode_fields([], _, List) ->
    lists:reverse(List);
decode_fields([{_, Type, _, _} | Fields], Packet, List) ->
    {Column, Rest} = decode_string(Packet),
    This = convert_type(Type, Column),
    decode_fields(Fields, Rest, [This | List]).

%%%===================================================================
%%% decode packet part
%%%===================================================================

%% https://dev.mysql.com/doc/dev/mysql-server/latest/page_protocol_basic_dt_integers.html
%% length-decoded-integer
decode_integer(<<Value:8, Rest/binary>>) when Value < 16#FB ->
    {Value, Rest};
decode_integer(<<251:8, Rest/binary>>) ->
    {undefined, Rest};
decode_integer(<<16#FC:8, Value:16/little, Rest/binary>>) ->
    {Value, Rest};
decode_integer(<<16#FD:8, Value:24/little, Rest/binary>>) ->
    {Value, Rest};
decode_integer(<<16#FE:8, Value:64/little, Rest/binary>>) ->
    {Value, Rest}.

%% length-encode-integer
encode_integer(Value) when Value < 16#FB ->
    <<Value>>;
encode_integer(Value) when Value =< 16#FFFF ->
    <<16#FC:8, Value:16/little>>;
encode_integer(Value) when Value =< 16#FFFFFF ->
    <<16#FD:8, Value:24/little>>;
encode_integer(Value) when Value =< 16#FFFFFFFFFFFFFFFF ->
    <<16#FE:8, Value:64/little>>.

%% https://dev.mysql.com/doc/dev/mysql-server/latest/page_protocol_basic_dt_strings.html
%% length-decoded-string
decode_string(<<Length:8, Value:Length/binary, Rest/binary>>) when Length < 16#FB ->
    {Value, Rest};
decode_string(<<16#FB:8, Rest/binary>>) ->
    {undefined, Rest};
decode_string(<<16#FC:8, Length:16/little, Value:Length/binary, Rest/binary>>) ->
    {Value, Rest};
decode_string(<<16#FD:8, Length:24/little, Value:Length/binary, Rest/binary>>) ->
    {Value, Rest};
decode_string(<<16#FE:8, Length:64/little, Value:Length/binary, Rest/binary>>) ->
    {Value, Rest}.

%% length-encode-string
encode_string(Value) ->
    <<(encode_integer(byte_size(Value)))/binary, Value/binary>>.

%% decode ok packet
decode_ok_packet(Packet) ->
    {AffectedRows, Rest2} = decode_integer(Packet),
    {InsertId, Rest3} = decode_integer(Rest2),
    <<Status:16/little, WarningCount:16/little, Message/binary>> = Rest3,
    #ok{affected_rows = AffectedRows, insert_id = InsertId, warning_count = WarningCount, status = Status, message = Message}.

%% decode eof result
decode_eof_packet(<<WarningCount:16/little, Status:16/little>>) ->
    #eof{warning_count = WarningCount, status = Status}.

%% decode error packet
decode_error_packet(<<Code:16/little, Status:6/binary-unit:8, Message/binary>>) ->
    #error{code = Code, status = Status, message = Message}.

%%%===================================================================
%%% data tool part
%%%===================================================================
%% bin log type define in mysql release include/binary_log_types.h
-define(MYSQL_TYPE_DECIMAL,           0).
-define(MYSQL_TYPE_TINY,              1).
-define(MYSQL_TYPE_SHORT,             2).
-define(MYSQL_TYPE_LONG,              3).
-define(MYSQL_TYPE_FLOAT,             4).
-define(MYSQL_TYPE_DOUBLE,            5).
-define(MYSQL_TYPE_NULL,              6).
-define(MYSQL_TYPE_TIMESTAMP,         7).
-define(MYSQL_TYPE_LONGLONG,          8).
-define(MYSQL_TYPE_INT24,             9).
-define(MYSQL_TYPE_DATE,              10).
-define(MYSQL_TYPE_TIME,              11).
-define(MYSQL_TYPE_DATETIME,          12).
-define(MYSQL_TYPE_YEAR,              13).
-define(MYSQL_TYPE_NEW_DATE,          14).
-define(MYSQL_TYPE_VARCHAR,           15).
-define(MYSQL_TYPE_BIT,               16).
-define(MYSQL_TYPE_TIMESTAMP2,        17).
-define(MYSQL_TYPE_DATETIME2,         18).
-define(MYSQL_TYPE_TIME2,             19).
-define(MYSQL_TYPE_JSON,              245).
-define(MYSQL_TYPE_NEW_DECIMAL,       246).
-define(MYSQL_TYPE_ENUM,              247).
-define(MYSQL_TYPE_SET,               248).
-define(MYSQL_TYPE_TINY_BLOB,         249).
-define(MYSQL_TYPE_MEDIUM_BLOB,       250).
-define(MYSQL_TYPE_LONG_BLOB,         251).
-define(MYSQL_TYPE_BLOB,              252).
-define(MYSQL_TYPE_VAR_STRING,        253).
-define(MYSQL_TYPE_STRING,            254).
-define(MYSQL_TYPE_GEOMETRY,          255).

%% integer format
convert_type(_,                       undefined) -> undefined;
convert_type(?MYSQL_TYPE_DECIMAL,     Value)     -> try binary_to_float(Value) catch _:_ -> binary_to_integer(Value) end;
convert_type(?MYSQL_TYPE_TINY,        Value)     -> binary_to_integer(Value);
convert_type(?MYSQL_TYPE_SHORT,       Value)     -> binary_to_integer(Value);
convert_type(?MYSQL_TYPE_LONG,        Value)     -> binary_to_integer(Value);
convert_type(?MYSQL_TYPE_FLOAT,       Value)     -> try binary_to_float(Value) catch _:_ -> binary_to_integer(Value) end;
convert_type(?MYSQL_TYPE_DOUBLE,      Value)     -> try binary_to_float(Value) catch _:_ -> binary_to_integer(Value) end;
convert_type(?MYSQL_TYPE_NULL,        Value)     -> Value;
convert_type(?MYSQL_TYPE_TIMESTAMP,   <<Y:4/binary, "-", Mo:2/binary, "-", D:2/binary, " ", H:2/binary, ":", Mi:2/binary, ":", S:2/binary>>) -> {{binary_to_integer(Y), binary_to_integer(Mo), binary_to_integer(D)}, {binary_to_integer(H), binary_to_integer(Mi), binary_to_integer(S)}};
convert_type(?MYSQL_TYPE_LONGLONG,    Value)     -> binary_to_integer(Value);
convert_type(?MYSQL_TYPE_INT24,       Value)     -> binary_to_integer(Value);
convert_type(?MYSQL_TYPE_DATE,        <<Y:4/binary, "-", M:2/binary, "-", D:2/binary>>) -> {binary_to_integer(Y), binary_to_integer(M), binary_to_integer(D)};
convert_type(?MYSQL_TYPE_TIME,        <<H:2/binary, ":", M:2/binary, ":", S:2/binary>>) -> {binary_to_integer(H), binary_to_integer(M), binary_to_integer(S)};
convert_type(?MYSQL_TYPE_DATETIME,    <<Y:4/binary, "-", Mo:2/binary, "-", D:2/binary, " ", H:2/binary, ":", Mi:2/binary, ":", S:2/binary>>) -> {{binary_to_integer(Y), binary_to_integer(Mo), binary_to_integer(D)}, {binary_to_integer(H), binary_to_integer(Mi), binary_to_integer(S)}};
convert_type(?MYSQL_TYPE_YEAR,        Value)     -> binary_to_integer(Value);
convert_type(?MYSQL_TYPE_NEW_DATE,    Value)     -> Value;
convert_type(?MYSQL_TYPE_VARCHAR,     Value)     -> Value;
convert_type(?MYSQL_TYPE_BIT,         Value)     -> Value;
convert_type(?MYSQL_TYPE_JSON,        Value)     -> Value;
convert_type(?MYSQL_TYPE_NEW_DECIMAL, Value)     -> try binary_to_float(Value) catch _:_ -> binary_to_integer(Value) end;
convert_type(?MYSQL_TYPE_ENUM,        Value)     -> Value;
convert_type(?MYSQL_TYPE_SET,         Value)     -> Value;
convert_type(?MYSQL_TYPE_TINY_BLOB,   Value)     -> Value;
convert_type(?MYSQL_TYPE_MEDIUM_BLOB, Value)     -> Value;
convert_type(?MYSQL_TYPE_LONG_BLOB,   Value)     -> Value;
convert_type(?MYSQL_TYPE_BLOB,        Value)     -> Value;
convert_type(?MYSQL_TYPE_VAR_STRING,  Value)     -> Value;
convert_type(?MYSQL_TYPE_STRING,      Value)     -> Value;
convert_type(?MYSQL_TYPE_GEOMETRY,    Value)     -> Value;
convert_type(Type,                        _)     -> erlang:exit({unknown_field_type, Type}).
