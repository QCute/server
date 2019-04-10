-module(mysql_driver).
-export([fetch/2, start_link/1, state/1]).
-export([handle_result/3, handle_result/4]).
-export([get_field/1, get_rows/1, get_affected/1, get_reason/1, get_code/1, get_error_state/1, get_insert_id/1]).
-export([select/2, insert/2, update/2, delete/2]).
%%%-------------------------------------------------------------------
%%% Macros
%%%-------------------------------------------------------------------
-define(TIMEOUT,                5000).  %% query default timeout

%%% ------------------------------------------------------------------
%%% MySQL Commands
-define(OP_SLEEP,               16#00).
-define(OP_QUIT,                16#01).
-define(OP_INIT_DB,             16#02).
-define(OP_QUERY,               16#03).
-define(OP_FIELD_LIST,          16#04).
-define(OP_CREATE_DB,           16#05).
-define(OP_DROP_DB,             16#06).
-define(OP_REFRESH,             16#07).
-define(OP_SHUTDOWN,            16#08).
-define(OP_STATISTICS,          16#09).
-define(OP_PROCESS_INFO,        16#0a).
-define(OP_CONNECT,             16#0b).
-define(OP_PROCESS_KILL,        16#0c).
-define(OP_DEBUG,               16#0d).
-define(OP_PING,                16#0e).
-define(OP_TIME,                16#0f).
-define(OP_DELAYED_INSERT,      16#10).
-define(OP_CHANGE_USER,         16#11).
-define(OP_BINLOG_DUMP,         16#12).
-define(OP_TABLE_DUMP,          16#13).
-define(OP_CONNECT_OUT,         16#14).
-define(OP_REGISTER_SLAVE,      16#15).
-define(OP_STMT_PREPARE,        16#16).
-define(OP_STMT_EXECUTE,        16#17).
-define(OP_STMT_SEND_LONG_DATA, 16#18).
-define(OP_STMT_CLOSE,          16#19).
-define(OP_STMT_RESET,          16#1a).
-define(OP_SET_OPTION,          16#1b).
-define(OP_STMT_FETCH,          16#1c).


-define(LONG_PASSWORD,          16#00000001).  %% 1
-define(LONG_FLAG,              16#00000004).  %% 4
-define(CONNECT_WITH_DB,        16#00000008).  %% 8
-define(PROTOCOL_41,            16#00000200).  %% 512

-define(TRANSACTIONS,           16#00002000).  %% 8192
-define(SECURE_CONNECTION,      16#00008000).  %% 32768
-define(MAX_PACKET_SIZE,        16#1000000).   %%

%% Response packet tag (first byte)
-define(OK,                     16#00).  %% 0
-define(EOF,                    16#fe).  %% 254
-define(ERROR,                  16#ff).  %% 255

%%%-------------------------------------------------------------------
%%% Records
%%%-------------------------------------------------------------------
%% mysql result record
-record(mysql_result, {
    type,
    field = [],
    rows = [],
    affected_rows = 0,
    insert_id = 0,
    error_code = 0,
    error_message = "",
    error_state = ""
}).

-record(handshake, {
    version :: [integer()],
    id :: integer(),
    capabilities :: integer(),
    charset :: integer(),
    status :: integer(),
    salt :: binary(),
    plugin :: binary()
}).

%% mysql connection state
-record(state, {
    socket,
    parent,
    host,
    port,
    user,
    password,
    database,
    encoding,
    result,
    timeout = infinity,
    data = <<>>,
    handshake,
    packet = <<>>,
    number = 0,
    fields = [],
    rows = []
}).

%%====================================================================
%% API functions
%%====================================================================
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

%% @doc start link
-spec start_link(list()) -> term().
start_link(List) ->
    Parent = self(),
    Pid = spawn(fun() -> init(Parent, List) end),
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
get_field(#mysql_result{field = FieldInfo}) ->
    FieldInfo.

%% @doc Extract the Rows from MySQL Result on data received
%%
%% @spec get_result_rows(MySQLRes::mysql_result()) -> [Row::list()]
get_rows(#mysql_result{rows = Rows}) ->
    Rows.

%% @doc Extract the Rows from MySQL Result on update
%%
%% @spec get_result_affected_rows(MySQLRes::mysql_result()) ->
%%           AffectedRows::integer()
get_affected(#mysql_result{affected_rows = AffectedRows}) ->
    AffectedRows.

%% @doc Extract the error Reason from MySQL Result on error
%%
%% @spec get_result_reason(MySQLRes::mysql_result()) ->
%%    Reason::string()
get_reason(#mysql_result{error_message = Reason}) ->
    Reason.

%% @doc Extract the error ErrCode from MySQL Result on error
%%
%% @spec get_result_err_code(MySQLRes::mysql_result()) ->
%%    ErrCode::integer()
get_code(#mysql_result{error_code = ErrorCode}) ->
    ErrorCode.

%% @doc Extract the error ErrSqlState from MySQL Result on error
%%
%% @spec get_result_err_sql_state(MySQLRes::mysql_result()) ->
%%    ErrSqlState::string()
get_error_state(#mysql_result{error_state = ErrorSqlState}) ->
    ErrorSqlState.

%% @doc Extract the Insert Id from MySQL Result on update
%%
%% @spec get_result_insert_id(MySQLRes::mysql_result()) ->
%%           InsertId::integer()
get_insert_id(#mysql_result{insert_id = InsertId}) ->
    InsertId.

%%%====================================================================
%%%  Internal functions
%%%====================================================================
init(Parent, ArgList) ->
    case connect(Parent, ArgList) of
        {ok, State} ->
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
            case login(State#state{socket = Socket}) of
                {ok, NewState} ->
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
        {ok, NewState = #state{packet = Packet, user = User, password = Password, number = Number}} ->
            Handshake = #handshake{salt = Salt, plugin = Plugin} = encode_handshake(Packet),
            case pack_handshake(User, Password, Salt, Plugin) of
                {ok, HandshakePacket} ->
                    send_packet(NewState, HandshakePacket, Number + 1),
                    verify(NewState#state{handshake = Handshake, number = Number + 1});
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

%% login verify
verify(State) ->
    case read(State) of
        {ok, NewState = #state{packet = <<?OK:8, _Rest/binary>>}} ->
            %% use new auth
            {ok, NewState};
        {ok, #state{packet = <<?EOF:8>>}} ->
            %% old auth already unsupported
            {error, unsupported_auth};
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
encode_handshake(<<_Protocol:8, Rest/binary>>) ->
    %% Protocol version 10.
    [Version, Rest1] = binary:split(Rest, <<0>>),
    <<Id:32/little,                        %% connection id
        Salt1:8/binary-unit:8,             %% salt upper half part
        0:8,                               %% "filler" -- everything below is optional
        CapabilitiesLower:16/little,       %%
        CharSet:8,                         %% server charset
        Status:16/little,                  %% server status
        CapabilitiesUpper:16/little,       %%
        _SaltLength:8,                     %% if capabilities & CLIENT_PLUGIN_AUTH, otherwise 0
        _Reserved:10/binary-unit:8,        %% 10 unused (reserved) bytes
        Rest2/binary>> = Rest1,
    Capabilities = CapabilitiesLower + 16#10000 * CapabilitiesUpper,
    [Salt2, Rest3] = binary:split(Rest2, <<0>>),
    [Name | _] = binary:split(Rest3, <<0>>),
    #handshake{version = Version, id = Id, capabilities = Capabilities, charset = CharSet, status = Status, salt = <<Salt1/binary, Salt2/binary>>, plugin = Name}.

%% new auth method mysql_native_password support mysql 5.x or later
pack_handshake(User, Password, Salt, PluginName) ->
    case hash_password(Password, Salt, PluginName) of
        {ok, Hash} ->
            %% connect without database, database capabilities is 0, database capabilities binary is <<<>>
            Capability = ?LONG_PASSWORD bor ?LONG_FLAG bor ?TRANSACTIONS bor ?PROTOCOL_41 bor ?SECURE_CONNECTION bor 0,
            Packet = <<Capability:32/little, ?MAX_PACKET_SIZE:32/little, 8:8, 0:23/integer-unit:8, (iolist_to_binary(User))/binary, ?OK:8, (size(Hash)):8, Hash/binary, <<>>/binary>>,
            {ok, Packet};
        Error ->
            Error
    end.

%% construct password hash digest
%% mysql 8.x caching_sha2_password by default will failed
hash_password([], _, <<"mysql_native_password">>) ->
    {ok, <<>>};
hash_password(Password, Salt, <<"mysql_native_password">>) ->
    Hash = crypto:hash(sha, Password),
    DoubleHash = crypto:hash(sha, Hash),
    Final = crypto:hash_final(crypto:hash_update(crypto:hash_update(crypto:hash_init(sha), Salt), DoubleHash)),
    Binary = list_to_binary(lists:zipwith(fun (E1, E2) -> E1 bxor E2 end, binary_to_list(Final), binary_to_list(Hash))),
    {ok, Binary};
hash_password(_, _, PluginName) ->
    {error, {unsupported_plugin, PluginName}}.

%%%====================================================================
%%%  database about part
%%%====================================================================
%% set base
set_base(State = #state{parent = Parent}) ->
    %% change database/set charset
    case change_database(State) of
        {ok, _} ->
            case set_charset(State) of
                {ok, _} ->
                    erlang:send(Parent, {self(), connected}),
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
send_packet(#state{socket = Socket}, Packet, SequenceNumber) when is_binary(Packet), is_integer(SequenceNumber) ->
    Data = <<(size(Packet)):24/little, SequenceNumber:8, Packet/binary>>,
    gen_tcp:send(Socket, Data).

%% read packet
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
read(State = #state{socket = Socket, data = Data}, Timeout) ->
    receive
        {tcp, Socket, InData} ->
            NewData = list_to_binary([Data, InData]),
            read(State#state{data = NewData}, Timeout);
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
    send_packet(State, Packet, 0),
    %% get response now
    handle_query_result(State#state{number = 0}).

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
                {error, Result} ->
                    {error, Result};
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
decode_type(0)                     -> 'DECIMAL';
decode_type(1)                     -> 'TINY';
decode_type(2)                     -> 'SHORT';
decode_type(3)                     -> 'LONG';
decode_type(4)                     -> 'FLOAT';
decode_type(5)                     -> 'DOUBLE';
decode_type(6)                     -> 'NULL';
decode_type(7)                     -> 'TIMESTAMP';
decode_type(8)                     -> 'LONGLONG';
decode_type(9)                     -> 'INT24';
decode_type(10)                    -> 'DATE';
decode_type(11)                    -> 'TIME';
decode_type(12)                    -> 'DATETIME';
decode_type(13)                    -> 'YEAR';
decode_type(14)                    -> 'NEWDATE';
decode_type(246)                   -> 'NEWDECIMAL';
decode_type(247)                   -> 'ENUM';
decode_type(248)                   -> 'SET';
decode_type(249)                   -> 'TINYBLOB';
decode_type(250)                   -> 'MEDIUM_BLOG';
decode_type(251)                   -> 'LONG_BLOG';
decode_type(252)                   -> 'BLOB';
decode_type(253)                   -> 'VAR_STRING';
decode_type(254)                   -> 'STRING';
decode_type(255)                   -> 'GEOMETRY'.

%% convert type
format_type(null,              _)  -> undefined;
format_type(Column,        Field)  -> convert_type(element(4, Field), Column).
%% integer format
convert_type('TINY',       Value)  -> list_to_integer(binary_to_list(Value));
convert_type('SHORT',      Value)  -> list_to_integer(binary_to_list(Value));
convert_type('LONG',       Value)  -> list_to_integer(binary_to_list(Value));
convert_type('LONGLONG',   Value)  -> list_to_integer(binary_to_list(Value));
convert_type('INT24',      Value)  -> list_to_integer(binary_to_list(Value));
convert_type('YEAR',       Value)  -> list_to_integer(binary_to_list(Value));
%% timestamp/data_time format
convert_type('TIMESTAMP',  Value)  -> element(2, io_lib:fread("~d-~d-~d ~d:~d:~d", binary_to_list(Value)));
convert_type('DATETIME',   Value)  -> element(2, io_lib:fread("~d-~d-~d ~d:~d:~d", binary_to_list(Value)));
%% time format
convert_type('TIME',       Value)  -> element(2, io_lib:fread("~d:~d:~d", binary_to_list(Value)));
%% date format
convert_type('DATE',       Value)  -> element(2, io_lib:fread("~d-~d-~d", binary_to_list(Value)));
%% decimal float double
convert_type('DECIMAL',    Value)  -> io_lib:fread("~d", binary_to_list(Value));
convert_type('NEWDECIMAL', Value)  -> io_lib:fread("~d", binary_to_list(Value));
convert_type('FLOAT',      Value)  -> io_lib:fread("~f", binary_to_list(Value));
convert_type('DOUBLE',     Value)  -> io_lib:fread("~f", binary_to_list(Value));
%% other
convert_type(_Other,       Value)  -> Value.

%%%====================================================================
%%%  common tool part
%%%====================================================================
