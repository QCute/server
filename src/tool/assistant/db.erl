%%%-------------------------------------------------------------------
%%% @doc
%%% database query/sql execute tool
%%% @end
%%%-------------------------------------------------------------------
-module(db).
-compile({inline, [query/1]}).
-compile({inline, [execute/1]}).
-compile({inline, [quote_string/1]}).
%% API
-export([start/0, start/2]).
-export([version/0]).
-export([select_one/1, select_row/1, select_column/1]).
-export([select_one/2, select_row/2, select_column/2]).
-export([select/1, insert/1, update/1, delete/1]).
-export([select/2, insert/2, update/2, delete/2]).
-export([save/4]).
-export([query/1, execute/1]).
-export([in/1, join/1]).
-export([collect/5]).
-export([save_into/5]).
-export([format/2, format_record/1, serialize/1]).
-export([dot/1, to_hump/1, to_lower_hump/1, to_snake/1]).
-export([quote_string/1]).
-export([id/0, limit/0, initialize/0, get_auto_increment/1, set_auto_increment/2]).
%% Includes
-include("time.hrl").
-include("journal.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc start database connector pool
-spec start() -> {ok, Pid :: pid()} | {error, Reason :: term()}.
start() ->
    %% read pool args from application config
    PoolArgs = config:mysql_connector_pool(),
    %% read connector args from application config
    ConnectorArgs = config:mysql_connector(),
    %% use volley process pool manager to start connector pool
    start(PoolArgs, ConnectorArgs).

%% @doc start database connector pool
-spec start(PoolArgs :: [volley:config()], ConnectorArgs :: [mysql_connector:config()]) -> {ok, Pid :: pid()} | {error, Reason :: term()}.
start(PoolArgs, ConnectorArgs) ->
    %% use volley process pool manager to start connector pool
    volley:start_pool(?MODULE, [{worker, {mysql_connector, start_link, [ConnectorArgs]}} | PoolArgs]).

%% @doc version
-spec version() -> binary().
version() ->
    select_one("SELECT VERSION();").

%% @doc select one
-spec select_one(Sql :: mysql_connector:sql()) -> mysql_connector:data() | [].
select_one(Sql) ->
    case select(Sql) of
        [[One | _] | _] ->
            One;
        [] ->
            []
    end.

%% @doc select one
-spec select_one(Sql :: mysql_connector:sql(), Binding :: list()) -> mysql_connector:data() | [].
select_one(Sql, Binding) ->
    case select(Sql, Binding) of
        [[One | _] | _] ->
            One;
        [] ->
            []
    end.

%% @doc select row
-spec select_row(Sql :: mysql_connector:sql()) -> [mysql_connector:data()] | [].
select_row(Sql) ->
    case select(Sql) of
        [Row | _] ->
            Row;
        [] ->
            []
    end.

%% @doc select row
-spec select_row(Sql :: mysql_connector:sql(), Binding :: list()) -> [mysql_connector:data()] | [].
select_row(Sql, Binding) ->
    case select(Sql, Binding) of
        [Row | _] ->
            Row;
        [] ->
            []
    end.

%% @doc select column
-spec select_column(Sql :: mysql_connector:sql()) -> [mysql_connector:data()] | [].
select_column(Sql) ->
    [Head || [Head | _] <- select(Sql)].

%% @doc select column
-spec select_column(Sql :: mysql_connector:sql(), Binding :: list()) -> [mysql_connector:data()] | [].
select_column(Sql, Binding) ->
    [Head || [Head | _] <- select(Sql, Binding)].

%% @doc select
-spec select(Sql :: mysql_connector:sql()) -> mysql_connector:rows_data() | [].
select(Sql) ->
    query(Sql).

%% @doc insert
-spec select(Sql :: mysql_connector:sql(), Binding :: list()) -> mysql_connector:rows_data() | [].
select(Sql, Binding) ->
    query(format(Sql, Binding)).

%% @doc insert
-spec insert(Sql :: mysql_connector:sql()) -> mysql_connector:affected_rows() | mysql_connector:insert_id() | mysql_connector:rows_data() | [].
insert(Sql) ->
    query(Sql).

%% @doc insert
-spec insert(Sql :: mysql_connector:sql(), Binding :: list()) -> mysql_connector:affected_rows() | mysql_connector:insert_id() | mysql_connector:rows_data() | [].
insert(Sql, Binding) ->
    query(format(Sql, Binding)).

%% @doc update
-spec update(Sql :: mysql_connector:sql()) -> mysql_connector:affected_rows() | [].
update(Sql) ->
    query(Sql).

%% @doc update
-spec update(Sql :: mysql_connector:sql(), Binding :: list()) -> mysql_connector:affected_rows() | [].
update(Sql, Binding) ->
    query(format(Sql, Binding)).

%% @doc delete
-spec delete(Sql :: mysql_connector:sql()) -> mysql_connector:affected_rows() | mysql_connector:rows_data() | [].
delete(Sql) ->
    query(Sql).

%% @doc delete
-spec delete(Sql :: mysql_connector:sql(), Binding :: list()) -> mysql_connector:affected_rows() | mysql_connector:rows_data() | [].
delete(Sql, Binding) ->
    query(format(Sql, Binding)).

%% @doc save
-spec save(Head :: binary(), Format :: binary(), Tail :: binary(), Data :: list() | ets:tab()) -> NewData :: list() | ok.
save(Head, Format, Tail, Data) ->
    save_into(Head, Format, Tail, Data, 0).

%% @doc query
%% @doc execute sql and fetch result
-spec query(Sql :: mysql_connector:sql()) -> mysql_connector:affected_rows() | mysql_connector:insert_id() | mysql_connector:rows_data() | [].
query("") ->
    [];
query(<<>>) ->
    [];
query(Sql) ->
    Worker = volley:get(?MODULE),
    mysql_connector:query(Sql, Worker, ?MINUTE_MILLISECONDS).

%% @doc execute
%% @doc execute sql and fetch result
-spec execute(Sql :: mysql_connector:sql()) -> mysql_connector:affected_rows() | mysql_connector:insert_id() | mysql_connector:rows_data() | [].
execute("") ->
    [];
execute(<<>>) ->
    [];
execute(Sql) ->
    Worker = volley:get(?MODULE),
    mysql_connector:execute(Sql, Worker, ?MINUTE_MILLISECONDS).

%%%===================================================================
%%% extension
%%%===================================================================
%% @doc in
-spec in(Data :: list()) -> fun(() -> binary()).
in(Data) ->
    fun() -> join(Data) end.

%% @doc join data
-spec join(Data :: list()) -> Sql :: binary().
join(Data) ->
    join_loop(Data, <<>>).

join_loop([], <<>>) ->
    <<"NULL">>;
join_loop([], Acc) ->
    Acc;
join_loop([H], Acc) ->
    %% end of list
    Sql = format(<<"?">>, [H]),
    <<Acc/binary, Sql/binary>>;
join_loop([H | T], Acc) ->
    Sql = format(<<"?">>, [H]),
    %% insert delimiter
    NewAcc = <<Acc/binary, Sql/binary, $,>>,
    join_loop(T, NewAcc).

%% @doc collect transform data
-spec collect(Sql :: mysql_connector:sql(), Value :: mysql_connector:sql(), Update :: mysql_connector:sql(), Binding :: list() | ets:tab(), Filter :: non_neg_integer()) -> binary().
%% list
collect(Head, Format, Tail, Binding, Flag) when is_list(Binding) ->
    collect_list_loop(Head, Format, Tail, Binding, Flag, <<>>);
%% ets
collect(Head, Format, Tail, Tab, Flag) when is_atom(Tab) orelse is_reference(Tab) ->
    ets:safe_fixtable(Tab, true),
    Key = ets:first(Tab),
    Object = ets:lookup(Tab, Key),
    collect_ets_loop(Head, Format, Tail, Tab, Key, Object, Flag, <<>>).

%% list
%% without value do not concat head and tail
collect_list_loop(_, _, _, [], _, <<>>) ->
    <<>>;
%% end of list
collect_list_loop(Head, _, Tail, [], _, Acc) ->
    <<Head/binary, Acc/binary, Tail/binary>>;
%% first element
collect_list_loop(Head, Format, Tail, [H | T], 0, <<>>) ->
    Data = format(Format, H),
    %% change update/save flag
    collect_list_loop(Head, Format, Tail, T, 0, Data);
%% normal element
collect_list_loop(Head, Format, Tail, [H | T], 0, Acc) ->
    Data = format(Format, H),
    %% change update/save flag
    collect_list_loop(Head, Format, Tail, T, 0, <<Acc/binary, ",", Data/binary>>);
%% first element with filter
collect_list_loop(Head, Format, Tail, [H | T], Flag, <<>>) when element(Flag, H) =/= 0 ->
    Data = format(Format, H),
    collect_list_loop(Head, Format, Tail, T, Flag, Data);
%% normal element with filter
collect_list_loop(Head, Format, Tail, [H | T], Flag, Acc) when element(Flag, H) =/= 0 ->
    Data = format(Format, H),
    %% insert delimiter
    collect_list_loop(Head, Format, Tail, T, Flag, <<Acc/binary, ",", Data/binary>>);
%% other element
collect_list_loop(Head, Format, Tail, [_ | T], Flag, Binary) ->
    collect_list_loop(Head, Format, Tail, T, Flag, Binary).

%% ets
%% without value do not concat head and tail
collect_ets_loop(_, _, _, Tab, '$end_of_table', [], _, <<>>) ->
    ets:safe_fixtable(Tab, false),
    [];
%% end of table
collect_ets_loop(Head, _, Tail, Tab, '$end_of_table', [], _, Acc) ->
    ets:safe_fixtable(Tab, false),
    %% end of table
    <<Head/binary, Acc/binary, Tail/binary>>;
%% element
collect_ets_loop(Head, Format, Tail, Tab, Key, [H], 0, <<>>) ->
    Data = format(Format, H),
    %% next
    Next = ets:next(Tab, Key),
    Object = ets:lookup(Tab, Next),
    collect_ets_loop(Head, Format, Tail, Tab, Next, Object, 0, Data);
%% normal element
collect_ets_loop(Head, Format, Tail, Tab, Key, [H], 0, Acc) ->
    Data = format(Format, H),
    %% next
    Next = ets:next(Tab, Key),
    Object = ets:lookup(Tab, Next),
    collect_ets_loop(Head, Format, Tail, Tab, Next, Object, 0, <<Acc/binary, ",", Data/binary>>);
%% first element with filter
collect_ets_loop(Head, Format, Tail, Tab, Key, [H], Flag, <<>>) when element(Flag, H) =/= 0 ->
    Data = format(Format, H),
    %% next
    Next = ets:next(Tab, Key),
    Object = ets:lookup(Tab, Next),
    collect_ets_loop(Head, Format, Tail, Tab, Next, Object, Flag, Data);
%% normal element with filter
collect_ets_loop(Head, Format, Tail, Tab, Key, [H], Flag, Acc) when element(Flag, H) =/= 0 ->
    Data = format(Format, H),
    %% next
    Next = ets:next(Tab, Key),
    Object = ets:lookup(Tab, Next),
    %% insert delimiter
    collect_ets_loop(Head, Format, Tail, Tab, Next, Object, Flag, <<Acc/binary, ",", Data/binary>>);
%% other element
collect_ets_loop(Head, Format, Tail, Tab, Key, _, Flag, Acc) ->
    Next = ets:next(Tab, Key),
    Object = ets:lookup(Tab, Next),
    collect_ets_loop(Head, Format, Tail, Tab, Next, Object, Flag, Acc).

%% @doc save into
-spec save_into(Sql :: mysql_connector:sql(), Value :: mysql_connector:sql(), Update :: mysql_connector:sql(), Binding :: list() | ets:tab(), Filter :: non_neg_integer()) -> list() | ok.
%% list
save_into(Head, Format, Tail, Binding, Flag) when is_list(Binding) ->
    save_into_list_loop(Head, Format, Tail, Binding, Flag, <<>>, []);
%% ets
save_into(Head, Format, Tail, Tab, Flag) when is_atom(Tab) orelse is_reference(Tab) ->
    ets:safe_fixtable(Tab, true),
    Key = ets:first(Tab),
    Object = ets:lookup(Tab, Key),
    save_into_ets_loop(Head, Format, Tail, Tab, Key, Object, Flag, <<>>).

%% list
%% without value do not concat head and tail
save_into_list_loop(_, _, _, [], _, <<>>, List) ->
    List;
%% end of list
save_into_list_loop(Head, _, Tail, [], _, Acc, List) ->
    query(<<Head/binary, Acc/binary, Tail/binary>>),
    List;
%% first element
save_into_list_loop(Head, Format, Tail, [H | T], 0, <<>>, List) ->
    Data = format(Format, H),
    %% change update/save flag
    save_into_list_loop(Head, Format, Tail, T, 0, Data, [H | List]);
%% normal element
save_into_list_loop(Head, Format, Tail, [H | T], 0, Acc, List) ->
    Data = format(Format, H),
    %% change update/save flag
    save_into_list_loop(Head, Format, Tail, T, 0, <<Acc/binary, ",", Data/binary>>, [H | List]);
%% first element with filter
save_into_list_loop(Head, Format, Tail, [H | T], Flag, <<>>, List) when element(Flag, H) =/= 0 ->
    Data = format(Format, H),
    %% change update/save flag
    New = erlang:setelement(Flag, H, 0),
    save_into_list_loop(Head, Format, Tail, T, Flag, Data, [New | List]);
%% normal element with filter
save_into_list_loop(Head, Format, Tail, [H | T], Flag, Acc, List) when element(Flag, H) =/= 0 ->
    Data = format(Format, H),
    %% change update/save flag
    New = erlang:setelement(Flag, H, 0),
    %% insert delimiter
    save_into_list_loop(Head, Format, Tail, T, Flag, <<Acc/binary, ",", Data/binary>>, [New | List]);
%% other element
save_into_list_loop(Head, Format, Tail, [H | T], Flag, Binary, List) ->
    save_into_list_loop(Head, Format, Tail, T, Flag, Binary, [H | List]).

%% ets
%% without value do not concat head and tail
save_into_ets_loop(_, _, _, Tab, '$end_of_table', [], _, <<>>) ->
    ets:safe_fixtable(Tab, false),
    [];
%% end of table
save_into_ets_loop(Head, _, Tail, Tab, '$end_of_table', [], _, Acc) ->
    ets:safe_fixtable(Tab, false),
    %% end of table
    query(<<Head/binary, Acc/binary, Tail/binary>>),
    [];
%% first element
save_into_ets_loop(Head, Format, Tail, Tab, Key, [H], 0, <<>>) ->
    Data = format(Format, H),
    %% next
    Next = ets:next(Tab, Key),
    Object = ets:lookup(Tab, Next),
    save_into_ets_loop(Head, Format, Tail, Tab, Next, Object, 0, Data);
%% normal element
save_into_ets_loop(Head, Format, Tail, Tab, Key, [H], 0, Acc) ->
    Data = format(Format, H),
    %% next
    Next = ets:next(Tab, Key),
    Object = ets:lookup(Tab, Next),
    save_into_ets_loop(Head, Format, Tail, Tab, Next, Object, 0, <<Acc/binary, ",", Data/binary>>);
%% first element  with filter
save_into_ets_loop(Head, Format, Tail, Tab, Key, [H], Flag, <<>>) when element(Flag, H) =/= 0 ->
    Data = format(Format, H),
    %% change update/save flag
    ets:update_element(Tab, Key, {Flag, 0}),
    %% next
    Next = ets:next(Tab, Key),
    Object = ets:lookup(Tab, Next),
    save_into_ets_loop(Head, Format, Tail, Tab, Next, Object, Flag, Data);
%% normal element  with filter
save_into_ets_loop(Head, Format, Tail, Tab, Key, [H], Flag, Acc) when element(Flag, H) =/= 0 ->
    Data = format(Format, H),
    %% change update/save flag
    ets:update_element(Tab, Key, {Flag, 0}),
    %% next
    Next = ets:next(Tab, Key),
    Object = ets:lookup(Tab, Next),
    %% insert delimiter
    save_into_ets_loop(Head, Format, Tail, Tab, Next, Object, Flag, <<Acc/binary, ",", Data/binary>>);
%% other element
save_into_ets_loop(Head, Format, Tail, Tab, Key, _, Flag, Acc) ->
    Next = ets:next(Tab, Key),
    Object = ets:lookup(Tab, Next),
    save_into_ets_loop(Head, Format, Tail, Tab, Next, Object, Flag, Acc).

%%%===================================================================
%%% tool
%%%===================================================================

%% @doc quick format
%% list type binding, using ?
%%      db:format("select * from table where id = ?", [1]).
%% tuple type binding, using index, such 1, 2, ...
%%      db:format("select * from table where id = :1: and name = :2:", {1, "me"}).
%% map type binding, using key, such id, name, ...
%%      db:format("select * from table where id = :id: and name = :name:", #{id => 1, name => "me"}).
-spec format(Format :: string() | binary(), Args :: tuple() | [term()] | term()) -> binary().
format(Sql, Binding) when is_list(Binding) ->
    format_list_binding(iolist_to_binary(Sql), Binding, <<>>);
format(Sql, Binding) when is_tuple(Binding) ->
    format_tuple_binding(iolist_to_binary(Sql), <<>>, Binding, <<>>);
format(Sql, Binding) when is_map(Binding) ->
    format_map_binding(iolist_to_binary(Sql), <<>>, Binding, <<>>).

%% list arguments
format_list_binding(<<>>, [], Acc) ->
    Acc;
format_list_binding(<<$?, Binary/binary>>, [Arg | Args], Acc) when is_integer(Arg) ->
    Data = integer_to_binary(Arg),
    format_list_binding(Binary, Args, <<Acc/binary, Data/binary>>);
format_list_binding(<<$?, Binary/binary>>, [Arg | Args], Acc) when is_float(Arg) ->
    Data = list_to_binary(io_lib_format:fwrite_g(Arg)),
    format_list_binding(Binary, Args, <<Acc/binary, Data/binary>>);
format_list_binding(<<$?, Binary/binary>>, [Arg | Args], Acc) when is_binary(Arg) ->
    format_list_binding(Binary, Args, <<Acc/binary, "\"", Arg/binary, "\"">>);
format_list_binding(<<$?, Binary/binary>>, [Arg | Args], Acc) when is_atom(Arg) ->
    format_list_binding(Binary, Args, <<Acc/binary, "'", (atom_to_binary(Arg))/binary, "'">>);
format_list_binding(<<$?, Binary/binary>>, [Arg | Args], Acc) when is_function(Arg) ->
    Data = apply(Arg, []),
    format_list_binding(Binary, Args, <<Acc/binary, Data/binary>>);
format_list_binding(<<$?, Binary/binary>>, [Arg | Args], Acc) ->
    Data = quote_string(serialize(Arg)),
    format_list_binding(Binary, Args, <<Acc/binary, "\"", Data/binary, "\"">>);
format_list_binding(<<H:8, Binary/binary>>, Args, Acc) ->
    format_list_binding(Binary, Args, <<Acc/binary, H:8>>).

%% tuple arguments
format_tuple_binding(<<>>, <<>>, _, Acc) ->
    Acc;
format_tuple_binding(<<$:, Integer:8, Binary/binary>>, <<>>, Binding, Acc) when $1 =< Integer andalso Integer =< $9 ->
    format_tuple_binding(Binary, <<Integer:8>>, Binding, Acc);
format_tuple_binding(<<$:, Binary/binary>>, Slot, Binding, Acc) when byte_size(Slot) > 0 ->
    Index = binary_to_integer(Slot),
    Arg = element(Index, Binding),
    case is_integer(Arg) of
        true ->
            Data = integer_to_binary(Arg),
            format_tuple_binding(Binary, <<>>, Binding, <<Acc/binary, Data/binary>>);
        false when is_float(Arg) ->
            Data = list_to_binary(io_lib_format:fwrite_g(Arg)),
            format_tuple_binding(Binary, <<>>, Binding, <<Acc/binary, Data/binary>>);
        false when is_binary(Arg) ->
            format_tuple_binding(Binary, <<>>, Binding, <<Acc/binary, "\"", Arg/binary, "\"">>);
        false when is_atom(Arg) ->
            format_tuple_binding(Binary, <<>>, Binding, <<Acc/binary, "'", (atom_to_binary(Arg))/binary, "'">>);
        false when is_function(Arg) ->
            Data = apply(Arg, []),
            format_tuple_binding(Binary, <<>>, Binding, <<Acc/binary, Data/binary>>);
        false ->
            Data = quote_string(serialize(Arg)),
            format_tuple_binding(Binary, <<>>, Binding, <<Acc/binary, "\"", Data/binary, "\"">>)
    end;
format_tuple_binding(<<Integer:8, Binary/binary>>, Slot, Binding, Acc) when byte_size(Slot) > 0 andalso $0 =< Integer andalso Integer =< $9 ->
    format_tuple_binding(Binary, <<Slot/binary, Integer:8>>, Binding, Acc);
format_tuple_binding(<<H:8, Binary/binary>>, Slot, Binding, Acc) ->
    format_tuple_binding(Binary, Slot, Binding, <<Acc/binary, H:8>>).

%% map arguments
format_map_binding(<<>>, <<>>, _, Acc) ->
    Acc;
format_map_binding(<<$:, Char:8, Binary/binary>>, <<>>, Binding, Acc) ->
    format_map_binding(Binary, <<Char:8>>, Binding, Acc);
format_map_binding(<<$:, Binary/binary>>, Slot, Binding, Acc) when byte_size(Slot) > 0 ->
    Arg = maps:get(binary_to_atom(Slot), Binding),
    case is_integer(Arg) of
        true ->
            Data = integer_to_binary(Arg),
            format_map_binding(Binary, <<>>, Binding, <<Acc/binary, Data/binary>>);
        false when is_float(Arg) ->
            Data = list_to_binary(io_lib_format:fwrite_g(Arg)),
            format_map_binding(Binary, <<>>, Binding, <<Acc/binary, Data/binary>>);
        false when is_binary(Arg) ->
            format_tuple_binding(Binary, <<>>, Binding, <<Acc/binary, "\"", Arg/binary, "\"">>);
        false when is_atom(Arg) ->
            format_tuple_binding(Binary, <<>>, Binding, <<Acc/binary, "'", (atom_to_binary(Arg))/binary, "'">>);
        false when is_function(Arg) ->
            Data = apply(Arg, []),
            format_map_binding(Binary, <<>>, Binding, <<Acc/binary, Data/binary>>);
        false ->
            Data = quote_string(serialize(Arg)),
            format_tuple_binding(Binary, <<>>, Binding, <<Acc/binary, "\"", Data/binary, "\"">>)
    end;
format_map_binding(<<Char:8, Binary/binary>>, Slot, Binding, Acc) when byte_size(Slot) > 0 ->
    format_map_binding(Binary, <<Slot/binary, Char:8>>, Binding, Acc);
format_map_binding(<<H:8, Binary/binary>>, Slot, Binding, Acc) ->
    format_map_binding(Binary, Slot, Binding, <<Acc/binary, H:8>>).

%% term to binary(visualization)
serialize(Value) when is_list(Value) ->
    serialize_list_loop(Value);
serialize(Value) when is_tuple(Value) ->
    serialize_tuple_loop(Value);
serialize(Value) when is_map(Value) ->
    serialize_map_loop(maps:next(maps:iterator(Value)));
serialize(Value) when is_integer(Value) ->
    integer_to_binary(Value);
serialize(Value) when is_float(Value) ->
    list_to_binary(io_lib_format:fwrite_g(Value));
serialize(Value) when is_binary(Value) ->
    <<"\"", Value/binary, "\"">>;
serialize(Value) ->
    type:to_binary(Value).

%% format tuple
serialize_tuple_loop({}) ->
    <<"{}">>;
serialize_tuple_loop(Tuple) ->
    serialize_tuple_loop(Tuple, 1, tuple_size(Tuple), <<${>>).
serialize_tuple_loop(Tuple, Size, Size, Binary) ->
    Data = serialize(element(Size, Tuple)),
    <<Binary/binary, Data/binary, $}>>;
serialize_tuple_loop(Tuple, Index, Size, Binary) ->
    Data = serialize(element(Index, Tuple)),
    serialize_tuple_loop(Tuple, Index + 1, Size, <<Binary/binary, Data/binary, $,>>).

%% format map
serialize_map_loop(none) ->
    <<"#{}">>;
serialize_map_loop(Value) ->
    serialize_map_loop(Value, <<$#, ${>>).
serialize_map_loop({Key, Value, none}, Binary) ->
    KeyData = serialize(Key),
    ValueData = serialize(Value),
    <<Binary/binary, KeyData/binary, "=>", ValueData/binary, $}>>;
serialize_map_loop({Key, Value, Iterator}, Binary) ->
    KeyData = serialize(Key),
    ValueData = serialize(Value),
    serialize_map_loop(maps:next(Iterator), <<Binary/binary, KeyData/binary, "=>", ValueData/binary, $,>>).

%% format list
serialize_list_loop([]) ->
    <<"[]">>;
serialize_list_loop(List) ->
    serialize_list_loop(List, <<$[>>).
serialize_list_loop([H], Binary) ->
    Data = serialize(H),
    <<Binary/binary, Data/binary, $]>>;
serialize_list_loop([H | T], Binary) ->
    Data = serialize(H),
    serialize_list_loop(T, <<Binary/binary, Data/binary, $,>>).

%% @doc sql quote string
-spec quote_string(Binary :: binary()) -> binary().
quote_string(Binary) ->
    quote_loop(Binary, <<>>).

quote_loop(<<>>, Acc) ->
    Acc;
quote_loop(<<00, Rest/binary>>, Acc) ->
    quote_loop(Rest, <<Acc/binary, $\\, $0>>);
quote_loop(<<10, Rest/binary>>, Acc) ->
    quote_loop(Rest, <<Acc/binary, $\\, $n>>);
quote_loop(<<13, Rest/binary>>, Acc) ->
    quote_loop(Rest, <<Acc/binary, $\\, $r>>);
quote_loop(<<26, Rest/binary>>, Acc) ->
    quote_loop(Rest, <<Acc/binary, $\\, $Z>>);
quote_loop(<<34, Rest/binary>>, Acc) ->
    quote_loop(Rest, <<Acc/binary, $\\, 34>>);
quote_loop(<<39, Rest/binary>>, Acc) ->
    quote_loop(Rest, <<Acc/binary, $\\, 39>>);
quote_loop(<<92, Rest/binary>>, Acc) ->
    quote_loop(Rest, <<Acc/binary, $\\, 92>>);
quote_loop(<<96, Rest/binary>>, Acc) ->
    quote_loop(Rest, <<Acc/binary, $\\, 96>>);
quote_loop(<<C, Rest/binary>>, Acc) ->
    quote_loop(Rest, <<Acc/binary, C>>).

%% mysql real escape charters
%% +------+------+-------+
%% |  00  |  00  | NULL  |
%% +------+------+-------+
%% |  10  |  0A  | \n    |
%% +------+------+-------+
%% |  13  |  0D  | \r    |
%% +------+------+-------+
%% |  26  |  1A  | ctl-Z |
%% +------+------+-------+
%% |  34  |  27  | "     |
%% +------+------+-------+
%% |  39  |  22  | '     |
%% +------+------+-------+
%% |  92  |  5C  | \     |
%% +------+------+-------+

%% @doc format record
-spec format_record(Record :: tuple()) -> binary().
format_record(Record) ->
    [Tag | Fields] = tuple_to_list(Record),
    [_ | Names] = record:find(Tag),
    format_record_field_loop(Names, Fields,  <<"#", (atom_to_binary(Tag))/binary, "{">>).
format_record_field_loop([], [], Acc) ->
    <<Acc/binary, "}">>;
format_record_field_loop([Name], [Field], Acc) ->
    <<Acc/binary, (atom_to_binary(Name))/binary, " = ", (serialize(Field))/binary, "}">>;
format_record_field_loop([Name | Names], [Field | Fields], Acc) ->
    format_record_field_loop(Names, Fields, <<Acc/binary, (atom_to_binary(Name))/binary, " = ", (serialize(Field))/binary, $,>>).

%%%===================================================================
%%% assistant
%%%===================================================================

%% convert schema.table.column to `schema`.`table`.`column`
-spec dot(Name :: atom() | string()) -> string().
dot(Name) ->
    Tokens = string:tokens(type:to_list(Name), "."),
    Names = [lists:concat(["`", string:trim(Token, both, [$` | unicode_util:whitespace()]), "`"]) || Token <- Tokens],
    lists:flatten(string:join(Names, ".")).

%% convert schema.table.column to SchemaTableColumn
-spec to_hump(Name :: atom() | string()) -> string().
to_hump(Name) ->
    Tokens = string:tokens(type:to_list(Name), "."),
    Names = [string:trim(Token, both, [$` | unicode_util:whitespace()]) || Token <- Tokens],
    lists:flatten(word:to_hump(string:join(Names, "_"))).

%% convert schema.table.column to schemaTableColumn
-spec to_lower_hump(Name :: atom() | string()) -> string().
to_lower_hump(Name) ->
    Tokens = string:tokens(type:to_list(Name), "."),
    Names = [string:trim(Token, both, [$` | unicode_util:whitespace()]) || Token <- Tokens],
    lists:flatten(word:to_lower_hump(string:join(Names, "_"))).

%% convert schema.table.column to SchemaTableColumn
-spec to_snake(Name :: atom() | string()) -> string().
to_snake(Name) ->
    Tokens = string:tokens(type:to_list(Name), "."),
    Names = [string:trim(Token, both, [$` | unicode_util:whitespace()]) || Token <- Tokens],
    lists:flatten(word:to_snake(string:join(Names, "_"))).

%%%===================================================================
%%% database management
%%%===================================================================
%% @doc auto increment explain
%% 1001000000001
%% ↑  ↑        ↑
%% ↑  ↑        number sequence
%% ↑  server sequence
%% group sequence
%%

%% @doc get initialization auto increment id
-spec id() -> non_neg_integer().
id() ->
    ServerId = config:server_id(),
    ServerId * 1000000000.
%% bigint 8(byte)/64(bit)
%% 31536000000 = 1000 * 86400 * 365
%% 1000000000000 / 31536000000 ~= 31.709791983764585
%% maximize option ChannelId * 1000000000000000000 + ServerId * 1000000000000000.

%% @doc auto increment limit
-spec limit() -> non_neg_integer() | infinity.
limit() ->
    infinity.

%% @doc start initialization database
-spec initialize() -> ok.
initialize() ->
    try
        AutoIncrement = id() + 1,
        %% MySQL 8.x need
        %% set information_schema_stats_expiry = 0 in mysql.ini
        catch query("SET @@SESSION.`information_schema_stats_expiry` = 0;"),
        %% the AUTO_INCREMENT field after create is null, but insert some data after truncate it is 1
        TableList = select(<<"SELECT information_schema.`TABLES`.`TABLE_NAME` FROM information_schema.`TABLES` INNER JOIN information_schema.`COLUMNS` ON information_schema.`TABLES`.`TABLE_NAME` = information_schema.`COLUMNS`.`TABLE_NAME` WHERE information_schema.`TABLES`.`AUTO_INCREMENT` IN (1, NULL) AND information_schema.`TABLES`.`TABLE_SCHEMA` = DATABASE() AND information_schema.`COLUMNS`.`TABLE_SCHEMA` = DATABASE() AND information_schema.`COLUMNS`.`COLUMN_KEY` = 'PRI' AND information_schema.`COLUMNS`.`EXTRA` = 'auto_increment'">>),
        lists:foreach(fun([Table]) -> set_auto_increment(Table, AutoIncrement) end, TableList)
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?STACKTRACE(Class, Reason, ?GET_STACKTRACE(Stacktrace))
    end.

%% @doc get auto increment
-spec get_auto_increment(Table :: atom() | string()) -> non_neg_integer().
get_auto_increment(Table) ->
    select_one(<<"SELECT AUTO_INCREMENT FROM information_schema.`TABLES` WHERE `TABLE_SCHEMA` = DATABASE() AND `TABLE_NAME` = ?">>, [Table]).

%% @doc set auto increment
-spec set_auto_increment(Table :: atom() | string(), AutoIncrement :: non_neg_integer()) -> non_neg_integer().
set_auto_increment(Table, AutoIncrement) ->
    %% maximize
    query(format(<<"ALTER TABLE `", (iolist_to_binary(Table))/binary, "` AUTO_INCREMENT = ?">>, [AutoIncrement])).

%%%===================================================================
%%% Internal functions
%%%===================================================================
