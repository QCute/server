%%%-------------------------------------------------------------------
%%% @doc
%%% module data parser tool
%%% @end
%%%-------------------------------------------------------------------
-module(parser).
%% API
-export([convert/2, convert/3]).
-export([fill/2, fill_record/2, fill_record/4]).
-export([collect/3, collect_into/4]).
-export([format/2]).
-export([is_term/1, eval/1, eval/2]).
-export([to_string/1, to_binary/1, to_term/1]).
-export([transform/2, transform/3, transform/4]).
%%%===================================================================
%%% API
%%%===================================================================
%% @doc load data, convert raw list data to record
-spec convert(Data :: list(), Atom :: atom() | fun((term()) -> term())) -> list().
convert(Data, Atom) when is_atom(Atom) ->
    [list_to_tuple([Atom | Row]) || Row <- Data];
convert(Data, Handle) when is_function(Handle) ->
    [Handle(Row) || Row <- Data].
-spec convert(Data :: list(), Atom :: atom(), Handle :: fun((term()) -> term())) -> list().
convert(Data, Atom, Handle) when is_atom(Atom) andalso is_function(Handle) ->
    [Handle(list_to_tuple([Atom | Row])) || Row <- Data].

%% @doc fill tuple with list data (close range begin...end)
-spec fill(list(), list()) -> list().
fill(RecordList, Data) ->
    fill(RecordList, Data, []).
fill([], Data, Result) ->
    {lists:reverse(Result), Data};
fill([H | T], Data, Result) ->
    {Tuple, Remain} = fill_record(H, Data),
    fill(T, Remain, [Tuple | Result]).
fill_record(Tuple, Data) ->
    fill_record(Tuple, Data, 2, tuple_size(Tuple)).
fill_record(Tuple, Data, Start, End) when Start > End ->
    {Tuple, Data};
fill_record(Tuple, [H | Data], Start, End) when Start =< End ->
    fill_record(setelement(Start, Tuple, H), Data, Start + 1, End).

%% @doc save data
-spec collect(Data :: list() | ets:tab(), F :: fun((tuple()) -> list()), SQL :: {iolist(), iolist(), iolist()}) -> Sql :: binary().
collect(Data, F, {Head, Format, Tail}) when is_list(Data) ->
    collect_list(Data, F, Head, Format, Tail, <<>>).

collect_list([], _, _, _, _, Acc) ->
    Acc;
collect_list([H | T], F, Head, Format, Tail, Acc) ->
    Sql = format(Format, F(H)),
    case T of
        [] ->
            %% end of list
            <<Head/binary, Acc/binary, Sql/binary, Tail/binary>>;
        _ ->
            %% insert delimiter
            NewAcc = <<Acc/binary, Sql/binary, $,>>,
            collect_list(T, F, Head, Format, Tail, NewAcc)
    end.

%% @doc save data
-spec collect_into(Data :: list() | ets:tab(), F :: fun((tuple()) -> list()), SQL :: {iolist(), iolist(), iolist()}, Flag :: pos_integer()) -> {Sql :: binary(), NewData :: list()}.
collect_into(Data, F, {Head, Format, Tail}, Flag) when is_list(Data) ->
    collect_list(Data, F, Head, Format, Tail, Flag, <<>>, []);
collect_into(Tab, F, {Head, Format, Tail}, Flag) when is_atom(Tab) ->
    Key = ets:first(Tab),
    collect_ets(Tab, Key, ets:lookup(Tab, Key), F, Head, Format, Tail, Flag, <<>>).

%% list
collect_list([], _, _, _, _, _, <<>>, []) ->
    {<<>>, []};
collect_list([], _, _, _, _, _, <<>>, List) ->
    {<<>>, List};
collect_list([H | T], F, Head, Format, Tail, Flag, Acc, List) when element(Flag, H) =/= 0 orelse element(Flag, H) =/= undefined ->
    %% change update/save flag
    New = erlang:setelement(Flag, H, 0),
    %% format sql(convert args by callback F)
    Sql = format(Format, F(New)),
    case T of
        [] ->
            %% end of list
            {<<Head/binary, Acc/binary, Sql/binary, Tail/binary>>, [New | List]};
        _ ->
            %% insert delimiter
            NewAcc = <<Acc/binary, Sql/binary, $,>>,
            collect_list(T, F, Head, Format, Tail, Flag, NewAcc, [New | List])
    end;
collect_list([H | T], F, Head, Format, Tail, Flag, Binary, List) ->
    collect_list(T, F, Head, Format, Tail, Flag, Binary, [H | List]).

%% ets
collect_ets(_, '$end_of_table', [], _, _, _, _, _, <<>>) ->
    {<<>>, []};
collect_ets(Tab, Key, [H], F, Head, Format, Tail, Flag, Acc) when element(Flag, H) =/= 0 orelse element(Flag, H) =/= undefined ->
    %% change update/save flag
    New = erlang:setelement(Flag, H, 0),
    %% format sql(convert args by callback F)
    Sql = format(Format, F(New)),
    %% update new data
    ets:insert(Tab, New),
    %% next
    case ets:next(Tab, Key) of
        '$end_of_table' ->
            %% end of table
            {<<Head/binary, Acc/binary, Sql/binary, Tail/binary>>, []};
        Next ->
            %% collect new sql
            NewAcc = <<Acc/binary, Sql/binary, $,>>,
            collect_ets(Tab, Next, ets:lookup(Tab, Next),  F, Head, Format, Tail, Flag, NewAcc)
    end;
collect_ets(Tab, Key, _, F, Head, Format, Tail, Flag, Binary) ->
    Next = ets:next(Tab, Key),
    collect_ets(Tab, Next, ets:lookup(Tab, Next), F, Head, Format, Tail, Flag, Binary).

%% @doc quick format
-spec format(Format :: string() | binary(), Data :: [term()]) -> binary().
format(F, A) ->
    format(type:to_binary(F), A, <<>>).
format(<<>>, [], Acc) ->
    Acc;
format(<<$~, $w, Binary/binary>>, [A | Args], Acc) ->
    Data = serialize(A),
    format(Binary, Args, <<Acc/binary, Data/binary>>);
format(<<$~, $p, Binary/binary>>, [A | Args], Acc) ->
    Data = serialize(A),
    format(Binary, Args, <<Acc/binary, Data/binary>>);
format(<<$~, $s, Binary/binary>>, [A | Args], Acc) when is_binary(A) ->
    format(Binary, Args, <<Acc/binary, A/binary>>);
format(<<$~, $s, Binary/binary>>, [A | Args], Acc) when is_atom(A) ->
    Data = erlang:atom_to_binary(A, utf8),
    format(Binary, Args, <<Acc/binary, Data/binary>>);
format(<<$~, $s, Binary/binary>>, [A | Args], Acc) when is_list(A) ->
    Data = unicode:characters_to_binary(A),
    format(Binary, Args, <<Acc/binary, Data/binary>>);
format(<<$~, $s, Binary/binary>>, [A | Args], Acc) ->
    Data = type:to_binary(A),
    format(Binary, Args, <<Acc/binary, Data/binary>>);
format(<<H:8, Binary/binary>>, Args, Acc) ->
    format(Binary, Args, <<Acc/binary, H:8>>).

%% @doc term to binary(visualization)
-spec serialize(Term :: term()) -> binary().
serialize(T) when is_tuple(T) ->
    serialize_tuple_loop(T);
serialize(L) when is_list(L) ->
    serialize_list_loop(L);
serialize(<<>>) ->
    <<"<<>>">>;
serialize(O) ->
    type:to_binary(O).

%% format tuple to string
serialize_tuple_loop({}) ->
    <<${, $}>>;
serialize_tuple_loop(Tuple) ->
    serialize_tuple_loop(Tuple, 1, size(Tuple), <<${>>).
serialize_tuple_loop(Tuple, N, N, Binary) ->
    Data = serialize(element(N, Tuple)),
    <<Binary/binary, Data/binary, $}>>;
serialize_tuple_loop(Tuple, N, S, Binary) ->
    New = serialize(element(N, Tuple)),
    serialize_tuple_loop(Tuple, N + 1, S, <<Binary/binary, New/binary, $,>>).

%% format list to string
serialize_list_loop([]) ->
    <<$[, $]>>;
serialize_list_loop(List) ->
    serialize_list_loop(List, <<$[>>).
serialize_list_loop([H], Binary) ->
    Data = serialize(H),
    <<Binary/binary, Data/binary, $]>>;
serialize_list_loop([H | T], Binary) ->
    Data = serialize(H),
    serialize_list_loop(T, <<Binary/binary, Data/binary, $,>>).

%% @doc Erlang数据转字符串
-spec to_string(Term :: term()) -> string().
to_string(Term) ->
    binary_to_list(list_to_binary(io_lib:format("~w", [Term]))).

%% @doc Erlang数据转字符串
-spec to_binary(Term :: term()) -> binary().
to_binary(Term) ->
    erlang:list_to_bitstring(io_lib:format("~w", [Term])).

%% @doc 字符串转Erlang数据
-spec to_term(String :: string() | binary()) -> term().
to_term(<<>>) ->
    [];
to_term([]) ->
    [];
to_term(Raw) when is_integer(Raw) ->
    Raw;
to_term(Raw) ->
    case scan(Raw) of
        {ok, Term} ->
            Term;
        _ ->
            Raw
    end.
scan(Binary) when is_binary(Binary) ->
    scan(binary_to_list(Binary));
scan(String) ->
    case erl_scan:string(String ++ ".") of
        {ok, Tokens, _} ->
            erl_parse:parse_term(Tokens);
        _ ->
            undefined
    end.

%% @doc 是否数据表达式
is_term(String) ->
    case erl_scan:string(String ++ ".") of
        {ok, _, _} ->
            true;
        _ ->
            false
    end.

%% @doc transform list data to record
transform(Table, CallBack) ->
    %% table name same as record name
    Sql = lists:concat(["SELECT * FROM `", Table, "`"]),
    transform(Sql, Table, Table, CallBack).
transform(Sql, Table, CallBack) ->
    %% table name same as record name
    transform(Sql, Table, Table, CallBack).
transform(Sql, Table, Record, CallBack) ->
    Data = sql:select(Sql),
    %% load data delete first
    catch ets:delete_all_objects(Table),
    %% use callback transform data
    List = lists:foldl(fun(E, Acc) -> catch CallBack(list_to_tuple([Record | E]), Acc) end, [], Data),
    %% save to ets
    ets:insert(Table, List).

%% @doc The Erlang meta interpreter
-spec eval(String :: string() | binary()) -> term().
eval(String) ->
    {ok, Tokens, _} = erl_scan:string(type:to_list(String)),
    case lists:reverse(Tokens) of
        [{dot, _} | _] -> 
            NewTokens = Tokens;
        ReserveTokens ->
            NewTokens = lists:reverse([{dot, 1} | ReserveTokens])
    end,
    {ok, Expression} = erl_parse:parse_exprs(NewTokens),
    {value, Value, _} = erl_eval:exprs(Expression, []),
    Value.

%% @doc execute script on nodes
-spec eval(Nodes :: [atom()], String :: string()) -> term().
eval(Nodes, String) ->
    [io:format("node:~p result:~p~n", [Node, rpc:call(Node, parser, eval, [String], 1000)]) || Node <- Nodes].

%% ====================================================================
%% Internal functions
%% ====================================================================
