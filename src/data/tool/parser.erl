%%%-------------------------------------------------------------------
%%% @doc
%%% module data tool
%%% @end
%%%-------------------------------------------------------------------
-module(parser).
%% API
-export([convert/2, convert/3]).
-export([fill/2, fill_record/2, fill_record/4]).
-export([collect/4]).
-export([format/2]).
-export([is_term/1]).
-export([string_to_term/1, term_to_string/1, term_to_bit_string/1]).
-export([transform/2, transform/3, transform/4]).
-export([map/2, map/3, find/2, find/3, for/2, for/3, first/2, first/3]).
%% Includes
-include("common.hrl").
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
-spec collect(Data :: list() | atom(), F :: fun((tuple()) -> list()), SQL :: {iolist(), iolist(), iolist()}, Flag :: pos_integer()) -> {Sql :: binary(), NewData :: list()}.
collect(Data, F, {Head, Format, Tail}, Flag) when is_list(Data) ->
    %% head as base binary(keep list element order)
    collect_list(Data, F, Head, Format, Tail, Flag, <<>>, []);
collect(T, F, {Head, Format, Tail}, Flag) when is_atom(T) ->
    %% head as base binary
    collect_ets(T, ets:first(T), F, Head, Format, Tail, Flag, <<>>).

%% list
collect_list([], _, _, _, _, _, <<>>, []) ->
    {<<>>, []};
collect_list([], _, Head, _, Tail, _, Binary, List) ->
    {<<(iolist_to_binary(Head))/binary, Binary/binary, (iolist_to_binary(Tail))/binary>>, List};
collect_list([H | T], F, Head, Format, Tail, Flag, Binary, List) when element(Flag, H) == 1 orelse element(Flag, H) == 2 orelse element(Flag, H) == update orelse element(Flag, H) == insert ->
    %% format sql(convert args by callback F)
    Sql = format(Format, F(H)),
    %% get default flag
    Status = type:default(erlang:element(Flag, H)),
    %% change update/save flag
    NewData = erlang:setelement(Flag, H, Status),
    case T of
        [] ->
            %% the last one, append head to sql string(delimiter not need)
            NewBinary = <<(iolist_to_binary(Head))/binary, Binary/binary, $,:8, Sql/binary, (iolist_to_binary(Tail))/binary>>,
            %% return sql and new data list
            {NewBinary, [NewData | List]};
        _ when Binary == <<>> ->
            %% insert delimiter
            NewBinary = <<Sql/binary>>,
            collect_list(T, F, Head, Format, Tail, Flag, NewBinary, [NewData | List]);
        _ ->
            %% insert delimiter
            NewBinary = <<Binary/binary, $,:8, Sql/binary>>,
            collect_list(T, F, Head, Format, Tail, Flag, NewBinary, [NewData | List])
    end;
collect_list([H | T], F, Head, Format, Tail, Flag, Binary, List) ->
    %% other no change data keep it origin
    collect_list(T, F, Head, Format, Tail, Flag, Binary, [H | List]).

%% ets
collect_ets(_, '$end_of_table', _, _, _, _, _, <<>>) ->
    {<<>>, []};
collect_ets(_, '$end_of_table', _, Head, _, Tail, _, Binary) ->
    {<<(iolist_to_binary(Head))/binary, Binary/binary, (iolist_to_binary(Tail))/binary>>, []};
collect_ets(T, Key, F, Head, Format, Tail, Flag, Binary) ->
    case ets:lookup(T, Key) of
        [H] when element(Flag, H) == 1 orelse element(Flag, H) == 2 orelse element(Flag, H) == update orelse element(Flag, H) == insert ->
            %% format sql(convert args by callback F)
            Sql = format(Format, F(H)),
            %% get default flag
            Status = type:default(erlang:element(Flag, H)),
            %% change update/save flag
            NewData = erlang:setelement(Flag, H, Status),
            %% update new data
            ets:insert(T, NewData),
            case ets:next(T, Key) of
                '$end_of_table' ->
                    %% the last one, append head to sql string(delimiter not need)
                    NewBinary = <<(iolist_to_binary(Head))/binary, Binary/binary, $,:8, Sql/binary, (iolist_to_binary(Tail))/binary>>,
                    %% return sql and new data updated to ets
                    {NewBinary, []};
                Next when Binary == <<>> ->
                    %% insert delimiter
                    NewBinary = <<Sql/binary>>,
                    collect_ets(T, Next, F, Head, Format, Tail, Flag, NewBinary);
                Next ->
                    %% insert delimiter
                    NewBinary = <<Binary/binary, $,:8, Sql/binary>>,
                    collect_ets(T, Next, F, Head, Format, Tail, Flag, NewBinary)
            end;
        _ ->
            collect_ets(T, ets:next(T, Key), F, Head, Format, Tail, Flag, Binary)
    end.

%% @doc quick format
-spec format(Format :: string() | binary(), Data :: [term()]) -> binary().
format(F, A) ->
    format(encoding:to_list(F), A, <<>>).
format([], [], Binary) ->
    Binary;
format([$~, $w | T], [A | Args], Binary) ->
    New = serialize(A),
    format(T, Args, <<Binary/binary, New/binary>>);
format([$~, $p | T], [A | Args], Binary) ->
    New = serialize(A),
    format(T, Args, <<Binary/binary, New/binary>>);
format([$~, $s | T], [A | Args], Binary) when is_atom(A) ->
    New = erlang:atom_to_binary(A, utf8),
    format(T, Args, <<Binary/binary, New/binary>>);
format([$~, $s | T], [A | Args], Binary) ->
    New = unicode:characters_to_binary(A),
    format(T, Args, <<Binary/binary, New/binary>>);
format([H | T], Args, Binary) ->
    format(T, Args, <<Binary/binary, H:8>>).

%% @doc term to binary(visualization)
-spec serialize(Term :: term()) -> binary().
serialize(I) when is_integer(I) ->
    integer_to_binary(I);
serialize(A) when is_atom(A) ->
    atom_to_binary(A, utf8);
serialize(T) when is_tuple(T) ->
    tuple_loop(T);
serialize(L) when is_list(L) ->
    list_loop(L);
serialize(B) when is_binary(B) ->
    B;
serialize(O) ->
    O.

%% format tuple to string
tuple_loop(Tuple) ->
    tuple_loop(Tuple, 1, size(Tuple), <<${>>).
tuple_loop(Tuple, N, N, Binary) ->
    New = serialize(element(N, Tuple)),
    <<Binary/binary, New/binary, $}>>;
tuple_loop(Tuple, N, S, Binary) ->
    New = serialize(element(N, Tuple)),
    tuple_loop(Tuple, N + 1, S, <<Binary/binary, New/binary, $,>>).

%% format list to string
list_loop(List) ->
    list_loop(List, <<$[>>).
list_loop([H], Binary) ->
    New = serialize(H),
    <<Binary/binary, New/binary, $]>>;
list_loop([H | T], Binary) ->
    New = serialize(H),
    list_loop(T, <<Binary/binary, New/binary, $,>>).

%% @doc Erlang数据转字符串
-spec term_to_string(Term :: term()) -> string().
term_to_string(Term) ->
    binary_to_list(list_to_binary(io_lib:format("~w", [Term]))).

%% @doc Erlang数据转字符串
-spec term_to_bit_string(Term :: term()) -> binary().
term_to_bit_string(Term) ->
    erlang:list_to_bitstring(io_lib:format("~w", [Term])).

%% @doc 字符串转Erlang数据
-spec string_to_term(String :: string() | binary()) -> term().
string_to_term(<<>>) ->
    [];
string_to_term([]) ->
    [];
string_to_term(Raw) when is_integer(Raw) ->
    Raw;
string_to_term(Raw) ->
    case scan(Raw) of
        {ok, Term} when not is_atom(Term) andalso not is_integer(Term) ->
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
    Sql = lists:concat(["select * from `", Table, "`"]),
    transform(Sql, Table, Table, CallBack).
transform(Sql, Table, CallBack) ->
    %% table name same as record name
    transform(Sql, Table, Table, CallBack).
transform(Sql, Table, Record, CallBack) ->
    Data = sql:select(?POOL, Table, Sql),
    %% load data delete first
    catch ets:delete_all_objects(Table),
    %% use callback transform data
    List = lists:foldl(fun(E, Acc) -> catch CallBack(list_to_tuple([Record | E]), Acc) end, [], Data),
    %% save to ets
    ets:insert(Table, List).

%% @doc ets each, update element/insert object by callback return verse
-spec map(F :: fun((Element :: term()) -> term()), Tab :: atom()) -> term().
map(F, T) ->
    map(F, T, 0).
-spec map(F :: fun((Element :: term()) -> term()), Tab :: atom(), P :: pos_integer()) -> term().
map(F, T, P) ->
    ets:safe_fixtable(T, true),
    try
        map_loop(F, T, P, ets:first(T))
    after
        ets:safe_fixtable(T, false)
    end.

map_loop(_F, _T, _P, '$end_of_table') ->
    ok;
map_loop(F, T, 0, Key) ->
    ets:insert(T, F(ets:lookup(T, Key))),
    map_loop(F, T, 0, ets:next(T, Key));
map_loop(F, T, P, Key) ->
    ets:update_element(T, Key, {P, F(ets:lookup(T, Key))}),
    map_loop(F, T, P, ets:next(T, Key)).

%% @doc ets for
-spec for(F :: fun((Element :: term()) -> term()), Tab :: atom()) -> term().
for(F, T) ->
    for(F, T, 0).
-spec for(F :: fun((Element :: term()) -> term()), Tab :: atom(), P :: pos_integer()) -> term().
for(F, T, P) ->
    ets:safe_fixtable(T, true),
    try
        for_loop(F, T, P, ets:first(T))
    after
        ets:safe_fixtable(T, false)
    end.

for_loop(_F, _T, _P, '$end_of_table') ->
    ok;
for_loop(F, T, 0, Key) ->
    F(ets:lookup(T, Key)),
    for_loop(F, T, 0, ets:next(T, Key));
for_loop(F, T, P, Key) ->
    F(ets:lookup_element(T, Key, P)),
    for_loop(F, T, P, ets:next(T, Key)).

%% @doc ets one
-spec first(F :: fun((Element :: term()) -> term()), Tab :: atom()) -> term().
first(F, T) ->
    first(F, T, []).
-spec first(F :: fun((Element :: term()) -> term()), Tab :: atom(), D :: term()) -> term().
first(F, T, D) ->
    ets:safe_fixtable(T, true),
    try
        first_loop(F, T, D, ets:first(T))
    after
        ets:safe_fixtable(T, false)
    end.

first_loop(_F, _T, D, '$end_of_table') ->
    D;
first_loop(F, T, D, Key) ->
    case F(ets:lookup(T, Key)) of
        [] ->
            first_loop(F, T, D, ets:next(T, Key));
        Result ->
            Result
    end.

%% @doc ets each, update element/insert object by callback return verse
-spec find(F :: fun((Element :: term()) -> term()), Tab :: atom()) -> term().
find(F, T) ->
    find(F, T, 0).
-spec find(F :: fun((Element :: term()) -> term()), Tab :: atom(), P :: pos_integer()) -> term().
find(F, T, P) ->
    ets:safe_fixtable(T, true),
    try
        find_loop(F, T, P, ets:first(T))
    after
        ets:safe_fixtable(T, false)
    end.

find_loop(_F, _T, _P, '$end_of_table') ->
    ok;
find_loop(F, T, 0, Key) ->
    ets:insert(T, F(ets:lookup(T, Key))),
    find_loop(F, T, 0, ets:next(T, Key));
find_loop(F, T, P, Key) ->
    ets:update_element(T, Key, {P, F(ets:lookup(T, Key))}),
    find_loop(F, T, P, ets:next(T, Key)).

%% ====================================================================
%% Internal functions
%% ====================================================================
