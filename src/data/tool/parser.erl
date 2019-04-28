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
-export([traverse/2, traverse/3, foreach/2, foreach/3]).
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
-spec collect(Data :: list() | atom(), CallBack :: fun((term()) -> term()), SQL :: {string(), string(), string()}, Flag :: pos_integer()) -> {Sql :: list(), NewData :: list()}.
collect([], _CallBack, _SQL, _Flag) ->
    {[], []};
collect([_ | _] = Data, CallBack, {Head, Format, Tail}, Flag) ->
    %% tail as base string(keep list element order)
    collect_list(lists:reverse(Data), CallBack, Head, Format, Flag, Tail, []);
collect(Table, CallBack, {Head, Format, Tail}, Flag) when is_atom(Table) ->
    %% tail as base string
    collect_ets(Table, ets:first(Table), CallBack, Head, Format, Flag, Tail).

%% list
collect_list([H | T], CallBack, Head, Format, Flag, String, NewDataList) when element(Flag, H) == update orelse element(Flag, H) == insert orelse element(Flag, H) == 1 orelse element(Flag, H) == 2 ->
    %% format sql
    Sql = format(Format, CallBack(H)),
    %% change save flag
    NewData = update(H, Flag),
    case T of
        [] ->
            %% the last one, append head to sql string(delimiter not need)
            NewString = Head ++ Sql ++ String,
            %% return sql and new data list
            {NewString, [NewData | NewDataList]};
        _ ->
            %% insert delimiter
            NewString = [$, | Sql ++ String],
            collect_list(T, CallBack, Head, Format, Flag, NewString, [NewData | NewDataList])
    end;
collect_list([H | T], CallBack, Head, Format, Flag, String, NewDataList) ->
    %% other no change data keep it origin
    collect_list(T, CallBack, Head, Format, Flag, String, [H | NewDataList]).

%% ets
collect_ets(_, '$end_of_table', _, _, _, _, []) ->
    {[], []};
collect_ets(Table, Key, CallBack, Head, Format, Flag, String) ->
    case ets:lookup(Table, Key) of
        [H] when element(Flag, H) == update orelse element(Flag, H) == insert orelse element(Flag, H) == 1 orelse element(Flag, H) == 2 ->
            %% change save flag
            NewData = update(H, Flag),
            %% update data
            ets:insert(Table, NewData),
            %% format sql
            Sql = format(Format, CallBack(H)),
            case ets:next(Table, Key) of
                '$end_of_table' ->
                    %% the last one, append head to sql string(delimiter not need)
                    NewString = Head ++ Sql ++ String,
                    %% return sql and new data updated to ets
                    {NewString, []};
                Next ->
                    %% insert delimiter
                    NewString = [$, | Sql ++ String],
                    collect_ets(Table, Next, CallBack, Head, Format, Flag, NewString)
            end;
        _ ->
            collect_ets(Table, ets:next(Table, Key), CallBack, Head, Format, Flag, String)
    end.

update(Data, Flag) ->
    case erlang:is_integer(erlang:element(Flag, Data)) of
        true ->
            erlang:setelement(Flag, Data, 0);
        _ ->
            erlang:setelement(Flag, Data, origin)
    end.

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

%% @doc quick format
-spec format(Format :: string() | binary(), Data :: [term()]) -> string().
format(F, A) when is_binary(F) ->
    format(binary_to_list(F), A);
format(F, A) ->
    format(lists:reverse(F), lists:reverse(A), []).
format([], [], String) ->
    String;
format([$s, $~ | T], [A | Args], String) when is_binary(A) ->
    format(T, Args, binary_to_list(A) ++ String);
format([$s, $~ | T], [A | Args], String) ->
    format(T, Args, A ++ String);
format([$w, $~ | T], [A | Args], String) ->
    format(T, Args, serialize(A) ++ String);
format([$p, $~ | T], [A | Args], String) ->
    format(T, Args, serialize(A) ++ String);
format([H | T], Args, String) ->
    format(T, Args, [H | String]).

%% @doc Erlang数据转字符串
-spec serialize(Term :: term()) -> string().
serialize(I) when is_binary(I) ->
    binary_to_list(I);
serialize(I) when is_integer(I) ->
    integer_to_list(I);
serialize(A) when is_atom(A) ->
    atom_to_list(A);
serialize(T) when is_tuple(T) ->
    L = tuple_to_list(T),
    R = [${ | string:join([serialize(X) || X <- L], ",")],
    lists:reverse([$} | lists:reverse(R)]);
serialize([_ | _] = L) ->
    R = [$[ | string:join([serialize(X) || X <- L], ",")],
    lists:reverse([$] | lists:reverse(R)]);
serialize(T) ->
    T.

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

%% @doc ets traverse, update element/insert object by callback return 
-spec traverse(F :: fun((Element :: term()) -> term()), Tab :: atom()) -> term().
traverse(F, T) ->
    traverse(F, T, 0).
-spec traverse(F :: fun((Element :: term()) -> term()), Tab :: atom(), P :: pos_integer()) -> term().
traverse(F, T, P) ->
    ets:safe_fixtable(T, true),
    try
        traverse_loop(F, T, P, ets:first(T))
    after
        ets:safe_fixtable(T, false)
    end.

traverse_loop(_F, _T, _P, '$end_of_table') ->
    ok;
traverse_loop(F, T, 0, Key) ->
    ets:insert(T, F(ets:lookup(T, Key))),
    traverse_loop(F, T, 0, ets:next(T, Key));
traverse_loop(F, T, P, Key) ->
    ets:update_element(T, Key, {P, F(ets:lookup(T, Key))}),
    traverse_loop(F, T, P, ets:next(T, Key)).

%% @doc ets foreach
-spec foreach(F :: fun((Element :: term()) -> term()), Tab :: atom()) -> term().
foreach(F, T) ->
    foreach(F, T, 0).
-spec foreach(F :: fun((Element :: term()) -> term()), Tab :: atom(), P :: pos_integer()) -> term().
foreach(F, T, P) ->
    ets:safe_fixtable(T, true),
    try
        foreach_loop(F, T, P, ets:first(T))
    after
        ets:safe_fixtable(T, false)
    end.

foreach_loop(_F, _T, _P, '$end_of_table') ->
    ok;
foreach_loop(F, T, 0, Key) ->
    F(ets:lookup(T, Key)),
    foreach_loop(F, T, 0, ets:next(T, Key));
foreach_loop(F, T, P, Key) ->
    F(ets:lookup_element(T, Key, P)),
    foreach_loop(F, T, P, ets:next(T, Key)).


%% ====================================================================
%% Internal functions
%% ====================================================================
