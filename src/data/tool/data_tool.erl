%%%-------------------------------------------------------------------
%%% @doc
%%% module data tool
%%% @end
%%%-------------------------------------------------------------------
-module(data_tool).
%% API
-export([load/2, load/3]).
-export([fill/2, fill_record/2, fill_record/4]).
-export([collect/4]).
-export([format/2]).
-export([is_term/1]).
-export([string_to_term/1, term_to_string/1, term_to_bit_string/1]).
-export([transform/2, transform/3, transform/4]).
%% includes
-include("common.hrl").
%%%===================================================================
%%% API
%%%===================================================================
%% @doc load data, convert raw list data to record
load(Data, Atom) when is_atom(Atom) ->
    [list_to_tuple([Atom | Row]) || Row <- Data];
load(Data, Handle) when is_function(Handle) ->
    [Handle(Row) || Row <- Data].
load(Data, Atom, Handle) when is_atom(Atom) andalso is_function(Handle) ->
    [Handle(list_to_tuple([Atom | Row])) || Row <- Data].

%% @doc fill tuple with list data (close range begin...end)
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
collect([], _CallBack, _SQL, _Flag) ->
    {[], []};
collect([_ | _] = Data, CallBack, SQL, Flag) ->
    collect_list(Data, CallBack, SQL, Flag);
collect(Table, CallBack, SQL, Flag) when is_atom(Table) ->
    collect_ets(Table, ets:first(Table), CallBack, SQL, Flag, []).

%% list
collect_list(Data, CallBack, SQL, Flag) ->
    collect_list(Data, CallBack, SQL, Flag, [], []).
collect_list([], _CallBack, _SQL, _Flag, [], Data) ->
    {[], Data};
collect_list([], _CallBack, {Head, _, Tail}, _Flag, String, Data) ->
    {lists:concat([Head, string:join(String, ","), Tail]), Data};
collect_list([H | T], CallBack, {_, Format, _} = SQL, Flag, String, Data) when element(Flag, H) == update orelse element(Flag, H) == insert ->
    collect_list(T, CallBack, SQL, Flag, [format(Format, CallBack(H)) | String], [setelement(Flag, H, origin) | Data]);
collect_list([H | T], CallBack, Format, Flag, String, Data) ->
    collect_list(T, CallBack, Format, Flag, String, [H | Data]).

%% ets
collect_ets(_Table, '$end_of_table', _CallBack, _SQL, _Flag, []) ->
    {[], []};
collect_ets(_Table, '$end_of_table', _CallBack, {Head, _, Tail}, _Flag, String) ->
    {lists:concat([Head, string:join(String, ","), Tail]), []};
collect_ets(Table, Key, CallBack, {_, Format, _} = SQL, Flag, String) ->
    case ets:lookup(Table, Key) of
        [H] when element(Flag, H) == update orelse element(Flag, H) == insert ->
            ets:insert(Table, setelement(Flag, H, origin)),
            collect_ets(Table, ets:next(Table, Key), CallBack, SQL, Flag, [format(Format, CallBack(H)) | String]);
        _ ->
            collect_ets(Table, ets:next(Table, Key), CallBack, SQL, Flag, String)    
    end.


%% @doc Erlang数据转字符串
term_to_string(Term) ->
    binary_to_list(list_to_binary(io_lib:format("~w", [Term]))).

%% @doc Erlang数据转字符串
term_to_bit_string(Term) ->
    erlang:list_to_bitstring(io_lib:format("~w", [Term])).

%% @doc 字符串转Erlang数据
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
%% ====================================================================
%% Internal functions
%% ====================================================================
