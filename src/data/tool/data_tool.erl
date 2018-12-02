%%%-------------------------------------------------------------------
%%% @doc
%%% module data tool
%%% @end
%%%-------------------------------------------------------------------
-module(data_tool).
-export([load/2, load/3, load/4]).
-export([collect/4]).
-export([format/2]).
-export([is_term/1]).
-export([string_to_term/1, term_to_string/1, term_to_bit_string/1]).
-export([transform/2, transform/3, transform/4]).
-include("common.hrl").
%%%===================================================================
%%% API
%%%===================================================================
%% @doc load data
load(DB, Record) when is_atom(Record) ->
    [list_to_tuple([Record | [string_to_term(Column) || Column <- Row]]) || Row <- DB].
load(DB, Record, CallBack) when is_atom(Record) andalso is_function(CallBack) ->
    [CallBack(list_to_tuple([Record | [string_to_term(Column) || Column <- Row]])) || Row <- DB].
load(DB, Record, N, Extra) when is_atom(Record) andalso is_integer(N) ->
    [begin T = list_to_tuple([Record | [string_to_term(Column) || Column <- Row]]), setelement(N, T, Extra) end || Row <- DB].

%% @doc save data
collect(Data, CallBack, Format, Flag) ->
    collect(Data, CallBack, Format, Flag, [], []).
collect([], _, _, _, [], Data) ->
    {[], Data};
collect([], _, {Head, _, Tail}, _, String, Data) ->
    {lists:concat([Head, string:join(String, ","), Tail]), Data};
collect([], _, _, _, [], Data) ->
    {[], Data};
collect([], _, _, _, String, Data) ->
    {String, Data};
collect([H | T], CallBack, {_, Format, _} = SQL, Flag, String, Data) when element(Flag, H) == update orelse element(Flag, H) == insert ->
    collect(T, CallBack, SQL, Flag, [format(Format, CallBack(H)) | String], [setelement(Flag, H, origin) | Data]);
collect([H | T], CallBack, Format, Flag, String, Data) when element(Flag, H) == update orelse element(Flag, H) == insert ->
    collect(T, CallBack, Format, Flag, [format(Format, CallBack(H)) | String], [setelement(Flag, H, origin) | Data]);
collect([H | T], CallBack, Format, Flag, String, Data) ->
    collect(T, CallBack, Format, Flag, String, [setelement(Flag, H, origin) | Data]).

%% @doc Erlang数据转字符串
term_to_string(Term) ->
    binary_to_list(list_to_binary(io_lib:format("~w", [Term]))).

%% @doc Erlang数据转字符串
term_to_bit_string(Term) ->
    erlang:list_to_bitstring(io_lib:format("~w", [Term])).

%% @doc 字符串转Erlang数据
string_to_term(Raw) when is_integer(Raw) ->
    Raw;
string_to_term(Raw) ->
    case scan(Raw) of
        {ok, Term} when not is_atom(Term) ->
            Term;
        _ ->
            Raw
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
    List = lists:foldl(fun(E, Acc) -> ?STACK_TRACE(catch CallBack(list_to_tuple([Record | E]), Acc)) end, [], Data),
    %% save to ets
    ets:insert(Table, List).
%% ====================================================================
%% Internal functions
%% ====================================================================
scan(Binary) when is_binary(Binary) ->
    scan(binary_to_list(Binary));
scan(String) ->
    case erl_scan:string(String ++ ".") of
        {ok, Tokens, _} ->
            erl_parse:parse_term(Tokens);
        _ ->
            undefined
    end.