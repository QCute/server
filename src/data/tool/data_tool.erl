%%%-------------------------------------------------------------------
%%% @doc
%%% module data tool
%%% @end
%%%-------------------------------------------------------------------
-module(data_tool).
-export([load/2, load/3, load/4]).
-export([format/2, convert/1]).
-export([is_term/1]).
-export([string_to_term/1, term_to_string/1, term_to_bit_string/1]).
-export([transform/2, transform/3, transform/4]).
-include("common.hrl").
%%%===================================================================
%%% API
%%%===================================================================
%% @doc load user data
load(DB, Record) when is_atom(Record) ->
    [list_to_tuple([Record | [string_to_term(Column) || Column <- Row]]) || Row <- DB].
load(DB, Record, CallBack) when is_atom(Record) andalso is_function(CallBack) ->
    [CallBack(list_to_tuple([Record | [string_to_term(Column) || Column <- Row]])) || Row <- DB].
load(DB, Record, N, Extra) when is_atom(Record) andalso is_integer(N) ->
    [begin T = list_to_tuple([Record | [string_to_term(Column) || Column <- Row]]), setelement(N, T, Extra) end || Row <- DB].

%% @doc format
format(F, A) ->
    binary_to_list(list_to_binary(io_lib:format(F, A))).

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

%% @doc Erlang数据转字符串
convert(I) when is_integer(I) ->
    integer_to_list(I);
convert(A) when is_atom(A) ->
    atom_to_list(A);
convert(T) when is_tuple(T) ->
    L = tuple_to_list(T),
    R = [${ | string:join([convert(X) || X <- L], ",")],
    lists:reverse([$} | lists:reverse(R)]);
convert([_ | _] = L) ->
    R = [$[ | string:join([convert(X) || X <- L], ",")],
    lists:reverse([$] | lists:reverse(R)]);
convert(T) ->
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
    Data = sql:select(?DB_GAME, Table, Sql),
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