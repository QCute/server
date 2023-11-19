%%%-------------------------------------------------------------------
%%% @doc
%%% data parser tool
%%% @end
%%%-------------------------------------------------------------------
-module(parser).
%% API
-export([convert/2, convert/3]).
-export([fill/2, fill_record/2, fill_record/4]).
-export([to_term/1, to_term/2, is_term/1]).
-export([parse_function/1]).
-export([to_form/1, evaluate/1, evaluate/2]).
%% Includes
-include("time.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc load data, convert raw list data to record
-spec convert(Data :: list(), Atom :: atom() | fun((term()) -> term())) -> list().
convert(Data, Atom) when is_atom(Atom) ->
    [list_to_tuple([Atom | Row]) || Row <- Data].

-spec convert(Data :: list(), Atom :: atom(), Handle :: fun((term()) -> term())) -> list().
convert(Data, Atom, Handle) when is_atom(Atom) andalso is_function(Handle) ->
    [Handle(list_to_tuple([Atom | Row])) || Row <- Data].

%% @doc fill tuple with list data (close range begin...end)
-spec fill([tuple()], list()) -> {[term()], list()}.
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

%% @doc binary/list to erlang term
-spec to_term(String :: string() | binary()) -> term().
to_term(<<>>) ->
    [];
to_term([]) ->
    [];
to_term(Raw) when is_integer(Raw) ->
    Raw;
to_term(Raw) ->
    to_term(Raw, Raw).

%% @doc binary/list to erlang term
-spec to_term(String :: string() | binary(), Default :: term()) -> term().
to_term(Raw, Default) ->
    case scan(Raw) of
        {ok, Term} ->
            Term;
        _ ->
            Default
    end.

%% scan term
scan(Binary) when is_binary(Binary) ->
    scan(unicode:characters_to_list(Binary));
scan(String) ->
    case erl_scan:string(String) of
        {ok, Tokens, _} ->
            erl_parse:parse_term(revise(lists:reverse([{dot, 1} | lists:reverse(Tokens)])));
        _ ->
            undefined
    end.

%% revise pid/port/reference string
revise(List) ->
    revise(List, []).
revise([], List) ->
    lists:reverse(List);
revise([{'<<', LineStart}, {string, Line, String}, {'>>', LineEnd} | T], List) ->
    revise(T, [{'>>', LineStart}, {atom, 1, utf8}, {'/', Line}, {string, Line, String}, {'<<', LineEnd} | List]);
revise([{'<<', LineStart}, {string, Line, String}, {'/', Line}, {atom, 1, Format}, {'>>', LineEnd} | T], List) ->
    revise(T, [{'>>', LineStart}, {atom, 1, Format}, {'/', Line}, {string, Line, String}, {'<<', LineEnd} | List]);
revise([{string, Line, String} | T], List) ->
    revise(T, [{'>>', Line}, {atom, 1, utf8}, {'/', Line}, {string, Line, String}, {'<<', Line} | List]);
revise([{'<', Line}, _, _, _, {'>', Line} | T], List) ->
    %% pid
    lists:reverse(List, [{atom, Line, undefined} | T]);
revise([{'#', Line}, {'var', _, 'Port'}, {'<', Line}, _, {'>', Line} | T], List) ->
    %% port
    lists:reverse(List, [{atom, Line, undefined} | T]);
revise([{'#', Line}, {'var', _, 'Ref'}, {'<', Line}, _, _, _, {'>', Line} | T], List) ->
    %% reference
    lists:reverse(List, [{atom, Line, undefined} | T]);
revise([H | T], List) ->
    revise(T, [H | List]).

%% parse function name and arity
-spec parse_function(String :: string()) -> [{Name :: atom(), Arity :: non_neg_integer()}].
parse_function(String) ->
    {ok, Tokens, _} = erl_scan:string(String),
    parse_function(Tokens, true, [], []).

%% parse extra and export
parse_function([], _, _, List) ->
    lists:reverse(List);
%% reach -export/-include/-type/-spec ...
parse_function([{'-', _}, {atom, _, _} | T], true, Acc, List) ->
    parse_function(T, false, Acc, List);
%% reach #name.field
parse_function([{atom, _, _}, {dot, _} | T], false, Acc, List) ->
    parse_function(T, false, Acc, List);
%% reach -spec ... -> ok().
parse_function([{dot, _} | T], false, Acc, List) ->
    parse_function(T, true, Acc, List);
parse_function([_ | T], false, Acc, List) ->
    parse_function(T, false, Acc, List);
%% reach #name.field
parse_function([X = {atom, _, _}, Y = {dot, _} | T], true, Acc, List) ->
    parse_function(T, true, [Y, X | Acc], List);
%% reach fn() -> ... ok.
parse_function([H = {dot, _} | T], true, Acc, List) ->
    %% take name and args
    {ok, {function, _, Name, Arity, _}} = erl_parse:parse(lists:reverse([H | Acc])),
    parse_function(T, true, [], [{Name, Arity} | List]);
%% other case
parse_function([H | T], true, Acc, List) ->
    parse_function(T, true, [H | Acc], List).

%% @doc is erlang term
-spec is_term(String :: string()) -> boolean().
is_term(String) ->
    element(1, erl_scan:string(String)) == ok.

%% @doc to term
-spec to_form(Data :: term()) -> term().
to_form(Data) ->
    {ok, Tokens, _} = erl_scan:string(lists:flatten(io_lib:format("~tp", [Data]))),
    case lists:reverse(Tokens) of
        [{dot, _} | _] ->
            NewTokens = Tokens;
        ReserveTokens ->
            NewTokens = lists:reverse([{dot, 1} | ReserveTokens])
    end,
    {ok, Expression} = erl_parse:parse_exprs(NewTokens),
    hd(Expression).

%% @doc the erlang meta interpreter
-spec evaluate(String :: string()) -> term().
evaluate(String) ->
    {ok, Tokens, _} = erl_scan:string(String),
    case lists:reverse(Tokens) of
        [{dot, _} | _] ->
            NewTokens = Tokens;
        ReserveTokens ->
            NewTokens = lists:reverse([{dot, 1} | ReserveTokens])
    end,
    {ok, Expression} = erl_parse:parse_exprs(NewTokens),
    {value, Value, _} = erl_eval:exprs(Expression, []),
    Value.

%% @doc evaluate script on nodes
-spec evaluate(Nodes :: [atom()], String :: string()) -> ok.
evaluate(Nodes, String) ->
    lists:foreach(fun(Node) -> io:format("node:~p result:~tp~n", [Node, rpc:call(Node, ?MODULE, ?FUNCTION_NAME, [String], ?CALL_TIMEOUT)]) end, Nodes).

%%%===================================================================
%%% Internal functions
%%%===================================================================
