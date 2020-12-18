%%%-------------------------------------------------------------------
%%% @doc
%%% data parser tool
%%% @end
%%%-------------------------------------------------------------------
-module(parser).
%% API
-export([convert/2, convert/3]).
-export([fill/2, fill_record/2, fill_record/4]).
-export([join/2]).
-export([collect/2, collect/3, collect_into/4]).
-export([format/2]).
-export([is_term/1, evaluate/1, evaluate/2]).
-export([to_string/1, to_binary/1, to_term/1]).
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

%% @doc join data
-spec join(Data :: list() | ets:tab(), SQL :: binary()) -> Sql :: binary().
join(Data, Format) ->
    join_loop(Data, Format, <<>>).

join_loop([], _, Acc) ->
    Acc;
join_loop([H], Format, Acc) ->
    %% end of list
    Sql = format(Format, [H]),
    <<Acc/binary, Sql/binary>>;
join_loop([H | T], Format, Acc) ->
    Sql = format(Format, [H]),
    %% insert delimiter
    NewAcc = <<Acc/binary, Sql/binary, $,>>,
    join_loop(T, Format, NewAcc).

%% @doc collect data
-spec collect(Data :: list() | ets:tab(), SQL :: binary() | {binary(), binary()} | {binary(), binary(), binary()}) -> Sql :: binary().
collect(Data, {Head, Format, Tail}) ->
    collect_loop(Data, Head, Format, Tail, <<>>);
collect(Data, {Head, Format}) ->
    collect_loop(Data, Head, Format, <<>>, <<>>);
collect(Data, Format) ->
    collect_loop(Data, <<>>, Format, <<>>, <<>>).

collect_loop([], _, _, _, Acc) ->
    Acc;
collect_loop([H], Head, Format, Tail, Acc) ->
    %% end of list
    Sql = format(Format, H),
    <<Head/binary, Acc/binary, Sql/binary, Tail/binary>>;
collect_loop([H | T], Head, Format, Tail, Acc) ->
    Sql = format(Format, H),
    %% insert delimiter
    NewAcc = <<Acc/binary, Sql/binary, $,>>,
    collect_loop(T, Head, Format, Tail, NewAcc).

%% @doc collect transform data
-spec collect(Data :: list() | ets:tab(), F :: fun((tuple()) -> list()), SQL :: {binary(), binary(), binary()}) -> Sql :: binary().
collect(Data, F, {Head, Format}) ->
    collect_loop(Data, F, Head, Format, <<>>, <<>>);
collect(Data, F, {Head, Format, Tail}) ->
    collect_loop(Data, F, Head, Format, Tail, <<>>).

collect_loop([], _, _, _, _, Acc) ->
    Acc;
collect_loop([H], F, Head, Format, Tail, Acc) ->
    %% end of list
    Sql = format(Format, F(H)),
    <<Head/binary, Acc/binary, Sql/binary, Tail/binary>>;
collect_loop([H | T], F, Head, Format, Tail, Acc) ->
    Sql = format(Format, F(H)),
    %% insert delimiter
    NewAcc = <<Acc/binary, Sql/binary, $,>>,
    collect_loop(T, F, Head, Format, Tail, NewAcc).

%% @doc save data
-spec collect_into(Data :: list() | ets:tab(), F :: fun((tuple()) -> list()), SQL :: {binary(), binary(), binary()}, Flag :: pos_integer()) -> {Sql :: binary(), NewData :: list()}.
collect_into(Data, F, {Head, Format, Tail}, Flag) when is_list(Data) ->
    collect_list_loop(Data, F, Head, Format, Tail, Flag, <<>>, []);
collect_into(Tab, F, {Head, Format, Tail}, Flag) when is_atom(Tab) ->
    ets:safe_fixtable(Tab, true),
    Key = ets:first(Tab),
    Object = ets:lookup(Tab, Key),
    collect_ets_loop(Tab, Key, Object, F, Head, Format, Tail, Flag, <<>>).

%% list
collect_list_loop([], _, _, _, _, _, <<>>, []) ->
    {<<>>, []};
collect_list_loop([], _, _, _, _, _, <<>>, List) ->
    {<<>>, List};
collect_list_loop([], _, Head, _, Tail, _, Acc, List) ->
    {<<Head/binary, Acc/binary, Tail/binary>>, List};
collect_list_loop([H | T], F, Head, Format, Tail, Flag, <<>>, List) when element(Flag, H) =/= 0 ->
    %% format sql(convert args by the callback F)
    Sql = format(Format, F(H)),
    %% change update/save flag
    New = erlang:setelement(Flag, H, 0),
    %% insert delimiter
    NewAcc = <<Sql/binary>>,
    collect_list_loop(T, F, Head, Format, Tail, Flag, NewAcc, [New | List]);
collect_list_loop([H | T], F, Head, Format, Tail, Flag, Acc, List) when element(Flag, H) =/= 0 ->
    %% format sql(convert args by the callback F)
    Sql = format(Format, F(H)),
    %% change update/save flag
    New = erlang:setelement(Flag, H, 0),
    %% insert delimiter
    NewAcc = <<Acc/binary, ",", Sql/binary>>,
    collect_list_loop(T, F, Head, Format, Tail, Flag, NewAcc, [New | List]);
collect_list_loop([H | T], F, Head, Format, Tail, Flag, Binary, List) ->
    collect_list_loop(T, F, Head, Format, Tail, Flag, Binary, [H | List]).

%% ets
collect_ets_loop(Tab, '$end_of_table', [], _, _, _, _, _, <<>>) ->
    ets:safe_fixtable(Tab, false),
    {<<>>, []};
collect_ets_loop(Tab, '$end_of_table', [], _, Head, _, Tail, _, Acc)  ->
    ets:safe_fixtable(Tab, false),
    %% end of table
    {<<Head/binary, Acc/binary, Tail/binary>>, []};
collect_ets_loop(Tab, Key, [H], F, Head, Format, Tail, Flag, <<>>) when element(Flag, H) =/= 0 ->
    %% format sql(convert args by the callback F)
    Sql = format(Format, F(H)),
    %% change update/save flag
    New = erlang:setelement(Flag, H, 0),
    %% update new data
    ets:insert(Tab, New),
    %% next
    Next = ets:next(Tab, Key),
    Object = ets:lookup(Tab, Next),
    %% collect new sql
    NewAcc = <<Sql/binary>>,
    collect_ets_loop(Tab, Next, Object,  F, Head, Format, Tail, Flag, NewAcc);
collect_ets_loop(Tab, Key, [H], F, Head, Format, Tail, Flag, Acc) when element(Flag, H) =/= 0 ->
    %% format sql(convert args by the callback F)
    Sql = format(Format, F(H)),
    %% change update/save flag
    New = erlang:setelement(Flag, H, 0),
    %% update new data
    ets:insert(Tab, New),
    %% next
    Next = ets:next(Tab, Key),
    Object = ets:lookup(Tab, Next),
    %% collect new sql
    NewAcc = <<Acc/binary, ",", Sql/binary>>,
    collect_ets_loop(Tab, Next, Object,  F, Head, Format, Tail, Flag, NewAcc);
collect_ets_loop(Tab, Key, _, F, Head, Format, Tail, Flag, Binary) ->
    Next = ets:next(Tab, Key),
    Object = ets:lookup(Tab, Next),
    collect_ets_loop(Tab, Next, Object, F, Head, Format, Tail, Flag, Binary).

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

%% term to binary(visualization)
serialize(<<>>) ->
    <<"<<>>">>;
serialize([]) ->
    <<"[]">>;
serialize(T) when is_binary(T) ->
    <<"<<", $", T/binary, $", ">>">>;
serialize(T) when is_tuple(T) ->
    serialize_tuple_loop(T);
serialize(L) when is_list(L) ->
    serialize_list_loop(L);
serialize(O) ->
    type:to_binary(O).

%% format tuple to string
serialize_tuple_loop({}) ->
    <<"{}">>;
serialize_tuple_loop(Tuple) ->
    serialize_tuple_loop(Tuple, 1, tuple_size(Tuple), <<${>>).
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

%% @doc erlang term to string(list)
-spec to_string(Term :: term()) -> string().
to_string([]) ->
    [];
to_string(<<>>) ->
    [];
to_string(Term) ->
    binary_to_list(list_to_binary(io_lib:format("~w", [Term]))).

%% @doc erlang term to binary
-spec to_binary(Term :: term()) -> binary().
to_binary([]) ->
    <<>>;
to_binary(<<>>) ->
    <<>>;
to_binary(Term) ->
    list_to_bitstring(io_lib:format("~w", [Term])).

%% @doc binary/list to erlang term
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

%% scan term
scan(Binary) when is_binary(Binary) ->
    scan(binary_to_list(Binary));
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
revise([{'<', 1}, _, _, _, {'>', 1} | T], List) ->
    %% pid
    lists:reverse(List, [{atom, 1, undefined} | T]);
revise([{'#', 1}, {'var', _, 'Port'}, {'<', 1}, _, {'>', 1} | T], List) ->
    %% port
    lists:reverse(List, [{atom, 1, undefined} | T]);
revise([{'#', 1}, {'var', _, 'Ref'}, {'<', 1}, _, _, _, {'>', 1} | T], List) ->
    %% reference
    lists:reverse(List, [{atom, 1, undefined} | T]);
revise([H | T], List) ->
    revise(T, [H | List]).

%% @doc is erlang term
-spec is_term(String :: string()) -> boolean().
is_term(String) ->
    case erl_scan:string(String) of
        {ok, _, _} ->
            true;
        _ ->
            false
    end.

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
    lists:foreach(fun(Node) -> io:format("node:~1024p result:~1024p~n", [Node, rpc:call(Node, parser, evaluate, [String], 1000)]) end, Nodes).

%%%===================================================================
%%% Internal functions
%%%===================================================================
