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
-export([collect/2, collect_into/3]).
-export([format/2, format_record/1]).
-export([quote_string/1]).
-export([to_string/1, to_binary/1, to_term/1, to_term/2, is_term/1]).
-export([evaluate/1, evaluate/2]).
-compile({inline, [quote_string/1]}).
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

%% @doc collect transform data
-spec collect(Data :: list() | ets:tab(), Sql :: {binary(), binary(), binary()} | {binary(), binary()}) -> Sql :: binary().
collect(Data, Sql = {_Head, _Format, _Tail}) ->
    %% tuple sql
    collect_loop(Data, Sql);
collect(Data, {Head, Format}) ->
    %% pair sql
    collect_loop(Data, {Head, Format, <<>>});
collect(Data, Sql) ->
    %% single sql
    collect_loop(Data, Sql).

%% list
collect_loop(Data, Sql) when is_list(Data) ->
    collect_list_loop(Data, Sql, <<>>);
%% ets
collect_loop(Tab, Sql) when is_atom(Tab) ->
    ets:safe_fixtable(Tab, true),
    Key = ets:first(Tab),
    Object = ets:lookup(Tab, Key),
    collect_ets_loop(Tab, Key, Object, Sql, <<>>).

%% list
collect_list_loop([], _, Acc) ->
    Acc;
collect_list_loop([H], {Head, Format, Tail}, Acc) ->
    %% end of list
    <<Head/binary, Acc/binary, (format(Format, H))/binary, Tail/binary>>;
collect_list_loop([H | T], Sql = {_Head, Format, _Tail}, Acc) ->
    %% insert delimiter
    collect_list_loop(T, Sql, <<Acc/binary, (format(Format, H))/binary, $,>>).

%% ets
collect_ets_loop(Tab, '$end_of_table', [], _, <<>>) ->
    ets:safe_fixtable(Tab, false),
    <<>>;
collect_ets_loop(Tab, '$end_of_table', [], {Head, _, Tail}, Acc) ->
    ets:safe_fixtable(Tab, false),
    %% end of table
    <<Head/binary, Acc/binary, Tail/binary>>;
collect_ets_loop(Tab, Key, [H], Sql = {_Head, Format, _Tail}, <<>>) ->
    Data = format(Format, H),
    %% next
    Next = ets:next(Tab, Key),
    Object = ets:lookup(Tab, Next),
    %% insert delimiter
    collect_ets_loop(Tab, Next, Object, Sql, Data);
collect_ets_loop(Tab, Key, [H], Sql = {_Head, Format, _Tail}, Acc) ->
    Data = format(Format, H),
    %% next
    Next = ets:next(Tab, Key),
    Object = ets:lookup(Tab, Next),
    %% insert delimiter
    collect_ets_loop(Tab, Next, Object, Sql, <<Acc/binary, ",", Data/binary>>).

%% @doc save data
-spec collect_into(Data :: list() | ets:tab(), Sql :: {binary(), binary(), binary()}, Flag :: pos_integer()) -> {Sql :: binary(), NewData :: list()}.
collect_into(Data, Sql = {_Head, _Format, _Tail}, Flag) ->
    %% tuple sql
    collect_into_loop(Data, Sql, Flag);
collect_into(Data, {Head, Format}, Flag) ->
    %% pair sql
    collect_into_loop(Data, {Head, Format, <<>>}, Flag);
collect_into(Data, Sql, Flag) ->
    %% single sql
    collect_into_loop(Data, Sql, Flag).

%% list
collect_into_loop(Data, Sql, Flag) when is_list(Data) ->
    collect_into_list_loop(Data, Sql, Flag, <<>>, []);
%% ets
collect_into_loop(Tab, Sql, Flag) when is_atom(Tab) ->
    ets:safe_fixtable(Tab, true),
    Key = ets:first(Tab),
    Object = ets:lookup(Tab, Key),
    collect_into_ets_loop(Tab, Key, Object, Sql, Flag, <<>>).

%% list
%% without value do not concat head and tail
collect_into_list_loop([], _, _, <<>>, List) ->
    {<<>>, List};
%% end of list
collect_into_list_loop([], {Head, _, Tail}, _, Acc, List) ->
    {<<Head/binary, Acc/binary, Tail/binary>>, List};
%% first element
collect_into_list_loop([H | T], Sql = {_Head, Format, _Tail}, Flag, <<>>, List) when element(Flag, H) =/= 0 ->
    Data = format(Format, H),
    %% change update/save flag
    New = erlang:setelement(Flag, H, 0),
    collect_into_list_loop(T, Sql, Flag, Data, [New | List]);
%% normal element
collect_into_list_loop([H | T], Sql = {_Head, Format, _Tail}, Flag, Acc, List) when element(Flag, H) =/= 0 ->
    Data = format(Format, H),
    %% change update/save flag
    New = erlang:setelement(Flag, H, 0),
    %% insert delimiter
    collect_into_list_loop(T, Sql, Flag, <<Acc/binary, ",", Data/binary>>, [New | List]);
%% other element
collect_into_list_loop([H | T], Sql, Flag, Binary, List) ->
    collect_into_list_loop(T, Sql, Flag, Binary, [H | List]).

%% ets
%% without value do not concat head and tail
collect_into_ets_loop(Tab, '$end_of_table', [], _, _, <<>>) ->
    ets:safe_fixtable(Tab, false),
    {<<>>, []};
%% end of table
collect_into_ets_loop(Tab, '$end_of_table', [], {Head, _, Tail}, _, Acc) ->
    ets:safe_fixtable(Tab, false),
    %% end of table
    {<<Head/binary, Acc/binary, Tail/binary>>, []};
%% first element
collect_into_ets_loop(Tab, Key, [H], Sql = {_Head, Format, _Tail}, Flag, <<>>) when element(Flag, H) =/= 0 ->
    Data = format(Format, H),
    %% change update/save flag
    ets:update_element(Tab, Key, {Flag, 0}),
    %% next
    Next = ets:next(Tab, Key),
    Object = ets:lookup(Tab, Next),
    collect_into_ets_loop(Tab, Next, Object, Sql, Flag, Data);
%% normal element
collect_into_ets_loop(Tab, Key, [H], Sql = {_Head, Format, _Tail}, Flag, Acc) when element(Flag, H) =/= 0 ->
    Data = format(Format, H),
    %% change update/save flag
    ets:update_element(Tab, Key, {Flag, 0}),
    %% next
    Next = ets:next(Tab, Key),
    Object = ets:lookup(Tab, Next),
    %% insert delimiter
    collect_into_ets_loop(Tab, Next, Object, Sql, Flag, <<Acc/binary, ",", Data/binary>>);
%% other element
collect_into_ets_loop(Tab, Key, _, Sql, Flag, Acc) ->
    Next = ets:next(Tab, Key),
    Object = ets:lookup(Tab, Next),
    collect_into_ets_loop(Tab, Next, Object, Sql, Flag, Acc).

%% @doc quick format
-spec format(Format :: string() | binary(), Args :: tuple() | [term()] | term()) -> binary().
format(Format, Args) when is_tuple(Args) ->
    format(iolist_to_binary(Format), 1, tuple_size(Args) + 1, Args, <<>>);
format(Format, Args) when is_list(Args) ->
    format(iolist_to_binary(Format), Args, <<>>);
format(Format, Args) ->
    format(iolist_to_binary(Format), [Args], <<>>).

%% tuple arguments
format(<<>>, Size, Size, _, Acc) ->
    Acc;
format(<<$~, $~, Binary/binary>>, Index, Size, Tuple, Acc) ->
    format(Binary, Index, Size, Tuple, <<Acc/binary, $~:8>>);
format(<<$~, $p, Binary/binary>>, Index, Size, Tuple, Acc) ->
    Data = serialize(element(Index, Tuple)),
    format(Binary, Index + 1, Size, Tuple, <<Acc/binary, Data/binary>>);
format(<<$~, $w, Binary/binary>>, Index, Size, Tuple, Acc) ->
    Data = serialize(element(Index, Tuple)),
    format(Binary, Index + 1, Size, Tuple, <<Acc/binary, Data/binary>>);
format(<<$', $~, $s, $', Binary/binary>>, Index, Size, Tuple, Acc) ->
    Data = quote_string(serialize_string(element(Index, Tuple))),
    format(Binary, Index + 1, Size, Tuple, <<Acc/binary, $', Data/binary, $'>>);
format(<<$", $~, $s, $", Binary/binary>>, Index, Size, Tuple, Acc) ->
    Data = quote_string(serialize_string(element(Index, Tuple))),
    format(Binary, Index + 1, Size, Tuple, <<Acc/binary, $", Data/binary, $">>);
format(<<$`, $~, $s, $`, Binary/binary>>, Index, Size, Tuple, Acc) ->
    Data = quote_string(serialize_string(element(Index, Tuple))),
    format(Binary, Index + 1, Size, Tuple, <<Acc/binary, $`, Data/binary, $`>>);
format(<<$~, $s, Binary/binary>>, Index, Size, Tuple, Acc) ->
    Data = serialize_string(element(Index, Tuple)),
    format(Binary, Index + 1, Size, Tuple, <<Acc/binary, Data/binary>>);
format(<<$~, $i, Binary/binary>>, Index, Size, Tuple, Acc) ->
    format(Binary, Index + 1, Size, Tuple, Acc);
format(<<H:8, Binary/binary>>, Index, Size, Tuple, Acc) ->
    format(Binary, Index, Size, Tuple, <<Acc/binary, H:8>>).

%% list arguments
format(<<>>, [], Acc) ->
    Acc;
format(<<$~, $~, Binary/binary>>, Args, Acc) ->
    format(Binary, Args, <<Acc/binary, $~:8>>);
format(<<$~, $p, Binary/binary>>, Args, Acc) ->
    Data = serialize(hd(Args)),
    format(Binary, tl(Args), <<Acc/binary, Data/binary>>);
format(<<$~, $w, Binary/binary>>, Args, Acc) ->
    Data = serialize(hd(Args)),
    format(Binary, tl(Args), <<Acc/binary, Data/binary>>);
format(<<$', $~, $s, $', Binary/binary>>, Args, Acc) ->
    Data = quote_string(serialize_string(hd(Args))),
    format(Binary, tl(Args), <<Acc/binary, $', Data/binary, $'>>);
format(<<$", $~, $s, $", Binary/binary>>, Args, Acc) ->
    Data = quote_string(serialize_string(hd(Args))),
    format(Binary, tl(Args), <<Acc/binary, $", Data/binary, $">>);
format(<<$`, $~, $s, $`, Binary/binary>>, Args, Acc) ->
    Data = quote_string(serialize_string(hd(Args))),
    format(Binary, tl(Args), <<Acc/binary, $`, Data/binary, $`>>);
format(<<$~, $s, Binary/binary>>, Args, Acc) ->
    Data = serialize_string(hd(Args)),
    format(Binary, tl(Args), <<Acc/binary, Data/binary>>);
format(<<$~, $i, Binary/binary>>, Args, Acc) ->
    format(Binary, tl(Args), Acc);
format(<<H:8, Binary/binary>>, Args, Acc) ->
    format(Binary, Args, <<Acc/binary, H:8>>).

%% term to binary(visualization)
serialize(Value) when is_binary(Value) ->
    Value;
serialize(Value) when is_atom(Value) ->
    atom_to_binary(Value, utf8);
serialize(Value) when is_tuple(Value) ->
    serialize_tuple_loop(Value);
serialize(Value) when is_list(Value) ->
    serialize_list_loop(Value);
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

%% format list string
serialize_string(Value) when is_binary(Value) ->
    Value;
serialize_string(Value) when is_atom(Value) ->
    atom_to_binary(Value, utf8);
serialize_string(Value) when is_list(Value) ->
    try list_to_binary(Value) catch _:_ -> unicode:characters_to_binary(Value) end.

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
    [_ | Names] = beam:find(Tag),
    format_record_field_loop(Names, Fields,  <<"#", (atom_to_binary(Tag))/binary, "{">>).
format_record_field_loop([], [], Acc) ->
    <<Acc/binary, "}">>;
format_record_field_loop([Name], [Field], Acc) ->
    <<Acc/binary, (atom_to_binary(Name))/binary, " = ", (serialize(Field))/binary, "}">>;
format_record_field_loop([Name | Names], [Field | Fields], Acc) ->
    format_record_field_loop(Names, Fields, <<Acc/binary, (atom_to_binary(Name))/binary, " = ", (serialize(Field))/binary, $,>>).

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
    element(1, erl_scan:string(String)) == ok.

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
