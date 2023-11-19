%%%-------------------------------------------------------------------
%%% @doc
%%% lists extended library
%%% @end
%%%-------------------------------------------------------------------
-module(listing).
%% API
-export([for/3, for/4]).
-export([page/3]).
-export([duplicate/1]).
-export([unique/1, key_unique/2]).
-export([key_find/4, key_find/5]).
-export([key_get/4, key_get/5]).
-export([key_keep/4, key_update/4, key_append/3, key_remove/3, key_take/3, key_take/4]).
-export([key_sum/2, key_min/2, key_max/2]).
-export([collect/2, collect/3, collect_into/3, collect_into/4]).
-export([key_index/3, index/2, is_in/2, set/3, store/2, merge/2, key_merge/3]).
-export([key_group/2, key_group/3, key_count/2, update_count/3]).
-export([split/2]).
-export([find/2, find/3]).
-export([range_top/2, range_bottom/2]).
-export([range_find/4, range_find/5]).
-export([enumerate/1]).
-export([shuffle/1]).
-export([seq/1]).
-export([for/2]).
-export([random/1, random/2]).
-export([multi_random/2]).
-export([ratio/2, ratio_total/2]).
-export([join/2]).
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc for
-spec for(Min :: integer(), Max :: integer(), F :: fun((integer()) -> term())) -> ok.
for(Max, Max, F) ->
    F(Max),
    ok;
for(I, Max, F) ->
    F(I),
    for(I + 1, Max, F).

%% @doc for
-spec for(Min :: integer(), Max :: integer(), F :: fun((integer(), term()) -> term()), State :: term()) -> NewState :: term().
for(Min, Max, _F, State) when Min < Max ->
    State;
for(Max, Max, F, State) ->
    F(Max, State);
for(I, Max, F, State) ->
    for(I + 1, Max, F, F(I, State)).

%% @doc list page
-spec page(Data :: list(), Index :: non_neg_integer(), Per :: non_neg_integer()) -> list().
page(List, Index, Per) when is_list(List) andalso Index > 0 andalso Per > 0 ->
    ListLength = length(List),
    case Index * Per =< ListLength of
        true ->
            lists:sublist(List, (Index - 1) * Per + 1, Per);
        _ when (Index - 1) * Per =< ListLength ->
            Length = ListLength - (Index - 1) * Per,
            lists:sublist(List, (Index - 1) * Per + 1, Length);
        _ ->
            []
    end;
page(_, _, _) ->
    [].

%% @doc get duplicate item
-spec duplicate(List :: list()) -> list().
duplicate(List) ->
    duplicate_loop(List, []).

duplicate_loop([], List) ->
    lists:reverse(List);
duplicate_loop([H | T], List) ->
    case lists:member(H, T) of
        true ->
            duplicate_loop(T, [H | List]);
        false ->
            duplicate_loop(T, List)
    end.

%% @doc remove duplicate item
-spec unique(List :: list()) -> list().
unique(List) ->
    unique_loop(List, []).

unique_loop([], List) ->
    lists:reverse(List);
unique_loop([H | T], List) ->
    case lists:member(H, List) of
        true ->
            unique_loop(T, List);
        false ->
            unique_loop(T, [H | List])
    end.

%% @doc remove duplicate item by key
-spec key_unique(N :: non_neg_integer(), List :: [tuple()]) -> [tuple()].
key_unique(N, List) ->
    key_unique_loop(List, N, []).

key_unique_loop([], _N, List) ->
    lists:reverse(List);
key_unique_loop([H | T], N, List) ->
    case lists:keymember(element(N, H), N, List) of
        true ->
            key_unique_loop(T, N, List);
        false ->
            key_unique_loop(T, N, [H | List])
    end.

%% @doc key find, if key exists return element, otherwise return default
-spec key_find(Key :: term(), N :: pos_integer(), List :: [tuple()], Default :: tuple()) -> tuple().
key_find(Key, N, List, Default) ->
    case lists:keyfind(Key, N, List) of
        false ->
            Default;
        Result ->
            Result
    end.

%% @doc key find, if key exists return result, otherwise, return default
-spec key_find(Key :: term(), N :: pos_integer(), List :: [tuple()], Result :: term(), Default :: term()) -> term().
key_find(Key, N, List, Result, Default) ->
    case lists:keymember(Key, N, List) of
        true ->
            Result;
        false ->
            Default
    end.

%% @doc key get, if key exists return index 2 of the element, otherwise, return default
-spec key_get(Key :: term(), N :: pos_integer(), List :: [tuple()], Default :: term()) -> term().
key_get(Key, N, List, Default) ->
    case lists:keyfind(Key, N, List) of
        false ->
            Default;
        Result ->
            element(2, Result)
    end.

%% @doc key get, if key exists return index I of the element, otherwise, return default
-spec key_get(Key :: term(), N :: pos_integer(), List :: [tuple()], I :: pos_integer(), Default :: term()) -> term().
key_get(Key, N, List, I, Default) ->
    case lists:keyfind(Key, N, List) of
        false ->
            Default;
        Result ->
            element(I, Result)
    end.

%% @doc key keep
-spec key_keep(Key :: term(), N :: pos_integer(), List :: [tuple()], E :: term()) -> list().
key_keep(Key, N, List, E) ->
    case lists:keyfind(Key, N, List) of
        false ->
            %% not contain, store it
            [E | List];
        _ ->
            %% contain this, ignore
            List
    end.

%% @doc key update
-spec key_update(Key :: term(), N :: pos_integer(), List :: [tuple()], E :: term()) -> list().
key_update(Key, N, List, E) ->
    case lists:keyfind(Key, N, List) of
        false ->
            %% not contain, store it
            List;
        Value ->
            %% contain this, ignore
            lists:keyreplace(Key, N, List, setelement(N, Value, E))
    end.

%% @doc key append
-spec key_append(Key :: term(), List :: [tuple()], E :: term()) -> list().
key_append(Key, List, E) ->
    case lists:keyfind(Key, 1, List) of
        false ->
            %% not contain, store it
            [{Key, [E]} | List];
        {_, T} ->
            %% contain this, ignore
            lists:keyreplace(Key, 1, List, {Key, [E | T]})
    end.

%% @doc key remove
-spec key_remove(Key :: term(), List :: [tuple()], E :: term()) -> list().
key_remove(Key, List, E) ->
    case lists:keyfind(Key, 1, List) of
        false ->
            %% not contain, ignore it
            List;
        {_, T} ->
            case lists:delete(E, T) of
                [] ->
                    %% delete element
                    lists:keydelete(Key, 1, List);
                Remain ->
                    %% contain this, remove element
                    lists:keyreplace(Key, 1, List, {Key, Remain})
            end
    end.

%% @doc key take
-spec key_take(Key :: term(), List :: [tuple()], Default :: term()) -> {term(), list()}.
key_take(Key, List, Default) ->
    case lists:keytake(Key, 1, List) of
        false ->
            %% not contain, ignore it
            {Default, List};
        {value, Data, T} ->
            {Data, T}
    end.

%% @doc key take
-spec key_take(Key :: term(), N :: non_neg_integer(), List :: [tuple()], Default :: term()) -> {term(), list()}.
key_take(Key, N, List, Default) ->
    case lists:keytake(Key, N, List) of
        false ->
            %% not contain, ignore it
            {Default, List};
        {value, Data, T} ->
            {Data, T}
    end.

%% @doc key sum
-spec key_sum(N :: pos_integer(), List :: [tuple()]) -> integer().
key_sum(N, List) -> key_sum(List, N, 0).
key_sum([], _, Sum) -> Sum;
key_sum([H | T], N, Sum) -> key_sum(T, N, element(N, H) + Sum).

%% @doc key min
-spec key_min(N :: pos_integer(), List :: [tuple()]) -> tuple().
key_min(N, [H | T]) -> key_min(T, H, N).
key_min([H | T], Min, N) when element(N, H) < element(N, Min) -> key_min(T, H, N);
key_min([_ | T], Min, N) -> key_min(T, Min, N);
key_min([], Min, _) -> Min.

%% @doc key max
-spec key_max(N :: pos_integer(), List :: [tuple()]) -> tuple().
key_max(N, [H | T]) -> key_max(T, H, N).
key_max([H | T], Max, N) when element(N, H) > element(N, Max) -> key_max(T, H, N);
key_max([_ | T], Max, N) -> key_max(T, Max, N);
key_max([], Max, _) -> Max.

%% @doc key index
-spec key_index(N :: non_neg_integer(), X :: term(), List :: list()) -> pos_integer().
key_index(N, X, L) -> key_index(L, N, X, 1).
key_index([], _, _, _) -> 0;
key_index([H | _], N, X, P) when element(N, H) == X -> P;
key_index([_ | T], N, X, P) -> key_index(T, N, X, P + 1).

%% @doc find member index
-spec index(E :: term(), List :: list()) -> pos_integer().
index(E, L) -> index(E, L, 1).
index(_, [], _) -> 0;
index(E, [E | _], P) -> P;
index(E, [_ | T], P) -> index(E, T, P + 1).

%% @doc element is in the list
-spec is_in(E :: term(), List :: list()) -> boolean().
is_in(_, []) -> false;
is_in(E, [E | _]) -> true;
is_in(E, [_ | T]) -> is_in(E, T).

%% @doc replace member
-spec set(N :: pos_integer(), List :: list(), E :: term()) -> list().
set(N, L, E) -> set(L, [], N, E, 1).
set([], L, _, _, _) -> L;
set([_ | T], L, N, E, N) -> lists:reverse(L, [E | T]);
set([H | T], L, N, E, I) -> set(T, [H | L], N, E, I + 1).

%% @doc collect element from list
-spec collect(N :: pos_integer(), [tuple()]) -> [term()].
collect(N, List) ->
    [element(N, Tuple) || Tuple <- List].

%% @doc collect element from list except value
-spec collect(N :: pos_integer(), List :: [tuple()], Except :: term()) -> [term()].
collect(N, List, Except) when is_function(Except, 1) ->
    [element(N, Tuple) || Tuple <- List, Except(element(N, Tuple))];
collect(N, List, Except) ->
    [element(N, Tuple) || Tuple <- List, element(N, Tuple) =/= Except].

%% @doc collect element from list
-spec collect_into(N :: pos_integer(), [tuple()], F :: fun((term()) -> term())) -> [term()].
collect_into(N, List, F) ->
    [F(element(N, Tuple)) || Tuple <- List].

%% @doc collect element from list except value
-spec collect_into(N :: pos_integer(), List :: [tuple()], Except :: term(), F :: fun((term()) -> term())) -> [term()].
collect_into(N, List, Except, F) when is_function(Except, 1) ->
    [F(element(N, Tuple)) || Tuple <- List, Except(element(N, Tuple))];
collect_into(N, List, Except, F) ->
    [F(element(N, Tuple)) || Tuple <- List, element(N, Tuple) =/= Except].

%% @doc group by key
-spec key_group(N :: pos_integer(), List :: [tuple()]) -> [{Key :: term(), list()}].
key_group(N, List) ->
    key_group(N, List, fun(E, L) -> [E | L] end).

%% @doc group by key
-spec key_group(N :: pos_integer(), List :: [tuple()], F :: fun((term(), list()) -> term())) -> [{Key :: term(), list()}].
key_group(N, List, F) ->
    key_group(List, N, F, []).

key_group([], _, _, List) ->
    List;
key_group([H | T], N, F, List) ->
    Key = element(N, H),
    case lists:keyfind(Key, 1, List) of
        false ->
            key_group(T, N, F, [{Key, F(H, [])} | List]);
        {_, Group} ->
            key_group(T, N, F, lists:keystore(Key, 1, List, {Key, F(H, Group)}))
    end.

%% @doc count by key
-spec key_count(N :: pos_integer(), List :: [tuple()]) -> [{Key :: term(), Count :: non_neg_integer()}].
key_count(N, List) ->
    key_count(List, N, []).

key_count([], _, List) ->
    List;
key_count([H | T], N, List) ->
    Key = element(N, H),
    case lists:keyfind(Key, N, List) of
        false ->
            key_count(T, N, [{Key, 1} | List]);
        {_, Count} ->
            key_count(T, N, lists:keystore(Key, N, List, {Key, Count + 1}))
    end.

%% @doc update list count, remove it when counter less then 0
-spec update_count(Key :: term(), List :: list(), Value :: integer()) -> NewList :: list().
update_count(Key, List, Value) ->
    case lists:keyfind(Key, 1, List) of
        {_, Number} when Number + Value =< 0 ->
            lists:keydelete(Key, 1, List);
        {_, Number} ->
            lists:keystore(Key, 1, List, {Key, Number + Value});
        false ->
            [{Key, Value} | List]
    end.

%% @doc store
-spec store(Element :: any(), List :: list()) -> NewList :: list().
store(Element, List) ->
    case lists:member(Element, List) of
        false ->
            [Element | List];
        true ->
            List
    end.

%% @doc merge tow list(short list front)
-spec merge(Front :: list(), List :: list()) -> NewList :: list().
merge([], Back) ->
    Back;
merge([Front | T], Back) ->
    merge(T, [Front | Back]).

%% @doc merge tow list(new list back)
-spec key_merge(Key :: non_neg_integer(), Old :: list(), New :: list()) -> NewList :: list().
key_merge(_, New, []) ->
    New;
key_merge(Key, Old, [Front | T]) ->
    key_merge(Key, lists:keystore(element(Key, Front), Key, Old, Front), T).

%% @doc split
-spec split(N :: non_neg_integer(), List :: list()) -> {list(), list()}.
split(N, List) ->
    split(N, List, []).

split(_, [], Front) ->
    {lists:reverse(Front), []};
split(0, List, Front) ->
    {Front, List};
split(N, [Front | T], FrontList) ->
    split(N - 1, T, [Front | FrontList]).

%% @doc find
-spec find(F :: fun((term()) -> boolean()), List :: list()) -> term() | false.
find(F, List) ->
    find(F, List, false).

%% @doc find
-spec find(F :: fun((term()) -> boolean()), List :: list(), Default :: term()) -> term().
find(F, List, Default) ->
    find_loop(List, F, Default).

find_loop([], _, Default) ->
    Default;
find_loop([H | T], F, Default) ->
    case F(H) of
        true ->
            H;
        false ->
            find_loop(T, F, Default)
    end.

%% @doc range find top
-spec range_top(List :: list(), Range :: non_neg_integer()) -> term() | undefined.
range_top(List, Range) ->
    range_top_loop(List, Range).

range_top_loop([], _) ->
    undefined;
range_top_loop([Min, Max | _], Range) when Min =< Range andalso Range < Max ->
    Max;
range_top_loop([Max | _], Range) when Range < Max ->
    Max;
range_top_loop([_ | T], Range) ->
    range_top_loop(T, Range).

%% @doc range find bottom
-spec range_bottom(List :: list(), Range :: non_neg_integer()) -> term() | undefined.
range_bottom(List, Range) ->
    range_bottom_loop(List, Range).

range_bottom_loop([], _) ->
    undefined;
range_bottom_loop([Min, Max | _], Range) when Min =< Range andalso Range < Max ->
    Min;
range_bottom_loop([Min | _], Range) when Min =< Range ->
    Min;
range_bottom_loop([_ | T], Range) ->
    range_bottom_loop(T, Range).

%% @doc range find(close set)
-spec range_find(Value :: non_neg_integer(), Min :: pos_integer(), Max :: pos_integer(), list()) -> tuple() | [].
range_find(Value, MinPosition, MaxPosition, List) ->
    range_find_loop(List, MinPosition, MaxPosition, Value, []).

%% @doc range find(close set)
-spec range_find(Value :: non_neg_integer(), Min :: pos_integer(), Max :: pos_integer(), list(), Default :: term()) -> tuple() | term().
range_find(Value, MinPosition, MaxPosition, List, Default) ->
    range_find_loop(List, MinPosition, MaxPosition, Value, Default).

range_find_loop([], _, _, _, Default) ->
    Default;
range_find_loop([H | _], Min, Max, Value, _) when element(Min, H) =< Value andalso Value =< element(Max, H) ->
    H;
range_find_loop([_ | T], Min, Max, Value, Default) ->
    range_find_loop(T, Min, Max, Value, Default).

%% @doc enumerate
-spec enumerate(List :: list()) -> [{Index :: non_neg_integer(), term()}].
enumerate(List) ->
    enumerate_loop(List, 1).

enumerate_loop([], _) ->
    [];
enumerate_loop([H | T], Index) ->
    [{Index, H} | enumerate_loop(T, Index + 1)].

%% @doc shuffle list order
-spec shuffle(list()) -> list().
shuffle([]) -> [];
shuffle([I]) -> [I];
shuffle(L) ->
    Length = length(L),
    RandList = [{randomness:rand(1, Length), X} || X <- L],
    SortList = lists:keysort(1, RandList),
    [X || {_, X} <- SortList].

%% @doc sequence list
-spec seq(N :: non_neg_integer()) -> list().
seq(N) ->
    seq(N, []).

seq(N, List) when N =< 0 ->
    List;
seq(N, List) ->
    seq(N - 1, [N | List]).

%% @doc sequence list
-spec for(F :: fun((I :: non_neg_integer()) -> ok), N :: non_neg_integer()) -> ok.
for(_, N) when N =< 0 ->
    ok;
for(F, N) ->
    F(N),
    for(F, N - 1).

%% @doc random a item from list
-spec random(List :: list()) -> term().
random(List) ->
    random(List, []).

-spec random(List :: list(), Default :: term()) -> term().
random([], Default) ->
    Default;
random([I], _) ->
    I;
random(List, _) ->
    lists:nth(randomness:rand(1, length(List)), List).

%% @doc random n item from list
-spec multi_random(List :: list(), N :: non_neg_integer()) -> list().
multi_random(List, N) ->
    lists:sublist(shuffle(List), N).

%% @doc rand one in fix range (10000 by default)
-spec ratio(N :: pos_integer(), List :: [tuple()]) -> Element :: tuple() | [].
ratio(N, List) ->
    Rand = randomness:rand(),
    find_ratio(List, N, Rand).

%% it will find if given argument valid, let it crash when data error
find_ratio([], _N, _Rand) ->
    [];
find_ratio([H | T], N, Rand) ->
    case Rand =< element(N, H) of
        true ->
            H;
        false when T == [] ->
            H;
        false ->
            find_ratio(T, N, Rand)
    end.

%% @doc rand one in total range
-spec ratio_total(N :: pos_integer(), List :: [tuple()]) -> Element :: tuple() | [].
ratio_total(N, List) ->
    Total = key_sum(N, List),
    Rand = randomness:rand(1, Total),
    find_ratio_total(List, N, Rand, 0).

%% it will find if given argument valid, let it crash when data error
find_ratio_total([], _N, _Rand, _StartRatio) ->
    [];
find_ratio_total([H | T], N, Rand, StartRatio) ->
    EndRatio = StartRatio + element(N, H),
    case StartRatio < Rand andalso Rand =< EndRatio of
        true ->
            H;
        false ->
            find_ratio_total(T, N, Rand, EndRatio)
    end.

%% join list with separator
-spec join(List :: list(), Sep :: list()) -> list().
join([], Sep) when is_list(Sep) ->
    [];
join([H|T], Sep) ->
    lists:flatten([H | [[Sep, X] || X <- T]]).
