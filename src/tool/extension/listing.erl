%%%-------------------------------------------------------------------
%%% @doc
%%% module listing
%%% list extended library
%%% @end
%%%-------------------------------------------------------------------
-module(listing).
%% API
-export([for/3, for/4]).
-export([page/3]).
-export([diff/1, diff/2]).
-export([key_find/4, key_sum/2, key_min/2, key_max/2]).
-export([index/2, replace/3, collect/2, collect/3, store/2]).
-export([shuffle/1]).
-export([random/1, random/2]).
-export([multi_random/2]).
-export([ratio/2, ratio_total/2]).
%%%===================================================================
%%% API
%%%===================================================================
%% @doc for
-spec for(Min :: integer(), Max :: integer(), F :: fun((integer(), term()) -> term())) -> term().
for(Max, Max, F) ->
    F(Max);
for(I, Max, F)   ->
    F(I),
    for(I+1, Max, F).

%% @doc for
-spec for(Min :: integer(), Max :: integer(), F :: fun((integer(), term()) -> term()), term()) -> term().
for(Min, Max, _F, State) when Min < Max ->
    State;
for(Max, Max, F, State) ->
    F(Max, State);
for(I, Max, F, State) ->
    for(I + 1, Max, F, F(I, State)).

%% @doc list page
-spec page(Data :: list(), Index :: non_neg_integer(), Per :: non_neg_integer()) -> list().
%% @doc 列表
page(_, 0, _) ->
    [];
page(_, _, 0) ->
    [];
page([], _, _) ->
    [];
page(List, Index, Per) when is_list(List) andalso Index > 0 andalso Per > 0 ->
    ListLength = length(List),
    case Index * Per =< ListLength of
        true ->
            lists:sublist(List, (Index - 1) * Per + 1, Per);
        _ when (Index - 1) * Per =< ListLength ->
            Length =  ListLength - (Index - 1) * Per,
            lists:sublist(List, (Index - 1) * Per + 1, Length);
        _ ->
            []
    end;
page(_, _, _) ->
    [].

%% @doc 去重
-spec diff(List :: list()) -> list().
diff(List) ->
    diff(List, 0).

-spec diff(List :: list(), Key :: non_neg_integer()) -> list().
diff(List, Key) ->
    diff(List, Key, []).

diff([], _Key, List) ->
    List;
diff([H | T], 0, List) ->
    case lists:member(H, List) of
        true ->
            diff(T, 0, List);
        false ->
            diff(T, 0, [H | List])
    end;
diff([H | T], Key, List) ->
    case lists:keymember(H, Key, List) of
        true ->
            diff(T, Key, List);
        false ->
            diff(T, Key, [H | List])
    end.

%% @doc key sum
-spec key_sum(N :: pos_integer(), List :: [tuple()]) -> integer().
key_sum(N, List) ->
    key_sum(List, N, 0).
key_sum([], _, Sum) -> Sum;
key_sum([H | T], N, Sum) ->
    key_sum(T, N, element(N, H) + Sum).


-spec key_find(Key :: term(), N :: pos_integer(), List :: [tuple()], Default :: term()) -> tuple() | term().
key_find(_, _, [], Default) ->
    Default;
key_find(Key, N, List, Default) ->
    case lists:keyfind(Key, N, List) of
        false ->
            Default;
        Result ->
            Result
    end.

-spec key_min(N :: pos_integer(), List :: [tuple()])        -> integer().
key_min(N, [H|T])                                           -> key_min(T, H, N).
key_min([H|T], Min, N) when element(N, H) < element(N, Min) -> key_min(T, H, N);
key_min([_|T], Min, N)                                      -> key_min(T, Min, N);
key_min([], Min, _)                                         -> Min.

-spec key_max(List :: [tuple()], N :: pos_integer())        -> integer().
key_max([H|T], N)                                           -> key_max(T, H, N).
key_max([H|T], Max, N) when element(N, H) > element(N, Max) -> key_max(T, H, N);
key_max([_|T], Max, N)                                      -> key_max(T, Max, N);
key_max([], Max, _)                                         -> Max.

-spec index(List :: list(), E :: term()) -> pos_integer().
index(L, E)        -> index(L, E, 1).
index([], _, _)    -> 0;
index([E|_], E, N) -> N;
index([_|T], E, N) -> index(T, E, N + 1).

-spec replace(List :: list(), N :: pos_integer(), E :: term()) -> list().
replace(L, N, E)           -> replace(L, [], N, E, 1).
replace([], L, _, _, _)    -> L;
replace([_|T], L, N, E, N) -> lists:reverse(L, [E | T]);
replace([H|T], L, N, E, I) -> replace(T, [H | L], N, E, I + 1).

%% @doc collect element from list
-spec collect(N :: pos_integer(), [tuple()]) -> [].
collect(N, List) ->
    [element(N, Tuple) || Tuple <- List].

%% @doc collect element from list except value
-spec collect(N :: pos_integer(), List :: [tuple()], Except :: term()) -> [].
collect(N, List, Except) ->
    [element(N, Tuple) || Tuple <- List, element(N, Tuple) =/= Except].

%% @doc 储存元素
-spec store(Element :: any(), List :: list()) -> NewList :: list().
store(Element, List) ->
    case lists:member(Element, List) of
        false ->
            [Element | List];
        true ->
            List
    end.

%% @doc 打乱列表
-spec shuffle(list()) -> list().
shuffle([])  -> [];
shuffle([I]) -> [I];
shuffle(L)   ->
    Length = length(L),
    RandList = [{randomness:rand(1, Length), X} || X <- L],
    SortList = lists:keysort(1, RandList),
    [X || {_, X} <- SortList].

%% @doc 从列表随机一个
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

%% @doc 从列表随机N个
-spec multi_random(List :: list(), N :: non_neg_integer()) -> list().
multi_random(List, N) ->
    lists:sublist(shuffle(List), N).

%% @doc rand one in fix range (10000 by default)
-spec ratio(List :: [tuple()], N :: pos_integer()) -> Element :: tuple() | [].
ratio(List, N) ->
    Rand = randomness:rand(1, 10000),
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
-spec ratio_total(List :: [tuple()], N :: pos_integer()) -> Element :: tuple() | [].
ratio_total(List, N) ->
    Total = key_sum(List, N),
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
