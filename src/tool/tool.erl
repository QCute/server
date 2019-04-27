%%%-------------------------------------------------------------------
%%% @doc
%%% module tool
%%% @end
%%%-------------------------------------------------------------------
-module(tool).
-export([ceil/1, floor/1, page/3]).
-export([diff/1, diff/2]).
-export([key_find/4, key_sum/2, key_min/2, key_max/2]). 
-export([index/2, replace/3, store/2]).
-export([shuffle/1]). 
-export([sub_random/2]).
%%%===================================================================
%%% API
%%%===================================================================
%% @doc 取整 大于X的最小整数
-spec ceil(number()) -> integer().
ceil(X) ->
    case trunc(X) of
        X ->
            X;
        T when X > 0 ->
          T + 1;
        T ->
            T
    end.

%% @doc 取整 小于X的最大整数
-spec floor(number()) -> integer().
floor(X) ->
    case trunc(X) of
        X ->
            X;
        T when X > 0 ->
            T;
        T ->
            T - 1
    end.

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

%% @doc list/ets page
-spec page(Data :: atom() | list(), Index :: non_neg_integer(), Per :: non_neg_integer()) -> list().
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
%% @doc ETS
page(Tab, Index, Per) when is_atom(Tab) andalso Index > 0 andalso Per > 0 ->
    EtsLength = ets:info(Tab, size),
    case Index * Per =< EtsLength of
        true ->
            Start = (Index - 1) * Per,
            End = Start + Per - 1,
            [hd(ets:slot(Tab, S)) || S <- lists:seq(Start, End)];
        _ when (Index - 1) * Per =< EtsLength ->
            Length =  EtsLength - (Index - 1) * Per,
            Start = (Index - 1) * Per,
            End = Start + Length - 1,
            [hd(ets:slot(Tab, S)) || S <- lists:seq(Start, End)];
        _ ->
            []
    end;
page(_, _, _) ->
    [].

%% @doc key sum
-spec key_sum(List :: [tuple()], N :: pos_integer()) -> integer().
key_sum(List, N) ->
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

-spec key_min(List :: [tuple()], N :: pos_integer())        -> integer().
key_min([H|T], N)                                           -> key_min(T, H, N).
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

-spec store(Element :: any(), List :: list()) -> NewList :: list().
store(Element, List) ->
    case lists:member(Element, List) of
        false ->
            [Element | List];
        true ->
            List
    end.

-spec shuffle(list()) -> list().
shuffle([])  -> [];
shuffle([I]) -> [I];
shuffle(L)   ->
    Length = length(L),
    RandList = [{rand:rand(1, Length), X} || X <- L],
    SortList = lists:keysort(1, RandList),
    [X || {_, X} <- SortList].

-spec sub_random(List :: list(), N :: non_neg_integer()) -> list().
sub_random(List, N) ->
    lists:sublist(shuffle(List), N).
