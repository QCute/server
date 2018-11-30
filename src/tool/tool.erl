%%%-------------------------------------------------------------------
%%% @doc
%%% module tool
%%% @end
%%%-------------------------------------------------------------------
-module(tool).
-export([ceil/1, floor/1, page/3]).
-export([diff/1, diff/2]).
%%%===================================================================
%%% API
%%%===================================================================
%% @doc 取整 大于X的最小整数
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

%% @doc ets分页
% page(Tab, Index, Per) when is_atom(Tab) andalso Index > 0 andalso Per > 0 ->
%     EtsLength = ets:info(Tab, size),
%     case Index * Per =< EtsLength of
%         true ->
%             page_ets(Tab, undefined, (Index - 1) * Per, Per, []);
%         _ when (Index - 1) * Per =< EtsLength ->
%             Length =  EtsLength - (Index - 1) * Per,
%             page_ets(Tab, undefined, (Index - 1) * Per, Length, []);
%         _ ->
%             []
%     end;
% page(_, _, _) ->
%     [].


% page_ets(_, _, 0, 0, List) ->
%     List;
% page_ets(Tab, Key, 0, Amount, List) ->
%     Next = ets:next(Tab, Key),
%     page_ets(Tab, Next, 0, Amount - 1, [hd(ets:lookup(Tab, Next)) | List]);
% page_ets(Tab, undefined, 0, Amount, List) ->
%     page_ets(Tab, ets:first(Tab), 0, Amount, List);
% page_ets(Tab, undefined, Index, Amount, List) ->
%     page_ets(Tab, ets:first(Tab), Index - 1, Amount, List);
% page_ets(Tab, Key, Index, Amount, List) ->
%     page_ets(Tab, ets:next(Tab, Key), Index - 1, Amount, List).




