%%%------------------------------------------------------------------
%%% @doc
%%% module numeric
%%% math extended library
%%% @end
%%%------------------------------------------------------------------
-module(numeric).
%% API
-export([ceil/1, floor/1]).
%%%==================================================================
%%% API functions
%%%==================================================================
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
