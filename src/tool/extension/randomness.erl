%%%------------------------------------------------------------------
%%% @doc
%%% module randomness
%%% random extended library
%%% OTP_20 or later, random module will deprecated, rand module replace it
%%% this module can change name to random/randomness
%%% @end
%%%------------------------------------------------------------------
-module(randomness).
%% API
-export([hit/1, hit/3, hit_ge/1, hit_ge/3, hit_le/1, hit_le/3]).
-export([rand/0, rand/2]).
%%%==================================================================
%%% API functions
%%%==================================================================
%% @doc 命中判断 (大于等于)
-spec hit(Rate :: non_neg_integer()) -> boolean().
hit(0) ->
    false;
hit(10000) ->
    true;
hit(Rate) ->
    hit(1, 10000, Rate).
-spec hit(Min :: non_neg_integer(), Max :: non_neg_integer(), Rate :: non_neg_integer()) -> boolean().
hit(Min, Max, Rate) ->
    rand(Min, Max) =< Rate.

%% @doc 命中判断大(大于等于)
-spec hit_ge(Rate :: non_neg_integer()) -> boolean().
hit_ge(Rate) ->
    hit_ge(1, 10000, Rate).
-spec hit_ge(Min :: non_neg_integer(), Max :: non_neg_integer(), Rate :: non_neg_integer()) -> boolean().
hit_ge(Min, Max, Rate) ->
    rand(Min, Max) =< Rate.

%% @doc 命中判断(小于等于)
-spec hit_le(Rate :: non_neg_integer()) -> boolean().
hit_le(Rate) ->
    hit_le(1, 10000, Rate).
-spec hit_le(Min :: non_neg_integer(), Max :: non_neg_integer(), Rate :: non_neg_integer()) -> boolean().
hit_le(Min, Max, Rate) ->
    Rate =< rand(Min, Max).

%% @doc 产生一个介于Min到Max之间的随机整数
-spec rand() -> pos_integer().
rand() ->
    rand(1, 10000).
-spec rand(Min :: pos_integer(), Max :: pos_integer()) -> pos_integer().
rand(Same, Same) ->
    Same;
rand(Min, Max) ->
    M = Min - 1,
    rand:uniform(Max - M) + M.

%%%==================================================================
%%% Internal functions
%%%==================================================================
