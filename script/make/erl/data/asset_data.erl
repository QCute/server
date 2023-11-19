-module(asset_data).
-export([get/1]).

-spec get(Asset :: term()) -> non_neg_integer().
get(coin) ->
    100004;
get(copper) ->
    100003;
get(exp) ->
    100005;
get(gold) ->
    100001;
get(silver) ->
    100002;
get(_) ->
    0.

