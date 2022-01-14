-module(asset_data).
-export([get/1]).


-spec get(Asset :: atom()) -> ItemId :: integer() | Default :: integer().
get(gold) ->
    100001;
get(silver) ->
    100002;
get(copper) ->
    100003;
get(coin) ->
    100004;
get(exp) ->
    100005;
get(_Asset) ->
    0.


