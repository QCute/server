-module(classes_data).
-export([get/1]).

-spec get(Classes :: non_neg_integer()) -> binary().
get(1) ->
    <<"七杀"/utf8>>;
get(2) ->
    <<"天师"/utf8>>;
get(3) ->
    <<"飞羽"/utf8>>;
get(4) ->
    <<"御灵"/utf8>>;
get(5) ->
    <<"妙音"/utf8>>;
get(6) ->
    <<"星术"/utf8>>;
get(_) ->
    <<>>.

