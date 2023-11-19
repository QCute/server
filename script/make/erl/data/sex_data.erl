-module(sex_data).
-export([get/1]).

-spec get(Sex :: non_neg_integer()) -> binary().
get(1) ->
    <<"男"/utf8>>;
get(2) ->
    <<"女"/utf8>>;
get(_) ->
    <<>>.

