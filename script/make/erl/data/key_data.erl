-module(key_data).
-export([get/1]).
-include("key.hrl").

-spec get(Key :: binary()) -> non_neg_integer().
get(<<"fake"/utf8>>) ->
    2;
get(<<"test"/utf8>>) ->
    1;
get(_) ->
    0.

