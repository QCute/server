-module(key_data).
-export([get/1]).

-include("key.hrl").

-spec get(Key :: binary()) -> Type :: integer() | Default :: integer().
get(<<"fake"/utf8>>) ->
    2;
get(<<"test"/utf8>>) ->
    1;
get(_Key) ->
    0.


