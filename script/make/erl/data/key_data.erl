-module(key_data).
-export([get/1]).
-include("key.hrl").

-spec get(Key :: binary()) -> #key_data{}.
get(<<"fake"/utf8>>) ->
    #key_data{key = <<"fake"/utf8>>, type = 2};
get(<<"test"/utf8>>) ->
    #key_data{key = <<"test"/utf8>>, type = 1};
get(_) ->
    0.

