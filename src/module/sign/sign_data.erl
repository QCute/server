-module(sign_data).
-export([get/1]).

-include("sign.hrl").

-spec get(Day :: integer()) -> Award :: list() | Default :: [].
get(1) ->
    [{1,1}];
get(2) ->
    [{2,1}];
get(3) ->
    [{3,1}];
get(4) ->
    [{4,1}];
get(5) ->
    [{5,1}];
get(6) ->
    [{6,1}];
get(7) ->
    [{7,1}];
get(_) ->
    [].


