-module(sign_data).
-export([get/1]).
-include("sign.hrl").

-spec get(Day :: non_neg_integer()) -> #sign_data{}.
get(1) ->
    #sign_data{award = [{1,1}]};
get(2) ->
    #sign_data{award = [{2,1}]};
get(3) ->
    #sign_data{award = [{3,1}]};
get(4) ->
    #sign_data{award = [{4,1}]};
get(5) ->
    #sign_data{award = [{5,1}]};
get(6) ->
    #sign_data{award = [{6,1}]};
get(7) ->
    #sign_data{award = [{7,1}]};
get(_) ->
    undefined.

