-module(sign_data).
-compile(nowarn_export_all).
-compile(export_all).
-include("sign.hrl").


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


