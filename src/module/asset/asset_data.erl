-module(asset_data).
-compile(nowarn_export_all).
-compile(export_all).


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
get(_) -> 
    0.

