-module(asset_data).
-compile(nowarn_export_all).
-compile(export_all).


get(copper) ->
    3;
get(exp) ->
    4;
get(gold) ->
    1;
get(silver) ->
    2;
get(_) -> 
    0.

