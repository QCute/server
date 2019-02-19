-module(data_node).
-compile(nowarn_export_all).
-compile(export_all).


get(local, center) ->
    center;
get(main, center) ->
    center;
get(_, _) -> 
    [].

list(center) ->
    [center, center];
list(_) -> 
    [].

