-module(data_node).
-compile(nowarn_export_all).
-compile(export_all).


get(local) ->
    center;
get(main) ->
    center;
get(test) ->
    center;
get(_) -> 
    [].

all() ->
    [main, local, test].

