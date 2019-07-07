-module(node_data).
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

ip(local) ->
    [];
ip(main) ->
    [];
ip(test) ->
    [];
ip(_) -> 
    [].

all() ->
    [main, local, test].

