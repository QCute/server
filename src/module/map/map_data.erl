
-module(map_data).
-compile(nowarn_export_all).
-compile(export_all).
-include("map.hrl").


get(100000) ->
    #map_data{
        map_id = 100000,
        type = full,
        monsters = [1],
        enter_points = [{30,30}]
    };
get(_) -> 
    [].

