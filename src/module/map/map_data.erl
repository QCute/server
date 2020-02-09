-module(map_data).
-compile(nowarn_export_all).
-compile(export_all).
-include("map.hrl").


get(100000) ->
    #map_data{map_id = 100000, type = full, reconnect = false, monsters = [1], rank_key = role, rank_value = hurt, rank_mode = [], enter_points = [{10,10},{20,10},{30,10},{40,10},{50,10},{60,10},{70,10},{10,10},{90,10},{100,10}], pk_mode = [], enter_script = [], relive_script = [], leave_script = []};
get(100001) ->
    #map_data{map_id = 100001, type = full, reconnect = false, monsters = [2], rank_key = role, rank_value = hurt, rank_mode = share, enter_points = [{10,10},{20,10},{30,10},{40,10},{50,10},{60,10},{70,10},{10,10},{90,10},{100,10}], pk_mode = [], enter_script = [], relive_script = [], leave_script = []};
get(_) ->
    [].


