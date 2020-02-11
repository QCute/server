-module(map_data).
-compile(nowarn_export_all).
-compile(export_all).
-include("map.hrl").


get(100000) ->
    #map_data{map_id = 100000, type = slice, reconnect = false, monsters = [], rank_key = [], rank_value = [], rank_mode = [], enter_points = [{10,10},{20,10},{30,10},{40,10},{50,10},{60,10},{70,10},{10,10},{90,10},{100,10}], pk_mode = [], enter_script = [], relive_script = [], leave_script = []};
get(100001) ->
    #map_data{map_id = 100001, type = full, reconnect = false, monsters = [], rank_key = [], rank_value = [], rank_mode = [], enter_points = [{10,10},{20,10},{30,10},{40,10},{50,10},{60,10},{70,10},{10,10},{90,10},{100,10}], pk_mode = [], enter_script = [], relive_script = [], leave_script = []};
get(100002) ->
    #map_data{map_id = 100002, type = full, reconnect = false, monsters = [], rank_key = [], rank_value = [], rank_mode = [], enter_points = [{10,10},{20,10},{30,10},{40,10},{50,10},{60,10},{70,10},{10,10},{90,10},{100,10}], pk_mode = [], enter_script = [], relive_script = [], leave_script = []};
get(100003) ->
    #map_data{map_id = 100003, type = full, reconnect = false, monsters = [], rank_key = [], rank_value = [], rank_mode = [], enter_points = [{10,10},{20,10},{30,10},{40,10},{50,10},{60,10},{70,10},{10,10},{90,10},{100,10}], pk_mode = [], enter_script = [], relive_script = [], leave_script = []};
get(200001) ->
    #map_data{map_id = 200001, type = full, reconnect = false, monsters = [], rank_key = [], rank_value = [], rank_mode = [], enter_points = [{10,10},{20,10},{30,10},{40,10},{50,10},{60,10},{70,10},{10,10},{90,10},{100,10}], pk_mode = [], enter_script = [], relive_script = [], leave_script = []};
get(200002) ->
    #map_data{map_id = 200002, type = full, reconnect = false, monsters = [], rank_key = [], rank_value = [], rank_mode = [], enter_points = [{10,10},{20,10},{30,10},{40,10},{50,10},{60,10},{70,10},{10,10},{90,10},{100,10}], pk_mode = [], enter_script = [], relive_script = [], leave_script = []};
get(200003) ->
    #map_data{map_id = 200003, type = full, reconnect = false, monsters = [], rank_key = [], rank_value = [], rank_mode = [], enter_points = [{10,10},{20,10},{30,10},{40,10},{50,10},{60,10},{70,10},{10,10},{90,10},{100,10}], pk_mode = [], enter_script = [], relive_script = [], leave_script = []};
get(_) ->
    [].


