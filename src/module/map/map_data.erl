-module(map_data).
-compile(nowarn_export_all).
-compile(export_all).
-include("map.hrl").


get(100000) ->
    #map_data{map_id = 100000, type = slice, reconnect = false, monsters = [], rank_key = [], rank_value = hurt, rank_mode = share, enter_points = [], pk_mode = [], enter_script = [], relive_script = [], leave_script = []};
get(110001) ->
    #map_data{map_id = 110001, type = full, reconnect = false, monsters = [], rank_key = [], rank_value = hurt, rank_mode = share, enter_points = [], pk_mode = [], enter_script = [], relive_script = [], leave_script = []};
get(110002) ->
    #map_data{map_id = 110002, type = full, reconnect = false, monsters = [], rank_key = [], rank_value = hurt, rank_mode = share, enter_points = [], pk_mode = [], enter_script = [], relive_script = [], leave_script = []};
get(110003) ->
    #map_data{map_id = 110003, type = full, reconnect = false, monsters = [], rank_key = [], rank_value = hurt, rank_mode = share, enter_points = [], pk_mode = [], enter_script = [], relive_script = [], leave_script = []};
get(120001) ->
    #map_data{map_id = 120001, type = full, reconnect = false, monsters = [], rank_key = [], rank_value = hurt, rank_mode = share, enter_points = [], pk_mode = [], enter_script = [], relive_script = [], leave_script = []};
get(120002) ->
    #map_data{map_id = 120002, type = full, reconnect = false, monsters = [], rank_key = [], rank_value = hurt, rank_mode = share, enter_points = [], pk_mode = [], enter_script = [], relive_script = [], leave_script = []};
get(120003) ->
    #map_data{map_id = 120003, type = full, reconnect = false, monsters = [], rank_key = [], rank_value = hurt, rank_mode = share, enter_points = [], pk_mode = [], enter_script = [], relive_script = [], leave_script = []};
get(200001) ->
    #map_data{map_id = 200001, type = slice, reconnect = true, monsters = [], rank_key = role, rank_value = hurt, rank_mode = share, enter_points = [], pk_mode = [], enter_script = [], relive_script = [], leave_script = []};
get(200002) ->
    #map_data{map_id = 200002, type = slice, reconnect = true, monsters = [], rank_key = role, rank_value = hurt, rank_mode = share, enter_points = [], pk_mode = [], enter_script = [], relive_script = [], leave_script = []};
get(200003) ->
    #map_data{map_id = 200003, type = slice, reconnect = true, monsters = [], rank_key = role, rank_value = hurt, rank_mode = share, enter_points = [], pk_mode = [], enter_script = [], relive_script = [], leave_script = []};
get(_) ->
    [].


