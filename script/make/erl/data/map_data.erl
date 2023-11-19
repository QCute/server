-module(map_data).
-export([get/1]).
-export([city/0]).
-include("map.hrl").

-spec get(MapId :: non_neg_integer()) -> #map_data{}.
get(200100100) ->
    #map_data{map_id = 200100100, type = full, reconnect = false, monsters = [], rank_key = none, rank_value = hurt, rank_mode = share, enter_points = [], pk_mode = [], enter_script = [], relive_script = [], leave_script = []};
get(200200100) ->
    #map_data{map_id = 200200100, type = full, reconnect = false, monsters = [], rank_key = camp, rank_value = hurt, rank_mode = share, enter_points = [], pk_mode = [], enter_script = [], relive_script = [], leave_script = []};
get(200200200) ->
    #map_data{map_id = 200200200, type = full, reconnect = false, monsters = [], rank_key = camp, rank_value = hurt, rank_mode = share, enter_points = [], pk_mode = [], enter_script = [], relive_script = [], leave_script = []};
get(200200300) ->
    #map_data{map_id = 200200300, type = full, reconnect = false, monsters = [], rank_key = camp, rank_value = hurt, rank_mode = share, enter_points = [], pk_mode = [], enter_script = [], relive_script = [], leave_script = []};
get(200300100) ->
    #map_data{map_id = 200300100, type = full, reconnect = false, monsters = [], rank_key = team, rank_value = hurt, rank_mode = share, enter_points = [], pk_mode = [], enter_script = [], relive_script = [], leave_script = []};
get(200300200) ->
    #map_data{map_id = 200300200, type = full, reconnect = false, monsters = [], rank_key = team, rank_value = hurt, rank_mode = share, enter_points = [], pk_mode = [], enter_script = [], relive_script = [], leave_script = []};
get(200300300) ->
    #map_data{map_id = 200300300, type = full, reconnect = false, monsters = [], rank_key = team, rank_value = hurt, rank_mode = share, enter_points = [], pk_mode = [], enter_script = [], relive_script = [], leave_script = []};
get(200400100) ->
    #map_data{map_id = 200400100, type = slice, reconnect = true, monsters = [], rank_key = guild, rank_value = hurt, rank_mode = share, enter_points = [], pk_mode = [], enter_script = [], relive_script = [], leave_script = []};
get(200400200) ->
    #map_data{map_id = 200400200, type = slice, reconnect = true, monsters = [], rank_key = guild, rank_value = hurt, rank_mode = share, enter_points = [], pk_mode = [], enter_script = [], relive_script = [], leave_script = []};
get(200400300) ->
    #map_data{map_id = 200400300, type = slice, reconnect = true, monsters = [], rank_key = guild, rank_value = hurt, rank_mode = share, enter_points = [], pk_mode = [], enter_script = [], relive_script = [], leave_script = []};
get(200500100) ->
    #map_data{map_id = 200500100, type = slice, reconnect = true, monsters = [], rank_key = role, rank_value = hurt, rank_mode = share, enter_points = [], pk_mode = [], enter_script = [], relive_script = [], leave_script = []};
get(200500200) ->
    #map_data{map_id = 200500200, type = slice, reconnect = true, monsters = [], rank_key = role, rank_value = hurt, rank_mode = share, enter_points = [], pk_mode = [], enter_script = [], relive_script = [], leave_script = []};
get(200500300) ->
    #map_data{map_id = 200500300, type = slice, reconnect = true, monsters = [], rank_key = role, rank_value = hurt, rank_mode = share, enter_points = [], pk_mode = [], enter_script = [], relive_script = [], leave_script = []};
get(_) ->
    undefined.

-spec city() -> non_neg_integer().
city() ->
    200100100.

