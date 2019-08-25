-module(monster_data).
-compile(nowarn_export_all).
-compile(export_all).
-include("monster.hrl").


get(1) ->
    #monster_data{
        monster_id = 1,
        name = <<"active">>,
        act_type = active,
        act_script = [fighter],
        born_points = [{10 * 2, 10}]
    };
get(2) ->
    #monster_data{
        monster_id = 2,
        name = <<"passive">>,
        act_type = passive,
        act_script = [enemy],
        born_points = [{20 * 2, 10}]
    };
get(3) ->
    #monster_data{
        monster_id = 3,
        name = <<"movable">>,
        act_type = movable,
        act_script = [],
        born_points = [{30 * 2, 10}]
    };
get(4) ->
    #monster_data{
        monster_id = 4,
        name = <<"fix">>,
        act_type = fix,
        act_script = [],
        born_points = [{40 * 2, 10}]
    };
get(5) ->
    #monster_data{
        monster_id = 5,
        name = <<"fix">>,
        act_type = fix,
        act_script = [enemy],
        born_points = [{50 * 2, 10}]
    };
get(6) ->
    #monster_data{
        monster_id = 6,
        name = <<"active">>,
        act_type = active,
        act_script = [{monster, 2}, {monster, 5}, fighter],
        born_points = [{60 * 2, 10}]
    };
get(_) -> 
    [].

