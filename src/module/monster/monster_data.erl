-module(monster_data).
-compile(nowarn_export_all).
-compile(export_all).
-include("monster.hrl").


get(1) ->
    #monster_data{
        monster_id = 1,
        monster_name = <<"active">>,
        type = 10,
        level = 1,
        hp = 100,
        camp = 1,
        range = 100,
        act_type = active,
        act_script = [role],
        skill = [],
        born_points = [{20,10}],
        award = [{100005,100}]
    };
get(2) ->
    #monster_data{
        monster_id = 2,
        monster_name = <<"passive">>,
        type = 20,
        level = 1,
        hp = 200,
        camp = 1,
        range = 200,
        act_type = passive,
        act_script = [enemy],
        skill = [],
        born_points = [{40,10}],
        award = [{100005,200}]
    };
get(3) ->
    #monster_data{
        monster_id = 3,
        monster_name = <<"movable">>,
        type = 30,
        level = 1,
        hp = 300,
        camp = 1,
        range = 300,
        act_type = movable,
        act_script = [],
        skill = [],
        born_points = [{60,10}],
        award = [{100005,300}]
    };
get(4) ->
    #monster_data{
        monster_id = 4,
        monster_name = <<"fix">>,
        type = 40,
        level = 1,
        hp = 400,
        camp = 1,
        range = 400,
        act_type = fix,
        act_script = [],
        skill = [],
        born_points = [{80,10}],
        award = []
    };
get(5) ->
    #monster_data{
        monster_id = 5,
        monster_name = <<"fix">>,
        type = 50,
        level = 1,
        hp = 500,
        camp = 1,
        range = 500,
        act_type = fix,
        act_script = [enemy],
        skill = [],
        born_points = [{100,10}],
        award = []
    };
get(6) ->
    #monster_data{
        monster_id = 6,
        monster_name = <<"active">>,
        type = 60,
        level = 1,
        hp = 600,
        camp = 1,
        range = 600,
        act_type = active,
        act_script = [{monster, 20}, {monster, 50}, role],
        skill = [],
        born_points = [{120,10}],
        award = [{100005,600}]
    };
get(_) -> 
    [].

