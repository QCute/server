-module(monster_data).
-compile(nowarn_export_all).
-compile(export_all).
-include("monster.hrl").


get(1) ->
    #monster_data{monster_id = 1, group_id = 10, monster_name = <<"active">>, type = monster, level = 1, hp = 100, map_id = 0, camp = 1, range = 100, relive_time = 0, act_type = active, act_script = [role], skill = [], born_points = [{20,10}], award = [{100005,100}]};
get(2) ->
    #monster_data{monster_id = 2, group_id = 20, monster_name = <<"passive">>, type = guard, level = 1, hp = 200, map_id = 0, camp = 1, range = 200, relive_time = 0, act_type = passive, act_script = [enemy], skill = [], born_points = [{40,10}], award = [{100005,200}]};
get(3) ->
    #monster_data{monster_id = 3, group_id = 30, monster_name = <<"movable">>, type = boom, level = 1, hp = 300, map_id = 0, camp = 1, range = 300, relive_time = 0, act_type = movable, act_script = [], skill = [], born_points = [{60,10}], award = [{100005,300}]};
get(4) ->
    #monster_data{monster_id = 4, group_id = 40, monster_name = <<"fix">>, type = statue, level = 1, hp = 400, map_id = 0, camp = 1, range = 400, relive_time = 0, act_type = fix, act_script = [], skill = [], born_points = [{80,10}], award = []};
get(5) ->
    #monster_data{monster_id = 5, group_id = 50, monster_name = <<"act">>, type = boom, level = 1, hp = 500, map_id = 0, camp = 1, range = 500, relive_time = 0, act_type = fix, act_script = [enemy], skill = [], born_points = [{100,10}], award = []};
get(6) ->
    #monster_data{monster_id = 6, group_id = 60, monster_name = <<"boom">>, type = boss, level = 1, hp = 600, map_id = 0, camp = 1, range = 600, relive_time = 0, act_type = active, act_script = [{monster, 20}, {monster, 50}, role], skill = [], born_points = [{120,10}], award = [{100005,600}]};
get(_) ->
    [].


type(boom) ->
    [3, 5];
type(boss) ->
    [6];
type(guard) ->
    [2];
type(monster) ->
    [1];
type(statue) ->
    [4];
type(_) ->
    [].


all() ->
    [1, 2, 3, 4, 5, 6].


