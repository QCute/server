-module(monster_data).
-compile(nowarn_export_all).
-compile(export_all).
-include("monster.hrl").


get(1) ->
    #monster_data{monster_id = 1, group_id = 10, monster_name = <<"active"/utf8>>, type = monster, level = 1, hp = 100, map_id = 0, camp = 1, range = 1, distance = 300, relive_time = 0, act_type = active, act_script = [role], skills = [5], born_points = [{10,10},{20,10},{30,10},{40,10},{50,10},{60,10},{70,10},{10,10},{90,10},{100,10}], award = [{100005,100}]};
get(2) ->
    #monster_data{monster_id = 2, group_id = 20, monster_name = <<"passive"/utf8>>, type = guard, level = 1, hp = 200, map_id = 0, camp = 1, range = 2, distance = 300, relive_time = 0, act_type = passive, act_script = [enemy], skills = [], born_points = [{40,10}], award = [{100005,200}]};
get(3) ->
    #monster_data{monster_id = 3, group_id = 30, monster_name = <<"movable"/utf8>>, type = boom, level = 1, hp = 300, map_id = 0, camp = 1, range = 3, distance = 300, relive_time = 0, act_type = movable, act_script = [], skills = [], born_points = [{60,10}], award = [{100005,300}]};
get(4) ->
    #monster_data{monster_id = 4, group_id = 40, monster_name = <<"fix"/utf8>>, type = statue, level = 1, hp = 400, map_id = 0, camp = 1, range = 4, distance = 300, relive_time = 0, act_type = fix, act_script = [], skills = [], born_points = [{80,10}], award = []};
get(5) ->
    #monster_data{monster_id = 5, group_id = 50, monster_name = <<"act"/utf8>>, type = boom, level = 1, hp = 500, map_id = 0, camp = 1, range = 5, distance = 300, relive_time = 0, act_type = fix, act_script = [enemy], skills = [], born_points = [{100,10}], award = []};
get(6) ->
    #monster_data{monster_id = 6, group_id = 60, monster_name = <<"boom"/utf8>>, type = boss, level = 1, hp = 600, map_id = 0, camp = 1, range = 6, distance = 300, relive_time = 0, act_type = active, act_script = [{monster, 20}, {monster, 50}, role], skills = [], born_points = [{120,10}], award = [{100005,600}]};
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


