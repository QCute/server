-module(monster_data).
-export([get/1]).
-export([type/1]).
-export([all/0]).
-include("monster.hrl").

-spec get(MonsterId :: non_neg_integer()) -> #monster_data{}.
get(1) ->
    #monster_data{monster_id = 1, type = 1, name = <<"active"/utf8>>, description = <<"active"/utf8>>, level = 1, hp = 100, map_id = 200001, camp = 1, range = 1, distance = 300, relive_time = 0, act_type = active, act_script = [role], skills = [1], born_points = [{10,10},{20,10},{30,10},{40,10},{50,10},{60,10},{70,10},{10,10},{90,10},{100,10}], award = [{100005,100}]};
get(2) ->
    #monster_data{monster_id = 2, type = 2, name = <<"passive"/utf8>>, description = <<"passive"/utf8>>, level = 1, hp = 200, map_id = 200002, camp = 1, range = 2, distance = 300, relive_time = 0, act_type = passive, act_script = [enemy], skills = [1], born_points = [{40,10}], award = [{100005,200}]};
get(3) ->
    #monster_data{monster_id = 3, type = 3, name = <<"movable"/utf8>>, description = <<"movable"/utf8>>, level = 1, hp = 300, map_id = 200003, camp = 1, range = 3, distance = 300, relive_time = 0, act_type = movable, act_script = [], skills = [], born_points = [{60,10}], award = [{100005,300}]};
get(4) ->
    #monster_data{monster_id = 4, type = 4, name = <<"fix"/utf8>>, description = <<"fix"/utf8>>, level = 1, hp = 400, map_id = 0, camp = 1, range = 4, distance = 300, relive_time = 0, act_type = fix, act_script = [], skills = [], born_points = [{80,10}], award = []};
get(5) ->
    #monster_data{monster_id = 5, type = 5, name = <<"act"/utf8>>, description = <<"act"/utf8>>, level = 1, hp = 500, map_id = 0, camp = 1, range = 5, distance = 300, relive_time = 0, act_type = fix, act_script = [enemy], skills = [], born_points = [{100,10}], award = []};
get(6) ->
    #monster_data{monster_id = 6, type = 6, name = <<"boom"/utf8>>, description = <<"boom"/utf8>>, level = 1, hp = 600, map_id = 0, camp = 1, range = 6, distance = 300, relive_time = 0, act_type = active, act_script = [{monster, 20}, {monster, 50}, role], skills = [], born_points = [{120,10}], award = [{100005,600}]};
get(7) ->
    #monster_data{monster_id = 7, type = 5, name = <<"act"/utf8>>, description = <<"act"/utf8>>, level = 1, hp = 700, map_id = 0, camp = 1, range = 7, distance = 300, relive_time = 0, act_type = fix, act_script = [enemy], skills = [], born_points = [{140,10}], award = []};
get(8) ->
    #monster_data{monster_id = 8, type = 6, name = <<"boom"/utf8>>, description = <<"boom"/utf8>>, level = 1, hp = 800, map_id = 0, camp = 1, range = 8, distance = 300, relive_time = 0, act_type = fix, act_script = [{monster, 20}, {monster, 50}, role], skills = [], born_points = [{160,10}], award = []};
get(_) ->
    undefined.

-spec type(Type :: non_neg_integer()) -> [non_neg_integer()].
type(1) ->
    [1];
type(2) ->
    [2];
type(3) ->
    [3];
type(4) ->
    [4];
type(5) ->
    [5, 7];
type(6) ->
    [6, 8];
type(_) ->
    undefined.

-spec all() -> [non_neg_integer()].
all() ->
    [1, 2, 3, 4, 5, 6, 7, 8].

