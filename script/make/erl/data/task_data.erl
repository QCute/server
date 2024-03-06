-module(task_data).
-export([get/1]).
-include("task.hrl").

-spec get(TaskId :: non_neg_integer()) -> #task_data{}.
get(1) ->
    #task_data{task_id = 1, type = 1, pre_id = 0, next_id = 2, event = event_kill_monster, compare = nc, target = 0, number = 3, condition = [{level, 10}], cost = [], award = [{1,1}], title = <<""/utf8>>, content = <<""/utf8>>, description = <<""/utf8>>};
get(2) ->
    #task_data{task_id = 2, type = 1, pre_id = 1, next_id = 3, event = event_level_upgrade, compare = ge, target = 5, number = 1, condition = [{sex, 1}], cost = [{100003, 100}], award = [{1,10}], title = <<""/utf8>>, content = <<""/utf8>>, description = <<""/utf8>>};
get(3) ->
    #task_data{task_id = 3, type = 1, pre_id = 2, next_id = 4, event = event_dungeon_passed, compare = gt, target = 2, number = 1, condition = [{level, 10},{classes,2}], cost = [], award = [{1,100}], title = <<""/utf8>>, content = <<""/utf8>>, description = <<""/utf8>>};
get(4) ->
    #task_data{task_id = 4, type = 1, pre_id = 3, next_id = 5, event = event_shop_buy, compare = eq, target = 1, number = 1, condition = [{vip, 3}], cost = [], award = [{1,1000}], title = <<""/utf8>>, content = <<""/utf8>>, description = <<""/utf8>>};
get(5) ->
    #task_data{task_id = 5, type = 1, pre_id = 4, next_id = 0, event = event_guild_join, compare = nc, target = 0, number = 1, condition = [{classes, 1},{level, 2},{sex, 3},{vip, 4}], cost = [], award = [{1,1000}], title = <<""/utf8>>, content = <<""/utf8>>, description = <<""/utf8>>};
get(6) ->
    #task_data{task_id = 6, type = 1, pre_id = 5, next_id = 0, event = event_add_friend, compare = nc, target = 0, number = 5, condition = [], cost = [], award = [{1,10}], title = <<""/utf8>>, content = <<""/utf8>>, description = <<""/utf8>>};
get(1001) ->
    #task_data{task_id = 1001, type = 2, pre_id = 0, next_id = 1002, event = event_dungeon_exp_passed, compare = ge, target = 3, number = 1, condition = [], cost = [], award = [{1,10}], title = <<""/utf8>>, content = <<""/utf8>>, description = <<""/utf8>>};
get(1002) ->
    #task_data{task_id = 1002, type = 2, pre_id = 1001, next_id = 0, event = event_friend_add, compare = eq, target = 1, number = 1, condition = [], cost = [], award = [{1,10}], title = <<""/utf8>>, content = <<""/utf8>>, description = <<""/utf8>>};
get(100001) ->
    #task_data{task_id = 100001, type = 3, pre_id = 0, next_id = 100002, event = event_dungeon_copper_passed, compare = eq, target = 1, number = 1, condition = [], cost = [], award = [{1,10}], title = <<""/utf8>>, content = <<""/utf8>>, description = <<""/utf8>>};
get(100002) ->
    #task_data{task_id = 100002, type = 3, pre_id = 100001, next_id = 0, event = event_guild_join, compare = nc, target = 0, number = 1, condition = [], cost = [], award = [{1,10}], title = <<""/utf8>>, content = <<""/utf8>>, description = <<""/utf8>>};
get(_) ->
    undefined.

