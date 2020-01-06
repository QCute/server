-module(quest_data).
-compile(nowarn_export_all).
-compile(export_all).
-include("quest.hrl").


get(1) ->
    #quest_data{quest_id = 1, group_id = 1, pre_id = 0, next_id = 2, module = [], function = [], event = event_kill_monster, compare = nc, target = 0, number = 3, condition = [], award = [{1,1}], title = <<""/utf8>>, content = <<""/utf8>>, description = <<""/utf8>>};
get(2) ->
    #quest_data{quest_id = 2, group_id = 1, pre_id = 1, next_id = 3, module = role, function = check_quest, event = event_level_upgrade, compare = ge, target = 5, number = 1, condition = [{copper, 100}], award = [{1,10}], title = <<""/utf8>>, content = <<""/utf8>>, description = <<""/utf8>>};
get(3) ->
    #quest_data{quest_id = 3, group_id = 1, pre_id = 2, next_id = 4, module = [], function = [], event = event_pass_dungeon, compare = ge, target = 100001, number = 1, condition = [{level, 10}], award = [{1,100}], title = <<""/utf8>>, content = <<""/utf8>>, description = <<""/utf8>>};
get(4) ->
    #quest_data{quest_id = 4, group_id = 1, pre_id = 3, next_id = 5, module = [], function = [], event = event_shop_buy, compare = eq, target = 1, number = 1, condition = [], award = [{1,1000}], title = <<""/utf8>>, content = <<""/utf8>>, description = <<""/utf8>>};
get(5) ->
    #quest_data{quest_id = 5, group_id = 1, pre_id = 4, next_id = 0, module = [], function = [], event = event_guild_join, compare = nc, target = 0, number = 1, condition = [], award = [{1,1000}], title = <<""/utf8>>, content = <<""/utf8>>, description = <<""/utf8>>};
get(6) ->
    #quest_data{quest_id = 6, group_id = 1, pre_id = 5, next_id = 0, module = friend, function = check_quest, event = event_friend_add, compare = nc, target = 0, number = 5, condition = [], award = [{1,10}], title = <<""/utf8>>, content = <<""/utf8>>, description = <<""/utf8>>};
get(1001) ->
    #quest_data{quest_id = 1001, group_id = 2, pre_id = 0, next_id = 1002, module = [], function = [], event = event_level_upgrade, compare = ge, target = 100, number = 1, condition = [], award = [{1,10}], title = <<""/utf8>>, content = <<""/utf8>>, description = <<""/utf8>>};
get(1002) ->
    #quest_data{quest_id = 1002, group_id = 2, pre_id = 1001, next_id = 0, module = [], function = [], event = event_shop_buy, compare = eq, target = 1, number = 1, condition = [], award = [{1,10}], title = <<""/utf8>>, content = <<""/utf8>>, description = <<""/utf8>>};
get(100001) ->
    #quest_data{quest_id = 100001, group_id = 3, pre_id = 0, next_id = 100002, module = shop, function = check_quest, event = event_shop_buy, compare = eq, target = 1, number = 1, condition = [], award = [{1,10}], title = <<""/utf8>>, content = <<""/utf8>>, description = <<""/utf8>>};
get(100002) ->
    #quest_data{quest_id = 100002, group_id = 3, pre_id = 100001, next_id = 0, module = [], function = [], event = event_guild_join, compare = nc, target = 0, number = 1, condition = [], award = [{1,10}], title = <<""/utf8>>, content = <<""/utf8>>, description = <<""/utf8>>};
get(_) ->
    [].


