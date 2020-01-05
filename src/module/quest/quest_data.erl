-module(quest_data).
-compile(nowarn_export_all).
-compile(export_all).
-include("quest.hrl").


get(1) ->
    #quest_data{quest_id = 1, group_id = 1, pre_id = 0, next_id = 2, event = event_kill_monster, target = 0, number = 3, compare = nc, condition = [], award = [{1,1}], title = <<""/utf8>>, content = <<""/utf8>>, description = <<""/utf8>>};
get(2) ->
    #quest_data{quest_id = 2, group_id = 1, pre_id = 1, next_id = 3, event = event_level_upgrade, target = 5, number = 1, compare = ge, condition = [{copper, 100}], award = [{1,10}], title = <<""/utf8>>, content = <<""/utf8>>, description = <<""/utf8>>};
get(3) ->
    #quest_data{quest_id = 3, group_id = 1, pre_id = 2, next_id = 4, event = event_pass_dungeon, target = 100001, number = 1, compare = ge, condition = [{level, 10}], award = [{1,100}], title = <<""/utf8>>, content = <<""/utf8>>, description = <<""/utf8>>};
get(4) ->
    #quest_data{quest_id = 4, group_id = 1, pre_id = 3, next_id = 5, event = event_shop_buy, target = 100001, number = 1, compare = eq, condition = [], award = [{1,1000}], title = <<""/utf8>>, content = <<""/utf8>>, description = <<""/utf8>>};
get(5) ->
    #quest_data{quest_id = 5, group_id = 1, pre_id = 4, next_id = 0, event = event_guild_join, target = 0, number = 1, compare = nc, condition = [], award = [{1,1000}], title = <<""/utf8>>, content = <<""/utf8>>, description = <<""/utf8>>};
get(1001) ->
    #quest_data{quest_id = 1001, group_id = 2, pre_id = 0, next_id = 1002, event = event_level_upgrade, target = 100, number = 1, compare = ge, condition = [], award = [{1,10}], title = <<""/utf8>>, content = <<""/utf8>>, description = <<""/utf8>>};
get(1002) ->
    #quest_data{quest_id = 1002, group_id = 2, pre_id = 1001, next_id = 0, event = event_shop_buy, target = 1, number = 1, compare = ge, condition = [], award = [{1,10}], title = <<""/utf8>>, content = <<""/utf8>>, description = <<""/utf8>>};
get(100001) ->
    #quest_data{quest_id = 100001, group_id = 3, pre_id = 0, next_id = 100002, event = event_shop_buy, target = 1, number = 1, compare = eq, condition = [], award = [], title = <<""/utf8>>, content = <<""/utf8>>, description = <<""/utf8>>};
get(100002) ->
    #quest_data{quest_id = 100002, group_id = 3, pre_id = 100001, next_id = 0, event = event_guild_join, target = 0, number = 1, compare = nc, condition = [], award = [], title = <<""/utf8>>, content = <<""/utf8>>, description = <<""/utf8>>};
get(_) ->
    [].


