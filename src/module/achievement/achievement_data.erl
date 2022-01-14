-module(achievement_data).
-export([get/1]).
-export([first/1]).
-export([last/1]).
-export([type/1]).

-include("achievement.hrl").

-spec get(AchievementId :: integer()) -> AchievementData :: #achievement_data{} | Default :: [].
get(1) ->
    #achievement_data{achievement_id = 1, type = 1, count_type = 1, pre_id = 0, next_id = 2, event = event_level_upgrade, target = 0, number = 3, award = [{1,1}], title = <<""/utf8>>, content = <<""/utf8>>, description = <<""/utf8>>};
get(2) ->
    #achievement_data{achievement_id = 2, type = 1, count_type = 2, pre_id = 1, next_id = 3, event = event_level_upgrade, target = 5, number = 1, award = [{1,10}], title = <<""/utf8>>, content = <<""/utf8>>, description = <<""/utf8>>};
get(3) ->
    #achievement_data{achievement_id = 3, type = 1, count_type = 3, pre_id = 2, next_id = 0, event = event_level_upgrade, target = 2, number = 1, award = [{1,100}], title = <<""/utf8>>, content = <<""/utf8>>, description = <<""/utf8>>};
get(4) ->
    #achievement_data{achievement_id = 4, type = 2, count_type = 4, pre_id = 0, next_id = 4, event = event_shop_buy, target = 1, number = 1, award = [{1,1000}], title = <<""/utf8>>, content = <<""/utf8>>, description = <<""/utf8>>};
get(5) ->
    #achievement_data{achievement_id = 5, type = 2, count_type = 5, pre_id = 4, next_id = 5, event = event_shop_buy, target = 0, number = 1, award = [{1,1000}], title = <<""/utf8>>, content = <<""/utf8>>, description = <<""/utf8>>};
get(6) ->
    #achievement_data{achievement_id = 6, type = 2, count_type = 6, pre_id = 5, next_id = 0, event = event_shop_buy, target = 0, number = 5, award = [{1,10}], title = <<""/utf8>>, content = <<""/utf8>>, description = <<""/utf8>>};
get(7) ->
    #achievement_data{achievement_id = 7, type = 3, count_type = 7, pre_id = 0, next_id = 8, event = event_dungeon_passed, target = 3, number = 1, award = [{1,10}], title = <<""/utf8>>, content = <<""/utf8>>, description = <<""/utf8>>};
get(8) ->
    #achievement_data{achievement_id = 8, type = 3, count_type = 8, pre_id = 8, next_id = 9, event = event_dungeon_passed, target = 1, number = 1, award = [{1,10}], title = <<""/utf8>>, content = <<""/utf8>>, description = <<""/utf8>>};
get(9) ->
    #achievement_data{achievement_id = 9, type = 3, count_type = 9, pre_id = 9, next_id = 0, event = event_dungeon_passed, target = 1, number = 1, award = [{1,10}], title = <<""/utf8>>, content = <<""/utf8>>, description = <<""/utf8>>};
get(_) ->
    [].


-spec first(Type :: integer()) -> MinAchievementId :: integer() | Default :: [].
first(1) ->
    1;
first(2) ->
    4;
first(3) ->
    7;
first(_) ->
    [].


-spec last(Type :: integer()) -> MaxAchievementId :: integer() | Default :: [].
last(1) ->
    3;
last(2) ->
    6;
last(3) ->
    9;
last(_) ->
    [].


-spec type(Type :: integer()) -> [AchievementId :: integer()] | Default :: [].
type(1) ->
    [1, 2, 3];
type(2) ->
    [4, 5, 6];
type(3) ->
    [7, 8, 9];
type(_) ->
    [].


