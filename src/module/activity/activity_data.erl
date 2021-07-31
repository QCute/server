-module(activity_data).
-compile(nowarn_export_all).
-compile(export_all).
-include("activity.hrl").


get(1) ->
    #activity_data{activity_id = 1, mode = 1, service = auction_server, type = 1, subtype = 1, award_type = manual, show_time = 1577808000, start_time = 1577808000, over_time = 1577808000, award_time = 1577808000, stop_time = 1577808000, show_hour = 9, start_hour = 10, over_hour = 22, start_award_hour = 22, over_award_hour = 23, min_open_days = 3, max_open_days = 7, name = <<"活动名"/utf8>>, icon = <<"activity.icon"/utf8>>, entrance = <<"activity"/utf8>>, description = <<"活动描述"/utf8>>};
get(2) ->
    #activity_data{activity_id = 2, mode = 2, service = boss_server, type = 1, subtype = 1, award_type = manual, show_time = 1577808000, start_time = 1577808000, over_time = 1577808000, award_time = 1577808000, stop_time = 1577808000, show_hour = 9, start_hour = 10, over_hour = 22, start_award_hour = 22, over_award_hour = 23, min_open_days = 3, max_open_days = 7, name = <<"活动名"/utf8>>, icon = <<"activity.icon"/utf8>>, entrance = <<"activity"/utf8>>, description = <<"活动描述"/utf8>>};
get(3) ->
    #activity_data{activity_id = 3, mode = 4, service = [], type = 1, subtype = 1, award_type = manual, show_time = 1577808000, start_time = 1577808000, over_time = 1577808000, award_time = 1577808000, stop_time = 1577808000, show_hour = 9, start_hour = 10, over_hour = 22, start_award_hour = 22, over_award_hour = 23, min_open_days = 3, max_open_days = 7, name = <<"活动名"/utf8>>, icon = <<"activity.icon"/utf8>>, entrance = <<"activity"/utf8>>, description = <<"活动描述"/utf8>>};
get(_) ->
    [].


list() ->
    [1, 2, 3].


