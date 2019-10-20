-module(activity_data).
-compile(nowarn_export_all).
-compile(export_all).
-include("activity.hrl").


get(1) ->
    #activity_data{
        activity_id = 1,
        mode = 1,
        service = [],
        type = 1,
        subtype = 1,
        award_type = 0,
        show_time = 0,
        start_time = 0,
        over_time = 0,
        award_time = 0,
        hide_time = 0,
        clean_time = 0,
        show_hour = 8,
        start_hour = 10,
        end_hour = 10,
        start_award_hour = 0,
        end_award_hour = 0,
        min_open_days = 3,
        max_open_days = 7,
        name = <<"活动名">>,
        icon = <<"activity.icon">>,
        entrance = <<"activity">>,
        description = <<"活动描述">>
    };
get(2) ->
    #activity_data{
        activity_id = 2,
        mode = 2,
        service = [],
        type = 1,
        subtype = 1,
        award_type = 0,
        show_time = 0,
        start_time = 0,
        over_time = 0,
        award_time = 0,
        hide_time = 0,
        clean_time = 0,
        show_hour = 8,
        start_hour = 10,
        end_hour = 10,
        start_award_hour = 0,
        end_award_hour = 0,
        min_open_days = 3,
        max_open_days = 7,
        name = <<"活动名">>,
        icon = <<"activity.icon">>,
        entrance = <<"activity">>,
        description = <<"活动描述">>
    };
get(3) ->
    #activity_data{
        activity_id = 3,
        mode = 4,
        service = [],
        type = 1,
        subtype = 1,
        award_type = 0,
        show_time = 0,
        start_time = 0,
        over_time = 0,
        award_time = 0,
        hide_time = 0,
        clean_time = 0,
        show_hour = 8,
        start_hour = 10,
        end_hour = 10,
        start_award_hour = 0,
        end_award_hour = 0,
        min_open_days = 3,
        max_open_days = 7,
        name = <<"活动名">>,
        icon = <<"activity.icon">>,
        entrance = <<"activity">>,
        description = <<"活动描述">>
    };
get(_) -> 
    [].

list() ->
    [1, 2, 3].

