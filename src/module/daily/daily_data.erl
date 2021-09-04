-module(daily_data).
-compile(nowarn_export_all).
-compile(export_all).
-include("daily.hrl").


get_daily(1) ->
    #daily_data{daily_id = 1, type = 1, count_type = 1, number = 1, score = 1, award = [{1,1000}]};
get_daily(2) ->
    #daily_data{daily_id = 2, type = 1, count_type = 2, number = 2, score = 2, award = [{1,1000}]};
get_daily(3) ->
    #daily_data{daily_id = 3, type = 1, count_type = 3, number = 3, score = 3, award = [{1,1000}]};
get_daily(4) ->
    #daily_data{daily_id = 4, type = 1, count_type = 4, number = 4, score = 4, award = [{1,1000}]};
get_daily(5) ->
    #daily_data{daily_id = 5, type = 1, count_type = 5, number = 5, score = 5, award = [{1,1000}]};
get_daily(6) ->
    #daily_data{daily_id = 6, type = 1, count_type = 6, number = 6, score = 6, award = [{1,1000}]};
get_daily(7) ->
    #daily_data{daily_id = 7, type = 1, count_type = 7, number = 7, score = 7, award = [{1,1000}]};
get_daily(8) ->
    #daily_data{daily_id = 8, type = 1, count_type = 8, number = 8, score = 8, award = [{1,1000}]};
get_daily(9) ->
    #daily_data{daily_id = 9, type = 1, count_type = 9, number = 9, score = 9, award = [{1,1000}]};
get_daily(_) ->
    [].


get_daily_active(1) ->
    #daily_active_data{stage_id = 1, pre_id = 0, next_id = 2, score = 30, award = [{1,1000}]};
get_daily_active(2) ->
    #daily_active_data{stage_id = 2, pre_id = 1, next_id = 3, score = 50, award = [{1,1000}]};
get_daily_active(3) ->
    #daily_active_data{stage_id = 3, pre_id = 2, next_id = 4, score = 80, award = [{1,1000}]};
get_daily_active(4) ->
    #daily_active_data{stage_id = 4, pre_id = 3, next_id = 5, score = 100, award = [{1,1000}]};
get_daily_active(5) ->
    #daily_active_data{stage_id = 5, pre_id = 4, next_id = 6, score = 120, award = [{1,1000}]};
get_daily_active(6) ->
    #daily_active_data{stage_id = 6, pre_id = 5, next_id = 0, score = 150, award = [{1,1000}]};
get_daily_active(_) ->
    [].


