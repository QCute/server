-module(daily_data).
-export([get/1]).
-include("daily.hrl").

-spec get(DailyId :: non_neg_integer()) -> #daily_data{}.
get(1) ->
    #daily_data{daily_id = 1, type = 1, count_type = 1, number = 1, score = 1, award = [{1,1000}]};
get(2) ->
    #daily_data{daily_id = 2, type = 1, count_type = 2, number = 2, score = 2, award = [{1,1000}]};
get(3) ->
    #daily_data{daily_id = 3, type = 1, count_type = 3, number = 3, score = 3, award = [{1,1000}]};
get(4) ->
    #daily_data{daily_id = 4, type = 1, count_type = 4, number = 4, score = 4, award = [{1,1000}]};
get(5) ->
    #daily_data{daily_id = 5, type = 1, count_type = 5, number = 5, score = 5, award = [{1,1000}]};
get(6) ->
    #daily_data{daily_id = 6, type = 1, count_type = 6, number = 6, score = 6, award = [{1,1000}]};
get(7) ->
    #daily_data{daily_id = 7, type = 1, count_type = 7, number = 7, score = 7, award = [{1,1000}]};
get(8) ->
    #daily_data{daily_id = 8, type = 1, count_type = 8, number = 8, score = 8, award = [{1,1000}]};
get(9) ->
    #daily_data{daily_id = 9, type = 1, count_type = 9, number = 9, score = 9, award = [{1,1000}]};
get(_) ->
    undefined.

