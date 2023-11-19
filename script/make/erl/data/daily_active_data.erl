-module(daily_active_data).
-export([get/1]).
-include("daily.hrl").

-spec get(StageId :: non_neg_integer()) -> #daily_active_data{}.
get(1) ->
    #daily_active_data{stage_id = 1, pre_id = 0, next_id = 2, score = 30, award = [{1,1000}]};
get(2) ->
    #daily_active_data{stage_id = 2, pre_id = 1, next_id = 3, score = 50, award = [{1,1000}]};
get(3) ->
    #daily_active_data{stage_id = 3, pre_id = 2, next_id = 4, score = 80, award = [{1,1000}]};
get(4) ->
    #daily_active_data{stage_id = 4, pre_id = 3, next_id = 5, score = 100, award = [{1,1000}]};
get(5) ->
    #daily_active_data{stage_id = 5, pre_id = 4, next_id = 6, score = 120, award = [{1,1000}]};
get(6) ->
    #daily_active_data{stage_id = 6, pre_id = 5, next_id = 0, score = 150, award = [{1,1000}]};
get(_) ->
    undefined.

