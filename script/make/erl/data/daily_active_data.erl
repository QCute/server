-module(daily_active_data).
-export([get/1]).
-export([list/0]).
-include("daily.hrl").

-spec get(StageId :: non_neg_integer()) -> #daily_active_data{}.
get(123500100) ->
    #daily_active_data{stage_id = 123500100, pre_id = 0, next_id = 123500200, score = 20, award = [{111000300,10}]};
get(123500200) ->
    #daily_active_data{stage_id = 123500200, pre_id = 123500100, next_id = 123500300, score = 40, award = [{111000300,30}]};
get(123500300) ->
    #daily_active_data{stage_id = 123500300, pre_id = 123500200, next_id = 123500400, score = 60, award = [{111000300,50}]};
get(123500400) ->
    #daily_active_data{stage_id = 123500400, pre_id = 123500300, next_id = 123500500, score = 80, award = [{111000300,80}]};
get(123500500) ->
    #daily_active_data{stage_id = 123500500, pre_id = 123500400, next_id = 0, score = 100, award = [{111000300,100}]};
get(_) ->
    undefined.

-spec list() -> [#daily_active_data{}].
list() ->
    [
        #daily_active_data{stage_id = 123500100, pre_id = 0, next_id = 123500200, score = 20, award = [{111000300,10}]},
        #daily_active_data{stage_id = 123500200, pre_id = 123500100, next_id = 123500300, score = 40, award = [{111000300,30}]},
        #daily_active_data{stage_id = 123500300, pre_id = 123500200, next_id = 123500400, score = 60, award = [{111000300,50}]},
        #daily_active_data{stage_id = 123500400, pre_id = 123500300, next_id = 123500500, score = 80, award = [{111000300,80}]},
        #daily_active_data{stage_id = 123500500, pre_id = 123500400, next_id = 0, score = 100, award = [{111000300,100}]}
    ].

