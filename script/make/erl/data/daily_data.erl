-module(daily_data).
-export([get/1]).
-export([list/0]).
-include("daily.hrl").

-spec get(DailyId :: non_neg_integer()) -> #daily_data{}.
get(123100100) ->
    #daily_data{daily_id = 123100100, type = 1, count_type = 1001, number = 3, score = 20, award = [{111000300,10}], description = <<"参与3场比赛"/utf8>>};
get(123100200) ->
    #daily_data{daily_id = 123100200, type = 1, count_type = 1002, number = 1, score = 2, award = [{111000300,10}], description = <<"挑战1次排行榜"/utf8>>};
get(123100300) ->
    #daily_data{daily_id = 123100300, type = 1, count_type = 1003, number = 5, score = 3, award = [{111000300,10}], description = <<"观看满5个视频"/utf8>>};
get(123100400) ->
    #daily_data{daily_id = 123100400, type = 1, count_type = 1004, number = 3, score = 4, award = [{111000300,10}], description = <<"胜利3场"/utf8>>};
get(123100500) ->
    #daily_data{daily_id = 123100500, type = 1, count_type = 1005, number = 50, score = 5, award = [{111000300,10}], description = <<"胜率超过50%"/utf8>>};
get(_) ->
    undefined.

-spec list() -> [#daily_data{}].
list() ->
    [
        #daily_data{daily_id = 123100100, type = 1, count_type = 1001, number = 3, score = 20, award = [{111000300,10}], description = <<"参与3场比赛"/utf8>>},
        #daily_data{daily_id = 123100200, type = 1, count_type = 1002, number = 1, score = 2, award = [{111000300,10}], description = <<"挑战1次排行榜"/utf8>>},
        #daily_data{daily_id = 123100300, type = 1, count_type = 1003, number = 5, score = 3, award = [{111000300,10}], description = <<"观看满5个视频"/utf8>>},
        #daily_data{daily_id = 123100400, type = 1, count_type = 1004, number = 3, score = 4, award = [{111000300,10}], description = <<"胜利3场"/utf8>>},
        #daily_data{daily_id = 123100500, type = 1, count_type = 1005, number = 50, score = 5, award = [{111000300,10}], description = <<"胜率超过50%"/utf8>>}
    ].

