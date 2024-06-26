-module(charge_data).
-export([get/1]).
-include("charge.hrl").

-spec get(ChargeId :: non_neg_integer()) -> #charge_data{}.
get(1) ->
    #charge_data{charge_id = 1, type = 3, limit = 1, exp = 6, original_price = 6.00000000000000000000e+00, now_price = 6.00000000000000000000e+00, gold = 6, gift_gold = 0, begin_open_days = 1, end_open_days = 9999, sort = 1, icon = <<"0"/utf8>>, name = <<"至尊神兵宝箱"/utf8>>, description = <<""/utf8>>};
get(2) ->
    #charge_data{charge_id = 2, type = 1, limit = 1, exp = 18, original_price = 1.80000000000000000000e+01, now_price = 1.80000000000000000000e+01, gold = 18, gift_gold = 5, begin_open_days = 1, end_open_days = 9999, sort = 2, icon = <<"1"/utf8>>, name = <<"元宝"/utf8>>, description = <<""/utf8>>};
get(3) ->
    #charge_data{charge_id = 3, type = 1, limit = 1, exp = 68, original_price = 6.80000000000000000000e+01, now_price = 6.80000000000000000000e+01, gold = 68, gift_gold = 40, begin_open_days = 1, end_open_days = 9999, sort = 3, icon = <<"2"/utf8>>, name = <<"元宝"/utf8>>, description = <<""/utf8>>};
get(4) ->
    #charge_data{charge_id = 4, type = 1, limit = 1, exp = 128, original_price = 1.28000000000000000000e+02, now_price = 1.28000000000000000000e+02, gold = 128, gift_gold = 90, begin_open_days = 1, end_open_days = 9999, sort = 4, icon = <<"3"/utf8>>, name = <<"元宝"/utf8>>, description = <<""/utf8>>};
get(5) ->
    #charge_data{charge_id = 5, type = 1, limit = 1, exp = 268, original_price = 2.68000000000000000000e+02, now_price = 2.68000000000000000000e+02, gold = 268, gift_gold = 190, begin_open_days = 1, end_open_days = 9999, sort = 5, icon = <<"4"/utf8>>, name = <<"元宝"/utf8>>, description = <<""/utf8>>};
get(6) ->
    #charge_data{charge_id = 6, type = 1, limit = 1, exp = 588, original_price = 5.88000000000000000000e+02, now_price = 5.88000000000000000000e+02, gold = 588, gift_gold = 330, begin_open_days = 1, end_open_days = 9999, sort = 6, icon = <<"5"/utf8>>, name = <<"元宝"/utf8>>, description = <<""/utf8>>};
get(7) ->
    #charge_data{charge_id = 7, type = 1, limit = 1, exp = 688, original_price = 6.88000000000000000000e+02, now_price = 6.88000000000000000000e+02, gold = 688, gift_gold = 590, begin_open_days = 1, end_open_days = 9999, sort = 7, icon = <<"6"/utf8>>, name = <<"元宝"/utf8>>, description = <<""/utf8>>};
get(8) ->
    #charge_data{charge_id = 8, type = 1, limit = 1, exp = 888, original_price = 8.88000000000000000000e+02, now_price = 8.88000000000000000000e+02, gold = 888, gift_gold = 1300, begin_open_days = 1, end_open_days = 9999, sort = 8, icon = <<"7"/utf8>>, name = <<"元宝"/utf8>>, description = <<""/utf8>>};
get(9) ->
    #charge_data{charge_id = 9, type = 2, limit = 1, exp = 1288, original_price = 1.28800000000000000000e+03, now_price = 1.28800000000000000000e+03, gold = 1288, gift_gold = 0, begin_open_days = 1, end_open_days = 9999, sort = 0, icon = <<""/utf8>>, name = <<"周卡"/utf8>>, description = <<""/utf8>>};
get(10) ->
    #charge_data{charge_id = 10, type = 6, limit = 1, exp = 8888, original_price = 8.88800000000000000000e+03, now_price = 8.88800000000000000000e+03, gold = 8888, gift_gold = 0, begin_open_days = 1, end_open_days = 9999, sort = 0, icon = <<""/utf8>>, name = <<"月卡"/utf8>>, description = <<""/utf8>>};
get(_) ->
    undefined.

