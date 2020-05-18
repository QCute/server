-module(recharge_data).
-compile(nowarn_export_all).
-compile(export_all).
-include("recharge.hrl").


get(1) ->
    #recharge_data{recharge_id = 1, type = 3, channel_id = 0, limit = 1, original_price = 6.0, now_price = 6.0, gold = 6, gift_gold = 0, begin_open_days = 1, end_open_days = 9999, sort = 1, icon = <<"0"/utf8>>, name = <<"至尊神兵宝箱"/utf8>>, description = <<""/utf8>>};
get(2) ->
    #recharge_data{recharge_id = 2, type = 1, channel_id = 0, limit = 1, original_price = 18.0, now_price = 18.0, gold = 18, gift_gold = 5, begin_open_days = 1, end_open_days = 9999, sort = 2, icon = <<"1"/utf8>>, name = <<"元宝"/utf8>>, description = <<""/utf8>>};
get(3) ->
    #recharge_data{recharge_id = 3, type = 1, channel_id = 0, limit = 1, original_price = 68.0, now_price = 68.0, gold = 68, gift_gold = 40, begin_open_days = 1, end_open_days = 9999, sort = 3, icon = <<"2"/utf8>>, name = <<"元宝"/utf8>>, description = <<""/utf8>>};
get(4) ->
    #recharge_data{recharge_id = 4, type = 1, channel_id = 0, limit = 1, original_price = 128.0, now_price = 128.0, gold = 128, gift_gold = 90, begin_open_days = 1, end_open_days = 9999, sort = 4, icon = <<"3"/utf8>>, name = <<"元宝"/utf8>>, description = <<""/utf8>>};
get(5) ->
    #recharge_data{recharge_id = 5, type = 1, channel_id = 0, limit = 1, original_price = 268.0, now_price = 268.0, gold = 268, gift_gold = 190, begin_open_days = 1, end_open_days = 9999, sort = 5, icon = <<"4"/utf8>>, name = <<"元宝"/utf8>>, description = <<""/utf8>>};
get(6) ->
    #recharge_data{recharge_id = 6, type = 1, channel_id = 0, limit = 1, original_price = 588.0, now_price = 588.0, gold = 588, gift_gold = 330, begin_open_days = 1, end_open_days = 9999, sort = 6, icon = <<"5"/utf8>>, name = <<"元宝"/utf8>>, description = <<""/utf8>>};
get(7) ->
    #recharge_data{recharge_id = 7, type = 1, channel_id = 0, limit = 1, original_price = 688.0, now_price = 688.0, gold = 688, gift_gold = 590, begin_open_days = 1, end_open_days = 9999, sort = 7, icon = <<"6"/utf8>>, name = <<"元宝"/utf8>>, description = <<""/utf8>>};
get(8) ->
    #recharge_data{recharge_id = 8, type = 1, channel_id = 0, limit = 1, original_price = 888.0, now_price = 888.0, gold = 888, gift_gold = 1300, begin_open_days = 1, end_open_days = 9999, sort = 8, icon = <<"7"/utf8>>, name = <<"元宝"/utf8>>, description = <<""/utf8>>};
get(9) ->
    #recharge_data{recharge_id = 9, type = 2, channel_id = 0, limit = 1, original_price = 1288.0, now_price = 1288.0, gold = 1288, gift_gold = 0, begin_open_days = 1, end_open_days = 9999, sort = 0, icon = <<""/utf8>>, name = <<"周卡"/utf8>>, description = <<""/utf8>>};
get(10) ->
    #recharge_data{recharge_id = 10, type = 6, channel_id = 0, limit = 1, original_price = 8888.0, now_price = 8888.0, gold = 8888, gift_gold = 0, begin_open_days = 1, end_open_days = 9999, sort = 0, icon = <<""/utf8>>, name = <<"月卡"/utf8>>, description = <<""/utf8>>};
get(_) ->
    [].


