-module(fashion_data).
-export([get/1]).

-include("fashion.hrl").

-spec get(FashionId :: integer()) -> FashionData :: #fashion_data{} | Default :: [].
get(101) ->
    #fashion_data{fashion_id = 101, type = 1, is_unique = false, expire_time = 0, attribute = [{3,30},{4,40}], name = <<"小试牛刀"/utf8>>, description = <<"VIP1可获得"/utf8>>};
get(102) ->
    #fashion_data{fashion_id = 102, type = 1, is_unique = false, expire_time = 0, attribute = [{3,30},{4,40}], name = <<"有钱任性"/utf8>>, description = <<"VIP2可获得"/utf8>>};
get(103) ->
    #fashion_data{fashion_id = 103, type = 1, is_unique = false, expire_time = 0, attribute = [{3,30},{4,40}], name = <<"一掷千金"/utf8>>, description = <<"VIP3可获得"/utf8>>};
get(104) ->
    #fashion_data{fashion_id = 104, type = 1, is_unique = false, expire_time = 0, attribute = [{3,30},{4,40}], name = <<"腰缠万贯"/utf8>>, description = <<"VIP4可获得"/utf8>>};
get(105) ->
    #fashion_data{fashion_id = 105, type = 1, is_unique = false, expire_time = 0, attribute = [{3,30},{4,40}], name = <<"挥金如土"/utf8>>, description = <<"VIP5可获得"/utf8>>};
get(106) ->
    #fashion_data{fashion_id = 106, type = 1, is_unique = false, expire_time = 0, attribute = [{3,30},{4,40}], name = <<"富甲天下"/utf8>>, description = <<"VIP6可获得"/utf8>>};
get(107) ->
    #fashion_data{fashion_id = 107, type = 1, is_unique = false, expire_time = 0, attribute = [{3,30},{4,40}], name = <<"富可敌国"/utf8>>, description = <<"VIP7可获得"/utf8>>};
get(108) ->
    #fashion_data{fashion_id = 108, type = 1, is_unique = false, expire_time = 0, attribute = [{3,30},{4,40}], name = <<"人生巅峰"/utf8>>, description = <<"VIP8可获得"/utf8>>};
get(109) ->
    #fashion_data{fashion_id = 109, type = 1, is_unique = false, expire_time = 0, attribute = [{3,30},{4,40}], name = <<"至尊王者"/utf8>>, description = <<"VIP9可获得"/utf8>>};
get(110) ->
    #fashion_data{fashion_id = 110, type = 1, is_unique = false, expire_time = 0, attribute = [{3,30},{4,40}], name = <<"高手对决"/utf8>>, description = <<"VIP0可获得"/utf8>>};
get(201) ->
    #fashion_data{fashion_id = 201, type = 2, is_unique = false, expire_time = 0, attribute = [{6,60},{7,70}], name = <<"武艺超群"/utf8>>, description = <<"开服冲榜活动获取"/utf8>>};
get(202) ->
    #fashion_data{fashion_id = 202, type = 2, is_unique = false, expire_time = 0, attribute = [{6,60},{7,70}], name = <<"出神入化"/utf8>>, description = <<"开服冲榜活动获取"/utf8>>};
get(203) ->
    #fashion_data{fashion_id = 203, type = 2, is_unique = false, expire_time = 0, attribute = [{6,60},{7,70}], name = <<"仙武主宰"/utf8>>, description = <<"开服冲榜活动获取"/utf8>>};
get(204) ->
    #fashion_data{fashion_id = 204, type = 2, is_unique = false, expire_time = 0, attribute = [{6,60},{7,70}], name = <<"锻造大师"/utf8>>, description = <<"开服冲榜活动获取"/utf8>>};
get(205) ->
    #fashion_data{fashion_id = 205, type = 2, is_unique = false, expire_time = 0, attribute = [{6,60},{7,70}], name = <<"黑暗主宰"/utf8>>, description = <<"开服冲榜活动获取"/utf8>>};
get(206) ->
    #fashion_data{fashion_id = 206, type = 2, is_unique = false, expire_time = 0, attribute = [{6,60},{7,70}], name = <<"聚魂先锋"/utf8>>, description = <<"开服冲榜活动获取"/utf8>>};
get(207) ->
    #fashion_data{fashion_id = 207, type = 2, is_unique = false, expire_time = 0, attribute = [{6,60},{7,70}], name = <<"全职高手"/utf8>>, description = <<"开服冲榜活动获取"/utf8>>};
get(208) ->
    #fashion_data{fashion_id = 208, type = 2, is_unique = false, expire_time = 0, attribute = [{6,60},{7,70}], name = <<"人中之龙"/utf8>>, description = <<"开服冲榜活动获取"/utf8>>};
get(209) ->
    #fashion_data{fashion_id = 209, type = 2, is_unique = false, expire_time = 0, attribute = [{6,60},{7,70}], name = <<"勇者无畏"/utf8>>, description = <<"开服冲榜活动获取"/utf8>>};
get(210) ->
    #fashion_data{fashion_id = 210, type = 2, is_unique = false, expire_time = 0, attribute = [{6,60},{7,70}], name = <<"称霸天下"/utf8>>, description = <<"开服冲榜活动获取"/utf8>>};
get(10010) ->
    #fashion_data{fashion_id = 10010, type = 3, is_unique = true, expire_time = 604800, attribute = [{5,50}], name = <<"归隐山林"/utf8>>, description = <<"充值获取"/utf8>>};
get(_) ->
    [].


