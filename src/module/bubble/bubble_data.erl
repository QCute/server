-module(bubble_data).
-export([get/1]).

-include("bubble.hrl").

-spec get(BubbleId :: integer()) -> BubbleData :: #bubble_data{} | Default :: [].
get(101) ->
    #bubble_data{bubble_id = 101, type = 1, tag = 0, expire_time = 0, name = <<"VIP1可获得"/utf8>>, description = <<"小试牛刀"/utf8>>};
get(102) ->
    #bubble_data{bubble_id = 102, type = 1, tag = 0, expire_time = 0, name = <<"VIP2可获得"/utf8>>, description = <<"有钱任性"/utf8>>};
get(103) ->
    #bubble_data{bubble_id = 103, type = 1, tag = 0, expire_time = 0, name = <<"VIP3可获得"/utf8>>, description = <<"一掷千金"/utf8>>};
get(104) ->
    #bubble_data{bubble_id = 104, type = 1, tag = 0, expire_time = 0, name = <<"VIP4可获得"/utf8>>, description = <<"腰缠万贯"/utf8>>};
get(105) ->
    #bubble_data{bubble_id = 105, type = 1, tag = 0, expire_time = 0, name = <<"VIP5可获得"/utf8>>, description = <<"挥金如土"/utf8>>};
get(106) ->
    #bubble_data{bubble_id = 106, type = 1, tag = 0, expire_time = 0, name = <<"VIP6可获得"/utf8>>, description = <<"富甲天下"/utf8>>};
get(107) ->
    #bubble_data{bubble_id = 107, type = 1, tag = 0, expire_time = 0, name = <<"VIP7可获得"/utf8>>, description = <<"富可敌国"/utf8>>};
get(108) ->
    #bubble_data{bubble_id = 108, type = 1, tag = 0, expire_time = 0, name = <<"VIP8可获得"/utf8>>, description = <<"人生巅峰"/utf8>>};
get(109) ->
    #bubble_data{bubble_id = 109, type = 1, tag = 0, expire_time = 0, name = <<"VIP9可获得"/utf8>>, description = <<"至尊王者"/utf8>>};
get(110) ->
    #bubble_data{bubble_id = 110, type = 1, tag = 0, expire_time = 0, name = <<"VIP0可获得"/utf8>>, description = <<"高手对决"/utf8>>};
get(201) ->
    #bubble_data{bubble_id = 201, type = 2, tag = 0, expire_time = 0, name = <<"开服冲榜活动获取"/utf8>>, description = <<"武艺超群"/utf8>>};
get(202) ->
    #bubble_data{bubble_id = 202, type = 2, tag = 0, expire_time = 0, name = <<"开服冲榜活动获取"/utf8>>, description = <<"出神入化"/utf8>>};
get(203) ->
    #bubble_data{bubble_id = 203, type = 2, tag = 0, expire_time = 0, name = <<"开服冲榜活动获取"/utf8>>, description = <<"仙武主宰"/utf8>>};
get(204) ->
    #bubble_data{bubble_id = 204, type = 2, tag = 0, expire_time = 0, name = <<"开服冲榜活动获取"/utf8>>, description = <<"锻造大师"/utf8>>};
get(205) ->
    #bubble_data{bubble_id = 205, type = 2, tag = 0, expire_time = 0, name = <<"开服冲榜活动获取"/utf8>>, description = <<"黑暗主宰"/utf8>>};
get(206) ->
    #bubble_data{bubble_id = 206, type = 2, tag = 0, expire_time = 0, name = <<"开服冲榜活动获取"/utf8>>, description = <<"聚魂先锋"/utf8>>};
get(207) ->
    #bubble_data{bubble_id = 207, type = 2, tag = 0, expire_time = 0, name = <<"开服冲榜活动获取"/utf8>>, description = <<"全职高手"/utf8>>};
get(208) ->
    #bubble_data{bubble_id = 208, type = 2, tag = 0, expire_time = 0, name = <<"开服冲榜活动获取"/utf8>>, description = <<"人中之龙"/utf8>>};
get(209) ->
    #bubble_data{bubble_id = 209, type = 2, tag = 0, expire_time = 0, name = <<"开服冲榜活动获取"/utf8>>, description = <<"勇者无畏"/utf8>>};
get(210) ->
    #bubble_data{bubble_id = 210, type = 2, tag = 0, expire_time = 0, name = <<"开服冲榜活动获取"/utf8>>, description = <<"称霸天下"/utf8>>};
get(10010) ->
    #bubble_data{bubble_id = 10010, type = 3, tag = 0, expire_time = 0, name = <<"充值获取"/utf8>>, description = <<"归隐山林"/utf8>>};
get(_) ->
    [].


