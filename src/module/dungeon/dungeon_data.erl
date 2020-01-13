-module(dungeon_data).
-compile(nowarn_export_all).
-compile(export_all).
-include("dungeon.hrl").


get(1) ->
    #dungeon_data{dungeon_id = 1, type = 1, event = [], condition = [{level,10}], cost = [{100003,100}], day_number = [{0,1}], buy_number = [{0,1}], map_id = 100001, monsters = [], time = 3600, award = [{100004,100}], name = <<""/utf8>>, description = <<""/utf8>>};
get(_) ->
    [].


