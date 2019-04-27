-module(data_quest).
-compile(nowarn_export_all).
-compile(export_all).
-include("quest.hrl").


get(1) ->
    #data_quest{
        quest_id = 1,
        group_id = 1,
        pre_id = 0,
        next_id = 2,
        condition = [],
        progress = [{quest_progress, event_kill_monster, 3, gt}],
        award = [{1,1}]
    };
get(2) ->
    #data_quest{
        quest_id = 2,
        group_id = 1,
        pre_id = 1,
        next_id = 3,
        condition = [{copper, 100}],
        progress = [{quest_progress, event_level_upgrade, 10, gt}],
        award = [{1,10}]
    };
get(3) ->
    #data_quest{
        quest_id = 3,
        group_id = 1,
        pre_id = 2,
        next_id = 0,
        condition = [{level, 10}],
        progress = [{quest_progress, event_dungeon, 5, gt}],
        award = [{1,100}]
    };
get(_) -> 
    [].

