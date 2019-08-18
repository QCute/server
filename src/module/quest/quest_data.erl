-module(quest_data).
-compile(nowarn_export_all).
-compile(export_all).
-include("quest.hrl").


get(1) ->
    #quest_data{
        quest_id = 1,
        group_id = 1,
        pre_id = 0,
        next_id = 2,
        event = event_kill_monster,
        target = 0,
        amount = 3,
        compare = gte,
        condition = [],
        progress = [{quest_progress, event_kill_monster, 3, gt}],
        award = [{1,1}],
        title = <<"">>,
        content = <<"">>,
        description = <<"">>
    };
get(2) ->
    #quest_data{
        quest_id = 2,
        group_id = 1,
        pre_id = 1,
        next_id = 3,
        event = event_level_upgrade,
        target = 5,
        amount = 1,
        compare = gte,
        condition = [{copper, 100}],
        progress = [{quest_progress, event_level_upgrade, 10, gt}],
        award = [{1,10}],
        title = <<"">>,
        content = <<"">>,
        description = <<"">>
    };
get(3) ->
    #quest_data{
        quest_id = 3,
        group_id = 1,
        pre_id = 2,
        next_id = 4,
        event = event_pass_dungeon,
        target = 100001,
        amount = 1,
        compare = gte,
        condition = [{level, 10}],
        progress = [{quest_progress, event_dungeon, 5, gt}],
        award = [{1,100}],
        title = <<"">>,
        content = <<"">>,
        description = <<"">>
    };
get(4) ->
    #quest_data{
        quest_id = 4,
        group_id = 1,
        pre_id = 3,
        next_id = 5,
        event = event_shop_buy,
        target = 0,
        amount = 1,
        compare = nc,
        condition = [],
        progress = [{quest_progress, event_shop_buy, 1, eq}],
        award = [{1,1000}],
        title = <<"">>,
        content = <<"">>,
        description = <<"">>
    };
get(5) ->
    #quest_data{
        quest_id = 5,
        group_id = 1,
        pre_id = 4,
        next_id = 0,
        event = event_guild_join,
        target = 0,
        amount = 1,
        compare = nc,
        condition = [],
        progress = [{quest_progress, event_level_upgrade, 10, gt}],
        award = [{1,1000}],
        title = <<"">>,
        content = <<"">>,
        description = <<"">>
    };
get(_) -> 
    [].

