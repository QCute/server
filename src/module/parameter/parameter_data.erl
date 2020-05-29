-module(parameter_data).
-compile(nowarn_export_all).
-compile(export_all).


get(bag_size) ->
    100;
get(chat_cd) ->
    30;
get(chat_level) ->
    10;
get(friend_level) ->
    30;
get(friend_number) ->
    50;
get(guild_create) ->
    [{1, [{level, 1}, {vip, 1}], [{gold, 1}]}, {2, [{level, 2}, {vip, 2}], [{gold, 2}]},{3, [{level, 3}, {vip, 3}], [{gold, 3}]}];
get(guild_create_cd) ->
    86400;
get(guild_join_cd) ->
    86400;
get(guild_member_limit) ->
    [{0, 50}, {1, 60}, {2, 70}, {3, 80}, {4, 90}, {5, 100}];
get(item_size) ->
    100;
get(language) ->
    sc;
get(language_set) ->
    [{1, sc}, {2, tc}, {3, en}, {4, kr}, {5, vi}];
get(login_cd) ->
    180;
get(store_size) ->
    100;
get(time_zone) ->
    +8;
get(_) ->
    [].


