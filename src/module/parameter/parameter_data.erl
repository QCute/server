-module(parameter_data).
-compile(nowarn_export_all).
-compile(export_all).


get(chat_cd) ->
    30;
get(chat_level) ->
    10;
get(friend_level) ->
    30;
get(friend_number) ->
    50;
get(guild_create_cd) ->
    86400;
get(guild_join_cd) ->
    86400;
get(lang) ->
    sc;
get(login_cd) ->
    180;
get(time_zone) ->
    +8;
get({guild_create, 1}) ->
    [{level, 10}, {vip, 0}, {gold, 0}];
get({guild_create, 2}) ->
    [{level, 50}, {vip, 1}, {gold, 100}];
get({guild_create, 3}) ->
    [{level, 100}, {vip, 3}, {gold, 500}];
get({guild_member_limit, 0}) ->
    50;
get({guild_member_limit, 1}) ->
    60;
get({guild_member_limit, 2}) ->
    70;
get({guild_member_limit, 3}) ->
    80;
get({guild_member_limit, 4}) ->
    90;
get({guild_member_limit, 5}) ->
    100;
get(_) ->
    [].


