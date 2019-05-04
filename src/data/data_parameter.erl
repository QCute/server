-module(data_parameter).
-compile(nowarn_export_all).
-compile(export_all).


get(null) ->
    [];
get(test) ->
    [];
get({guild_create, 1}) ->
    [{level, 10}, {vip, 0}, {gold, 0}];
get({guild_create, 2}) ->
    [{level, 10}, {vip, 1}, {gold, 100}];
get({guild_create, cd}) ->
    86400;
get({guild_member, limit, 0}) ->
    3;
get(_) -> 
    [].

