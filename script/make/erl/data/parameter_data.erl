-module(parameter_data).
-export([get/1]).
-export([get/2]).

-spec get(Key :: term()) -> term().
get(bag_size) ->
    100;
get(chat_cd) ->
    0;
get(chat_guild_size_limit) ->
    100;
get(chat_level) ->
    0;
get(chat_private_size_limit) ->
    100;
get(chat_system_size_limit) ->
    100;
get(chat_world_size_limit) ->
    100;
get(dungeon_inspire_buff_id) ->
    3;
get(friend_level) ->
    0;
get(friend_number) ->
    50;
get(guild_create_cd) ->
    86400;
get(guild_join_cd) ->
    86400;
get(guild_member_limit) ->
    [{0, 50}, {1, 60}, {2, 70}, {3, 80}, {4, 90}, {5, 100}];
get(item_size) ->
    100;
get(language) ->
    zhCN;
get(login_cd) ->
    180;
get(mail_expire_time) ->
    604800;
get(mail_max_item) ->
    10;
get(store_size) ->
    100;
get(time_zone) ->
    8;
get(_) ->
    undefined.

-spec get(Key :: atom(), Default :: term()) -> term().
get(Key, Default) ->
    case parameter_data:get(Key) of
        [] ->
            Default;
        Value ->
            Value
    end.
