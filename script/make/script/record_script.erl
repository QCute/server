%%%------------------------------------------------------------------
%%% @doc
%%% module record script
%%% @end
%%%------------------------------------------------------------------
-module(record_script).
-export([main/1]).
%% ------------------------ user guide -------------------------------
%%
%% default value guide
%% tinyint/smallint/int/bigint                         => your sql default value
%% varchar(0)                                          => 0
%% varchar                                             => []
%% char/char(0)                                        => <<>>
%% varchar/char with default(value) in comment         => value
%% 
%%%==================================================================
%%% API functions
%%%==================================================================
main([Key]) ->
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    Record = [X || X <- record(), filename:basename(element(1, X), ".hrl") == Key],
    Name = hd(string:tokens(Key, "_")),
    List = tool:default(Record, [{"include/" ++ Name ++ ".hrl", Key}]),
    io:format("~p~n", [catch record_maker:start(List)]);
main(_) ->
    io:format("invalid argument~n").

%%%==================================================================
%%% record data
%%%==================================================================
record() ->
    [
        {"include/role.hrl", role},
        {"include/asset.hrl", asset},
        {"include/vip.hrl", vip},
        {"include/count.hrl", count},
        {"include/item.hrl", item},
        {"include/item.hrl", item_data},
        {"include/quest.hrl", quest},
        {"include/quest.hrl", quest_data},
        {"include/shop.hrl", shop},
        {"include/shop.hrl", shop_data},
        {"include/mail.hrl", mail},
        {"include/friend.hrl", friend},
        {"include/skill.hrl", skill},
        {"include/skill.hrl", skill_data},
        {"include/buff.hrl", buff},
        {"include/buff.hrl", buff_data},
        {"include/title.hrl", title},
        {"include/title.hrl", title_data},
        {"include/key.hrl", key_award_data},
        {"include/activity.hrl", activity_data},
        {"include/auction.hrl", auction},
        {"include/auction.hrl", auction_role},
        {"include/auction.hrl", auction_data},
        {"include/map.hrl", map_data},
        {"include/monster.hrl", monster_data},
        {"include/dungeon.hrl", dungeon},
        {"include/dungeon.hrl", dungeon_data},
        {"include/rank.hrl", rank},
        {"include/guild.hrl", guild},
        {"include/guild.hrl", guild_role},
        {"include/guild.hrl", guild_apply},
        {"include/lucky_money.hrl", lucky_money},
        {"include/lucky_money.hrl", lucky_money_role}
    ].
