%%%-------------------------------------------------------------------
%%! +pc unicode
%%% @doc
%%% record script for record maker
%%% @end
%%%-------------------------------------------------------------------
-module(record_script).
-export([main/1]).
-include("../../../include/journal.hrl").
%% ------------------------ user guide -------------------------------
%%
%% default value guide
%% tinyint/smallint/int/bigint                         => your sql default value
%% varchar                                             => []
%% char                                                => <<>>
%%
%%%===================================================================
%%% API functions
%%%===================================================================
main(Keys) ->
    io:setopts([{encoding, unicode}]),
    io:setopts(standard_error, [{encoding, unicode}]),
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    Record = [X || X <- record(), lists:member(filename:basename(maps:get(file, X), ".hrl"), Keys)],
    try
        io:format("~tp~n", [record_maker:start(Record)])
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?HALT(Class, Reason, Stacktrace)
    end.

%%%===================================================================
%%% record data
%%%===================================================================
record() ->
    [
        #{file => "include/role.hrl", table => role},
        #{file => "include/asset.hrl", table => asset},
        #{file => "include/vip.hrl", table => vip},
        #{file => "include/count.hrl", table => count},
        #{file => "include/item.hrl", table => item},
        #{file => "include/item.hrl", table => item_data},
        #{file => "include/task.hrl", table => task},
        #{file => "include/task.hrl", table => task_data},
        #{file => "include/achievement.hrl", table => achievement},
        #{file => "include/achievement.hrl", table => achievement_data},
        #{file => "include/shop.hrl", table => shop},
        #{file => "include/shop.hrl", table => shop_data},
        #{file => "include/mail.hrl", table => mail},
        #{file => "include/friend.hrl", table => friend},
        #{file => "include/skill.hrl", table => skill},
        #{file => "include/skill.hrl", table => skill_data},
        #{file => "include/buff.hrl", table => buff},
        #{file => "include/buff.hrl", table => buff_data},
        #{file => "include/fashion.hrl", table => fashion},
        #{file => "include/fashion.hrl", table => fashion_data},
        #{file => "include/title.hrl", table => title},
        #{file => "include/title.hrl", table => title_data},
        #{file => "include/bubble.hrl", table => bubble},
        #{file => "include/bubble.hrl", table => bubble_data},
        #{file => "include/sign.hrl", table => sign},
        #{file => "include/sign.hrl", table => sign_data},
        #{file => "include/daily.hrl", table => daily},
        #{file => "include/daily.hrl", table => daily_data},
        #{file => "include/daily.hrl", table => daily_active},
        #{file => "include/daily.hrl", table => daily_active_data},
        #{file => "include/key.hrl", table => key},
        #{file => "include/key.hrl", table => key_award_data},
        #{file => "include/charge.hrl", table => charge},
        #{file => "include/charge.hrl", table => charge_data},
        #{file => "include/activity.hrl", table => activity_data},
        #{file => "include/auction.hrl", table => auction},
        #{file => "include/auction.hrl", table => auction_role},
        #{file => "include/auction.hrl", table => auction_data},
        #{file => "include/map.hrl", table => map_data},
        #{file => "include/monster.hrl", table => monster_data},
        #{file => "include/dungeon.hrl", table => dungeon},
        #{file => "include/dungeon.hrl", table => dungeon_data},
        #{file => "include/rank.hrl", table => rank},
        #{file => "include/guild.hrl", table => guild},
        #{file => "include/guild.hrl", table => guild_role},
        #{file => "include/guild.hrl", table => guild_apply},
        #{file => "include/lucky_money.hrl", table => lucky_money},
        #{file => "include/lucky_money.hrl", table => lucky_money_role}
    ].
