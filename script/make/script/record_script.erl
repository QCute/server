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
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    Record = [X || X <- record(), lists:member(filename:basename(element(1, X), ".hrl"), Keys)],
    Default = [{"include/" ++ string:join(string:replace(Key, "_data", "", trailing), "") ++ ".hrl", Key} || Key <- Keys],
    List = proplists:get_value(Record, [{[], Default}], Record),
    try
        io:format("~p~n", [record_maker:start(List)])
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?ERROR_STACKTRACE(Class, Reason, Stacktrace)
    end.

%%%===================================================================
%%% record data
%%%===================================================================
record() ->
    [
        {"include/role.hrl", role},
        {"include/asset.hrl", asset},
        {"include/vip.hrl", vip},
        {"include/count.hrl", count},
        {"include/item.hrl", item},
        {"include/item.hrl", item_data},
        {"include/task.hrl", task},
        {"include/task.hrl", task_data},
        {"include/achievement.hrl", achievement},
        {"include/achievement.hrl", achievement_data},
        {"include/shop.hrl", shop},
        {"include/shop.hrl", shop_data},
        {"include/mail.hrl", mail},
        {"include/friend.hrl", friend},
        {"include/skill.hrl", skill},
        {"include/skill.hrl", skill_data},
        {"include/buff.hrl", buff},
        {"include/buff.hrl", buff_data},
        {"include/fashion.hrl", fashion},
        {"include/fashion.hrl", fashion_data},
        {"include/title.hrl", title},
        {"include/title.hrl", title_data},
        {"include/bubble.hrl", bubble},
        {"include/bubble.hrl", bubble_data},
        {"include/sign.hrl", sign},
        {"include/sign.hrl", sign_data},
        {"include/daily.hrl", daily},
        {"include/daily.hrl", daily_data},
        {"include/daily.hrl", daily_active},
        {"include/daily.hrl", daily_active_data},
        {"include/key.hrl", key},
        {"include/key.hrl", key_award_data},
        {"include/recharge.hrl", recharge},
        {"include/recharge.hrl", recharge_data},
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
