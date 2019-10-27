%%%-------------------------------------------------------------------
%%% @doc
%%% module record script
%%% @end
%%%-------------------------------------------------------------------
-module(record_script).
-export([main/1]).
%% ------------------------ user guide -------------------------------
%%
%% default value guide
%% tinyint/smallint/int/bigint                         => your sql default value
%% varchar(0)                                          => undefined
%% varchar                                             => []
%% char/char(0)                                        => <<>>
%% text                                                => <<>>
%% varchar/char with default(value) in comment         => value
%% 
%%%===================================================================
%%% API
%%%===================================================================
main([Key]) ->
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    List = [X || X <- record(), filename:basename(element(1, X), ".hrl") == Key],
    console:stacktrace(catch record_maker:start(List));
main(_) ->
    io:format("invalid argument~n").

%%%===================================================================
%%% record data
%%%===================================================================
record() ->
    [
        {"include/role.hrl", role},
        {"include/asset.hrl", asset},
        {"include/vip.hrl", vip},
        {"include/item.hrl", item},
        {"include/item.hrl", item_data},
        {"include/guild.hrl", guild},
        {"include/guild.hrl", guild_role},
        {"include/guild.hrl", guild_apply},
        {"include/rank.hrl", rank},
        {"include/quest.hrl", quest},
        {"include/quest.hrl", quest_data},
        {"include/mail.hrl", mail},
        {"include/shop.hrl", shop},
        {"include/shop.hrl", shop_data},
        {"include/friend.hrl", friend},
        {"include/key.hrl", key_award_data},
        {"include/skill.hrl", skill},
        {"include/skill.hrl", skill_data},
        {"include/buff.hrl", buff},
        {"include/buff.hrl", buff_data},
        {"include/activity.hrl", activity_data},
        {"include/auction.hrl", auction},
        {"include/auction.hrl", auction_data},
        {"include/count.hrl", count},
        {"include/monster.hrl", monster_data}
    ].
