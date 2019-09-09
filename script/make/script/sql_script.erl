%%%-------------------------------------------------------------------
%%% @doc
%%% module sql script
%%% @end
%%%-------------------------------------------------------------------
-module(sql_script).
-export([main/1]).
%% ------------------------ user guide -------------------------------
%%
%% * insert:
%%     insert fields not contain auto_increment/char(0)/varchar(0) property
%%     insert update code will auto make when (flag) in comment
%% * select:
%%     select all fields and use primary key by default, (select) in comment will use it replace primary
%%     use join(`table`.`field`) to make select join outer table code
%% * update:
%%     update fields not contain auto_increment/char(0)/varchar(0)/(once) property
%%     update row and use primary key by default, (update) in comment will use it replace primary
%%     use (update_???) make fields update group
%% * delete:
%%     delete row and use primary key by default, (delete) in comment will use it replace primary
%%     use (delete_???) make keys delete group
%%     auto_increment in table, will auto make delete in code by this key
%% * extra mode:
%%     use {select, all} will make select code without key filter
%%     use {join, all} will make select join code without key filter
%%
%%%===================================================================
%%% API
%%%===================================================================
main([Key | T]) ->
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    maker:save_param_list(T),
    List = [X || X <- sql(), filename:basename(element(1, X), ".erl") == Key],
    console:stacktrace(catch sql_maker:start(List));
main(_) ->
    io:format("invail argument~n").

%%%===================================================================
%%% sql data
%%%===================================================================
sql() ->
    [
        {"src/module/role/role_sql.erl", role, ["role.hrl"]},
        {"src/module/asset/asset_sql.erl", asset, ["asset.hrl"]},
        {"src/module/item/item_sql.erl", item, ["item.hrl"]},
        {"src/module/guild/guild_sql.erl", guild, ["guild.hrl"], [{select, []}, {join, []}]},
        {"src/module/guild/guild_role_sql.erl", guild_role, ["guild.hrl"], [{select, []}, {join, []}]},
        {"src/module/guild/guild_apply_sql.erl", guild_apply, ["guild.hrl"], [{select, []}, {join, []}]},
        {"src/module/key/key_sql.erl", key, ["key.hrl"], [{select, []}]},
        {"src/module/quest/quest_sql.erl", quest, ["quest.hrl"]},
        {"src/module/rank/rank_sql.erl", rank, ["rank.hrl"]},
        {"src/module/mail/mail_sql.erl", mail, ["mail.hrl"]},
        {"src/module/shop/shop_sql.erl", shop, ["shop.hrl"]},
        {"src/module/friend/friend_sql.erl", friend, ["friend.hrl"]},
        {"src/module/vip/vip_sql.erl", vip, ["vip.hrl"]},
        {"src/module/auction/auction_sql.erl", auction, ["auction.hrl"], [{select, []}, {join, []}]},
        {"src/module/skill/skill_sql.erl", skill, ["skill.hrl"]},
        {"src/module/buff/buff_sql.erl", buff, ["buff.hrl"]}
    ].
