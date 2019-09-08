%%%-------------------------------------------------------------------
%%% @doc
%%% module sql script
%%% @end
%%%-------------------------------------------------------------------
-module(sql_script).
-export([main/1]).
%% ------------------------ user guide -------------------------------
%% 1. fields property/comment specified
%% insert fields not contain auto_increment/(ignore)/char(0)/varchar(0) property
%% no (update) property, use primary key to update, update fields not contain auto_increment/(once)/(ignore)/char(0)/varchar(0) property
%% (select) select all fields by default
%% (delete) delete this row by default
%%
%% 2. update/delete group support
%% (update_???)/(delete_???)
%% sql group will group by same group name, multi group supported
%%
%% 3. extra shell param : (select/select join all data without key constraint)
%%     select all   (mean select whole table)
%%     join all     (mean select join whole table)
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
        {"src/module/account/account_sql.erl", account, ["account.hrl"]},
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
