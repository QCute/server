%%%-------------------------------------------------------------------
%%% @doc
%%% module sql script
%%% @end
%%%-------------------------------------------------------------------
-module(sql_script).
-export([main/1]).
%% ------------------------ user guide -------------------------------
%%
%% extra shell param : (select/select join all data without key constraint)
%%     select all
%%     join all
%%
%%%===================================================================
%%% API
%%%===================================================================
main([Key | T]) ->
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    maker:save_param_list(T),
    List = [X || X <- sql(), filename:basename(element(1, X), ".erl") == Key],
    console:stacktrace(catch maker:start(fun sql_maker:parse/2, List)),
    ok;
main(_) ->
    io:format("invail argument~n").

%%%===================================================================
%%% sql data
%%%===================================================================
sql() ->
    [
        {"src/module/role/role_sql.erl", role, ["role.hrl"]},
        {"src/module/role/role_assets_sql.erl", assets, ["assets.hrl"]},
        {"src/module/item/item_sql.erl", item, ["item.hrl"]},
        {"src/module/guild/guild_sql.erl", guild, ["guild.hrl"]},                               %% select all join all
        {"src/module/guild/guild_role_sql.erl", guild_role, ["guild.hrl"]},                     %% select all join all
        {"src/module/guild/guild_apply_sql.erl", guild_apply, ["guild.hrl"]},                   %% select all join all
        {"src/module/key/key_sql.erl", key, ["key.hrl"]},                                       %% select all join all
        {"src/module/quest/quest_sql.erl", quest, ["quest.hrl"]},
        {"src/module/rank/rank_sql.erl", rank, ["rank.hrl"]},
        {"src/module/mail/mail_sql.erl", mail, ["mail.hrl"]},
        {"src/module/shop/shop_sql.erl", shop, ["shop.hrl"]}
    ].