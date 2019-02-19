%%%-------------------------------------------------------------------
%%% @doc
%%% module data script
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
    code:add_path("beam"),
    code:add_path("../beam"),
    code:add_path("../../beam"),
    code:add_path("../../../beam"),
    maker:save_param_list(T),
    List = [X || X <- sql(), string:str(element(1, X), Key) =/= 0],
    console:stack_trace(catch maker:start(fun sql_maker:parse/2, List)),
    ok;
main(_) ->
    io:format("invail argument~n").

%%%===================================================================
%%% sql data
%%%===================================================================
sql() ->
    [
        {"src/module/player/player_sql.erl", player, ["common.hrl", "player.hrl"]},
        {"src/module/item/item_sql.erl", item, ["common.hrl", "item.hrl"]},
        {"src/module/guild/guild_player_sql.erl", guild_player, ["common.hrl", "guild.hrl"]}, %% select all join all
        {"src/module/guild/guild_sql.erl", guild, ["common.hrl", "guild.hrl"]},               %% select all join all
        {"src/module/key/key_sql.erl", key, ["common.hrl", "key.hrl"]},                       %% select all join all
        {"src/module/quest/quest_sql.erl", quest, ["common.hrl", "quest.hrl"]},
        {"src/module/rank/rank_sql.erl", rank, ["common.hrl", "rank.hrl"]}                       
    ].