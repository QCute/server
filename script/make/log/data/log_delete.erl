%%%-------------------------------------------------------------------
%%% @doc
%%% log delete
%%% @end
%%%-------------------------------------------------------------------
-module(log_delete).
-export([delete/1]).
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc the log delete
-spec delete(Key :: atom()) -> AffectRows :: non_neg_integer().
delete(achievement_log) ->
    db:delete(<<"DELETE FROM `achievement_log` WHERE `time` < ~w LIMIT 1000">>, [time:now() - 259200]);
delete(asset_consume_log) ->
    db:delete(<<"DELETE FROM `asset_consume_log` WHERE `time` < ~w LIMIT 1000">>, [time:now() - 259200]);
delete(asset_produce_log) ->
    db:delete(<<"DELETE FROM `asset_produce_log` WHERE `time` < ~w LIMIT 1000">>, [time:now() - 259200]);
delete(auction_log) ->
    db:delete(<<"DELETE FROM `auction_log` WHERE `time` < ~w LIMIT 1000">>, [time:now() - 259200]);
delete(bubble_log) ->
    db:delete(<<"DELETE FROM `bubble_log` WHERE `time` < ~w LIMIT 1000">>, [time:now() - 259200]);
delete(fashion_log) ->
    db:delete(<<"DELETE FROM `fashion_log` WHERE `time` < ~w LIMIT 1000">>, [time:now() - 259200]);
delete(item_consume_log) ->
    db:delete(<<"DELETE FROM `item_consume_log` WHERE `time` < ~w LIMIT 1000">>, [time:now() - 259200]);
delete(item_produce_log) ->
    db:delete(<<"DELETE FROM `item_produce_log` WHERE `time` < ~w LIMIT 1000">>, [time:now() - 259200]);
delete(login_log) ->
    db:delete(<<"DELETE FROM `login_log` WHERE `time` < ~w LIMIT 1000">>, [time:now() - 259200]);
delete(online_log) ->
    db:delete(<<"DELETE FROM `online_log` WHERE `time` < ~w LIMIT 1000">>, [time:now() - 259200]);
delete(role_log) ->
    db:delete(<<"DELETE FROM `role_log` WHERE `time` < ~w LIMIT 1000">>, [time:now() - 259200]);
delete(shop_log) ->
    db:delete(<<"DELETE FROM `shop_log` WHERE `time` < ~w LIMIT 1000">>, [time:now() - 259200]);
delete(task_log) ->
    db:delete(<<"DELETE FROM `task_log` WHERE `time` < ~w LIMIT 1000">>, [time:now() - 259200]);
delete(title_log) ->
    db:delete(<<"DELETE FROM `title_log` WHERE `time` < ~w LIMIT 1000">>, [time:now() - 259200]);
delete(total_login_log) ->
    db:delete(<<"DELETE FROM `total_login_log` WHERE `time` < ~w LIMIT 1000">>, [time:now() - 259200]);
delete(total_online_log) ->
    db:delete(<<"DELETE FROM `total_online_log` WHERE `time` < ~w LIMIT 1000">>, [time:now() - 259200]);
delete(_) ->
    0.
