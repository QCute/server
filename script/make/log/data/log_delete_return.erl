%%%-------------------------------------------------------------------
%%% @doc
%%% log delete return
%%% @end
%%%-------------------------------------------------------------------
-module(log_delete_return).
-export([delete_return/1]).
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc the log retain sql
-spec delete_return(Key :: atom()) -> NewList :: list().
delete_return(achievement_log) ->
    db:delete(<<"DELETE FROM `achievement_log` WHERE `time` < ~w LIMIT 1000 RETURNING *">>, [time:now() - 259200]);
delete_return(asset_consume_log) ->
    db:delete(<<"DELETE FROM `asset_consume_log` WHERE `time` < ~w LIMIT 1000 RETURNING *">>, [time:now() - 259200]);
delete_return(asset_produce_log) ->
    db:delete(<<"DELETE FROM `asset_produce_log` WHERE `time` < ~w LIMIT 1000 RETURNING *">>, [time:now() - 259200]);
delete_return(auction_log) ->
    db:delete(<<"DELETE FROM `auction_log` WHERE `time` < ~w LIMIT 1000 RETURNING *">>, [time:now() - 259200]);
delete_return(bubble_log) ->
    db:delete(<<"DELETE FROM `bubble_log` WHERE `time` < ~w LIMIT 1000 RETURNING *">>, [time:now() - 259200]);
delete_return(fashion_log) ->
    db:delete(<<"DELETE FROM `fashion_log` WHERE `time` < ~w LIMIT 1000 RETURNING *">>, [time:now() - 259200]);
delete_return(item_consume_log) ->
    db:delete(<<"DELETE FROM `item_consume_log` WHERE `time` < ~w LIMIT 1000 RETURNING *">>, [time:now() - 259200]);
delete_return(item_produce_log) ->
    db:delete(<<"DELETE FROM `item_produce_log` WHERE `time` < ~w LIMIT 1000 RETURNING *">>, [time:now() - 259200]);
delete_return(login_log) ->
    db:delete(<<"DELETE FROM `login_log` WHERE `time` < ~w LIMIT 1000 RETURNING *">>, [time:now() - 259200]);
delete_return(online_log) ->
    db:delete(<<"DELETE FROM `online_log` WHERE `time` < ~w LIMIT 1000 RETURNING *">>, [time:now() - 259200]);
delete_return(role_log) ->
    db:delete(<<"DELETE FROM `role_log` WHERE `time` < ~w LIMIT 1000 RETURNING *">>, [time:now() - 259200]);
delete_return(shop_log) ->
    db:delete(<<"DELETE FROM `shop_log` WHERE `time` < ~w LIMIT 1000 RETURNING *">>, [time:now() - 259200]);
delete_return(task_log) ->
    db:delete(<<"DELETE FROM `task_log` WHERE `time` < ~w LIMIT 1000 RETURNING *">>, [time:now() - 259200]);
delete_return(title_log) ->
    db:delete(<<"DELETE FROM `title_log` WHERE `time` < ~w LIMIT 1000 RETURNING *">>, [time:now() - 259200]);
delete_return(total_login_log) ->
    db:delete(<<"DELETE FROM `total_login_log` WHERE `time` < ~w LIMIT 1000 RETURNING *">>, [time:now() - 259200]);
delete_return(total_online_log) ->
    db:delete(<<"DELETE FROM `total_online_log` WHERE `time` < ~w LIMIT 1000 RETURNING *">>, [time:now() - 259200]);
delete_return(_) ->
    [].
