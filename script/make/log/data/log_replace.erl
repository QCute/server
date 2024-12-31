%%%-------------------------------------------------------------------
%%% @doc
%%% log replace
%%% @end
%%%-------------------------------------------------------------------
-module(log_replace).
-export([replace/2]).
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc the log replace sql
-spec replace(Key :: atom(), Data :: list()) -> Sql :: binary().
replace(achievement_log, Data) ->
    db:collect(<<"REPLACE INTO (`id`, `role_id`, `achievement_id`, `time`) VALUES ">>, <<":1:, :2:, :3:, :4:">>, <<>>, Data, 0);
replace(asset_consume_log, Data) ->
    db:collect(<<"REPLACE INTO (`id`, `role_id`, `asset`, `asset_id`, `number`, `operation`, `from`, `time`) VALUES ">>, <<":1:, :2:, :3:, :4:, :5:, :6:, :7:, :8:">>, <<>>, Data, 0);
replace(asset_produce_log, Data) ->
    db:collect(<<"REPLACE INTO (`id`, `role_id`, `asset`, `asset_id`, `number`, `operation`, `from`, `time`) VALUES ">>, <<":1:, :2:, :3:, :4:, :5:, :6:, :7:, :8:">>, <<>>, Data, 0);
replace(auction_log, Data) ->
    db:collect(<<"REPLACE INTO (`id`, `auction_id`, `number`, `bid_number`, `price`, `role_id`, `role_name`, `server_id`, `time`) VALUES ">>, <<":1:, :2:, :3:, :4:, :5:, :6:, :7:, :8:, :9:">>, <<>>, Data, 0);
replace(bubble_log, Data) ->
    db:collect(<<"REPLACE INTO (`id`, `role_id`, `bubble_id`, `from`, `time`) VALUES ">>, <<":1:, :2:, :3:, :4:, :5:">>, <<>>, Data, 0);
replace(fashion_log, Data) ->
    db:collect(<<"REPLACE INTO (`id`, `role_id`, `fashion_id`, `from`, `time`) VALUES ">>, <<":1:, :2:, :3:, :4:, :5:">>, <<>>, Data, 0);
replace(item_consume_log, Data) ->
    db:collect(<<"REPLACE INTO (`id`, `role_id`, `item_id`, `operation`, `from`, `time`) VALUES ">>, <<":1:, :2:, :3:, :4:, :5:, :6:">>, <<>>, Data, 0);
replace(item_produce_log, Data) ->
    db:collect(<<"REPLACE INTO (`id`, `role_id`, `item_id`, `operation`, `from`, `time`) VALUES ">>, <<":1:, :2:, :3:, :4:, :5:, :6:">>, <<>>, Data, 0);
replace(login_log, Data) ->
    db:collect(<<"REPLACE INTO (`id`, `role_id`, `ip`, `device_id`, `login_time`, `online_time`, `logout_time`, `time`) VALUES ">>, <<":1:, :2:, :3:, :4:, :5:, :6:, :7:, :8:">>, <<>>, Data, 0);
replace(online_log, Data) ->
    db:collect(<<"REPLACE INTO (`id`, `total`, `online`, `hosting`, `hour`, `time`) VALUES ">>, <<":1:, :2:, :3:, :4:, :5:, :6:">>, <<>>, Data, 0);
replace(role_log, Data) ->
    db:collect(<<"REPLACE INTO (`id`, `role_id`, `exp`, `time`) VALUES ">>, <<":1:, :2:, :3:, :4:">>, <<>>, Data, 0);
replace(shop_log, Data) ->
    db:collect(<<"REPLACE INTO (`id`, `role_id`, `shop_id`, `number`, `time`) VALUES ">>, <<":1:, :2:, :3:, :4:, :5:">>, <<>>, Data, 0);
replace(task_log, Data) ->
    db:collect(<<"REPLACE INTO (`id`, `role_id`, `task_id`, `time`) VALUES ">>, <<":1:, :2:, :3:, :4:">>, <<>>, Data, 0);
replace(title_log, Data) ->
    db:collect(<<"REPLACE INTO (`id`, `role_id`, `title_id`, `from`, `time`) VALUES ">>, <<":1:, :2:, :3:, :4:, :5:">>, <<>>, Data, 0);
replace(total_login_log, Data) ->
    db:collect(<<"REPLACE INTO (`id`, `number`, `time`) VALUES ">>, <<":1:, :2:, :3:">>, <<>>, Data, 0);
replace(total_online_log, Data) ->
    db:collect(<<"REPLACE INTO (`id`, `role_id`, `online_time`, `time`) VALUES ">>, <<":1:, :2:, :3:, :4:">>, <<>>, Data, 0);
replace(_, _) ->
    <<>>.
