%%%-------------------------------------------------------------------
%%% @doc
%%% log save
%%% @end
%%%-------------------------------------------------------------------
-module(log_save).
-export([save/2]).
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc the log save
-spec save(Key :: atom(), Binding :: list()) -> NewList :: list().
save(achievement_log, Binding) ->
    db:save_into(<<"INSERT INTO `achievement_log` (`role_id`, `achievement_id`, `time`) VALUES ">>, <<"(:1:, :2:, :3:)">>, <<>>, Binding, 0);
save(asset_consume_log, Binding) ->
    db:save_into(<<"INSERT INTO `asset_consume_log` (`role_id`, `asset`, `asset_id`, `number`, `operation`, `from`, `time`) VALUES ">>, <<"(:1:, :2:, :3:, :4:, :5:, :6:, :7:)">>, <<>>, Binding, 0);
save(asset_produce_log, Binding) ->
    db:save_into(<<"INSERT INTO `asset_produce_log` (`role_id`, `asset`, `asset_id`, `number`, `operation`, `from`, `time`) VALUES ">>, <<"(:1:, :2:, :3:, :4:, :5:, :6:, :7:)">>, <<>>, Binding, 0);
save(auction_log, Binding) ->
    db:save_into(<<"INSERT INTO `auction_log` (`auction_id`, `number`, `bid_number`, `price`, `role_id`, `role_name`, `server_id`, `time`) VALUES ">>, <<"(:1:, :2:, :3:, :4:, :5:, :6:, :7:, :8:)">>, <<>>, Binding, 0);
save(bubble_log, Binding) ->
    db:save_into(<<"INSERT INTO `bubble_log` (`role_id`, `bubble_id`, `from`, `time`) VALUES ">>, <<"(:1:, :2:, :3:, :4:)">>, <<>>, Binding, 0);
save(fashion_log, Binding) ->
    db:save_into(<<"INSERT INTO `fashion_log` (`role_id`, `fashion_id`, `from`, `time`) VALUES ">>, <<"(:1:, :2:, :3:, :4:)">>, <<>>, Binding, 0);
save(item_consume_log, Binding) ->
    db:save_into(<<"INSERT INTO `item_consume_log` (`role_id`, `item_id`, `operation`, `from`, `time`) VALUES ">>, <<"(:1:, :2:, :3:, :4:, :5:)">>, <<>>, Binding, 0);
save(item_produce_log, Binding) ->
    db:save_into(<<"INSERT INTO `item_produce_log` (`role_id`, `item_id`, `operation`, `from`, `time`) VALUES ">>, <<"(:1:, :2:, :3:, :4:, :5:)">>, <<>>, Binding, 0);
save(login_log, Binding) ->
    db:save_into(<<"INSERT INTO `login_log` (`role_id`, `ip`, `device_id`, `login_time`, `online_time`, `logout_time`, `time`) VALUES ">>, <<"(:1:, :2:, :3:, :4:, :5:, :6:, :7:)">>, <<>>, Binding, 0);
save(online_log, Binding) ->
    db:save_into(<<"INSERT INTO `online_log` (`total`, `online`, `hosting`, `hour`, `time`) VALUES ">>, <<"(:1:, :2:, :3:, :4:, :5:)">>, <<>>, Binding, 0);
save(role_log, Binding) ->
    db:save_into(<<"INSERT INTO `role_log` (`role_id`, `exp`, `time`) VALUES ">>, <<"(:1:, :2:, :3:)">>, <<>>, Binding, 0);
save(shop_log, Binding) ->
    db:save_into(<<"INSERT INTO `shop_log` (`role_id`, `shop_id`, `number`, `time`) VALUES ">>, <<"(:1:, :2:, :3:, :4:)">>, <<>>, Binding, 0);
save(task_log, Binding) ->
    db:save_into(<<"INSERT INTO `task_log` (`role_id`, `task_id`, `time`) VALUES ">>, <<"(:1:, :2:, :3:)">>, <<>>, Binding, 0);
save(title_log, Binding) ->
    db:save_into(<<"INSERT INTO `title_log` (`role_id`, `title_id`, `from`, `time`) VALUES ">>, <<"(:1:, :2:, :3:, :4:)">>, <<>>, Binding, 0);
save(total_login_log, Binding) ->
    db:save_into(<<"INSERT INTO `total_login_log` (`number`, `time`) VALUES ">>, <<"(:1:, :2:)">>, <<>>, Binding, 0);
save(total_online_log, Binding) ->
    db:save_into(<<"INSERT INTO `total_online_log` (`role_id`, `online_time`, `time`) VALUES ">>, <<"(:1:, :2:, :3:)">>, <<>>, Binding, 0);
save(_, Binding) ->
    Binding.
