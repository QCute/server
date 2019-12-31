%%%------------------------------------------------------------------
%%% @doc
%%% module log sql
%%% @end
%%%------------------------------------------------------------------
-module(log_sql).
-compile(nowarn_export_all).
-compile(export_all).
%%%==================================================================
%%% API functions
%%%==================================================================
sql(online_log) ->
    {<<"INSERT INTO `online_log` (`time`, `hour`, `all`, `online`, `hosting`) VALUES ">>, <<"('~w', '~w', '~w', '~w', '~w')">>};
sql(role_log) ->
    {<<"INSERT INTO `role_log` (`role_id`, `exp`, `time`, `daily_time`) VALUES ">>, <<"('~w', '~w', '~w', '~w')">>};
sql(item_produce_log) ->
    {<<"INSERT INTO `item_produce_log` (`role_id`, `item_id`, `operation`, `source`, `time`, `daily_time`) VALUES ">>, <<"('~w', '~w', '~w', '~w', '~w', '~w')">>};
sql(item_consume_log) ->
    {<<"INSERT INTO `item_consume_log` (`role_id`, `item_id`, `operation`, `source`, `time`, `daily_time`) VALUES ">>, <<"('~w', '~w', '~w', '~w', '~w', '~w')">>};
sql(shop_log) ->
    {<<"INSERT INTO `shop_log` (`role_id`, `shop_id`, `number`, `time`, `daily_time`) VALUES ">>, <<"('~w', '~w', '~w', '~w', '~w')">>};
sql(quest_log) ->
    {<<"INSERT INTO `quest_log` (`role_id`, `quest_id`, `time`, `daily_time`) VALUES ">>, <<"('~w', '~w', '~w', '~w')">>};
sql(auction_log) ->
    {<<"INSERT INTO `auction_log` (`auction_id`, `number`, `bid_number`, `price`, `bidder_id`, `bidder_name`, `bidder_server_id`, `time`, `daily_time`) VALUES ">>, <<"('~w', '~w', '~w', '~w', '~w', '~s', '~w', '~w', '~w')">>};
sql(_) ->
    {<<>>, <<>>}.
