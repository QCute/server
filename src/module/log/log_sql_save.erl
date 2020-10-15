%%%-------------------------------------------------------------------
%%% @doc
%%% log sql save
%%% @end
%%%-------------------------------------------------------------------
-module(log_sql_save).
-compile(nowarn_export_all).
-compile(export_all).
%%%===================================================================
%%% API functions
%%%===================================================================
sql(online_log) ->
    {<<"INSERT INTO `online_log` (`all`, `online`, `hosting`, `hour`, `time`) VALUES ">>, <<"(~w, ~w, ~w, ~w, ~w)">>};
sql(total_login_log) ->
    {<<"INSERT INTO `total_login_log` (`total`, `hour_list`, `time`) VALUES ">>, <<"(~w, '~w', ~w)">>};
sql(login_log) ->
    {<<"INSERT INTO `login_log` (`role_id`, `ip`, `device_id`, `login_time`, `online_time`, `time`) VALUES ">>, <<"(~w, '~w', '~w', ~w, ~w, ~w)">>};
sql(role_log) ->
    {<<"INSERT INTO `role_log` (`role_id`, `exp`, `time`) VALUES ">>, <<"(~w, ~w, ~w)">>};
sql(item_produce_log) ->
    {<<"INSERT INTO `item_produce_log` (`role_id`, `item_id`, `operation`, `source`, `time`) VALUES ">>, <<"(~w, ~w, '~w', '~w', ~w)">>};
sql(item_consume_log) ->
    {<<"INSERT INTO `item_consume_log` (`role_id`, `item_id`, `operation`, `source`, `time`) VALUES ">>, <<"(~w, ~w, '~w', '~w', ~w)">>};
sql(shop_log) ->
    {<<"INSERT INTO `shop_log` (`role_id`, `shop_id`, `number`, `time`) VALUES ">>, <<"(~w, ~w, ~w, ~w)">>};
sql(quest_log) ->
    {<<"INSERT INTO `quest_log` (`role_id`, `quest_id`, `time`) VALUES ">>, <<"(~w, ~w, ~w)">>};
sql(title_log) ->
    {<<"INSERT INTO `title_log` (`role_id`, `title_id`, `from`, `time`) VALUES ">>, <<"(~w, ~w, '~w', ~w)">>};
sql(auction_log) ->
    {<<"INSERT INTO `auction_log` (`auction_id`, `number`, `bid_number`, `price`, `role_id`, `role_name`, `server_id`, `time`) VALUES ">>, <<"(~w, ~w, ~w, ~w, ~w, '~s', ~w, ~w)">>};
sql(_) ->
    {<<>>, <<>>}.
