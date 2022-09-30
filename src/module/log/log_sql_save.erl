%%%-------------------------------------------------------------------
%%% @doc
%%% log sql save
%%% @end
%%%-------------------------------------------------------------------
-module(log_sql_save).
-export([sql/1]).
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc the log sql
-spec sql(Key :: atom()) -> {InsertSql :: binary(), ValueFormat :: binary()}.
sql(online_log) ->
    {<<"INSERT INTO `online_log` (`total`, `online`, `hosting`, `hour`, `time`) VALUES ">>, <<"(~w, ~w, ~w, ~w, ~w)">>};
sql(total_online_log) ->
    {<<"INSERT INTO `total_online_log` (`role_id`, `online_time`, `time`) VALUES ">>, <<"(~w, ~w, ~w)">>};
sql(login_log) ->
    {<<"INSERT INTO `login_log` (`role_id`, `ip`, `device_id`, `login_time`, `online_time`, `logout_time`, `time`) VALUES ">>, <<"(~w, '~s', '~s', ~w, ~w, ~w, ~w)">>};
sql(total_login_log) ->
    {<<"INSERT INTO `total_login_log` (`number`, `time`) VALUES ">>, <<"(~w, ~w)">>};
sql(role_log) ->
    {<<"INSERT INTO `role_log` (`role_id`, `exp`, `time`) VALUES ">>, <<"(~w, ~w, ~w)">>};
sql(item_produce_log) ->
    {<<"INSERT INTO `item_produce_log` (`role_id`, `item_id`, `operation`, `from`, `time`) VALUES ">>, <<"(~w, ~w, '~w', '~w', ~w)">>};
sql(item_consume_log) ->
    {<<"INSERT INTO `item_consume_log` (`role_id`, `item_id`, `operation`, `from`, `time`) VALUES ">>, <<"(~w, ~w, '~w', '~w', ~w)">>};
sql(shop_log) ->
    {<<"INSERT INTO `shop_log` (`role_id`, `shop_id`, `number`, `time`) VALUES ">>, <<"(~w, ~w, ~w, ~w)">>};
sql(task_log) ->
    {<<"INSERT INTO `task_log` (`role_id`, `task_id`, `time`) VALUES ">>, <<"(~w, ~w, ~w)">>};
sql(achievement_log) ->
    {<<"INSERT INTO `achievement_log` (`role_id`, `achievement_id`, `time`) VALUES ">>, <<"(~w, ~w, ~w)">>};
sql(fashion_log) ->
    {<<"INSERT INTO `fashion_log` (`role_id`, `fashion_id`, `from`, `time`) VALUES ">>, <<"(~w, ~w, '~w', ~w)">>};
sql(title_log) ->
    {<<"INSERT INTO `title_log` (`role_id`, `title_id`, `from`, `time`) VALUES ">>, <<"(~w, ~w, '~w', ~w)">>};
sql(bubble_log) ->
    {<<"INSERT INTO `bubble_log` (`role_id`, `bubble_id`, `from`, `time`) VALUES ">>, <<"(~w, ~w, '~w', ~w)">>};
sql(auction_log) ->
    {<<"INSERT INTO `auction_log` (`auction_id`, `number`, `bid_number`, `price`, `role_id`, `role_name`, `server_id`, `time`) VALUES ">>, <<"(~w, ~w, ~w, ~w, ~w, '~s', ~w, ~w)">>};
sql(asset_produce_log) ->
    {<<"INSERT INTO `asset_produce_log` (`role_id`, `asset`, `asset_id`, `operation`, `from`, `time`) VALUES ">>, <<"(~w, '~w', ~w, '~w', '~w', ~w)">>};
sql(asset_consume_log) ->
    {<<"INSERT INTO `asset_consume_log` (`role_id`, `asset`, `asset_id`, `operation`, `from`, `time`) VALUES ">>, <<"(~w, '~w', ~w, '~w', '~w', ~w)">>};
sql(_) ->
    {<<>>, <<>>}.
