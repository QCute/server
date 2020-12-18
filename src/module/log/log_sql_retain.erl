%%%-------------------------------------------------------------------
%%% @doc
%%% log sql retain
%%% @end
%%%-------------------------------------------------------------------
-module(log_sql_retain).
-compile(nowarn_export_all).
-compile(export_all).
%%%===================================================================
%%% API functions
%%%===================================================================
sql() ->
    [
        {<<"SELECT * FROM `online_log` WHERE `time` < ~w LIMIT 1000">>, {<<"REPLACE INTO `online_log` (`id`, `all`, `online`, `hosting`, `hour`, `time`) VALUES ">>, <<"(~w, ~w, ~w, ~w, ~w, ~w)">>, <<";">>}, {<<"DELETE FROM `online_log` WHERE `id` IN (">>, <<"~w">>, <<")">>}, 2592000},
        {<<"SELECT * FROM `login_log` WHERE `time` < ~w LIMIT 1000">>, {<<"REPLACE INTO `login_log` (`id`, `role_id`, `ip`, `device_id`, `login_time`, `online_time`, `logout_time`, `time`) VALUES ">>, <<"(~w, ~w, '~s', '~s', ~w, ~w, ~w, ~w)">>, <<";">>}, {<<"DELETE FROM `login_log` WHERE `id` IN (">>, <<"~w">>, <<")">>}, 2592000},
        {<<"SELECT * FROM `role_log` WHERE `time` < ~w LIMIT 1000">>, {<<"REPLACE INTO `role_log` (`id`, `role_id`, `exp`, `time`) VALUES ">>, <<"(~w, ~w, ~w, ~w)">>, <<";">>}, {<<"DELETE FROM `role_log` WHERE `id` IN (">>, <<"~w">>, <<")">>}, 2592000},
        {<<"SELECT * FROM `item_produce_log` WHERE `time` < ~w LIMIT 1000">>, {<<"REPLACE INTO `item_produce_log` (`id`, `role_id`, `item_id`, `operation`, `from`, `time`) VALUES ">>, <<"(~w, ~w, ~w, '~w', '~w', ~w)">>, <<";">>}, {<<"DELETE FROM `item_produce_log` WHERE `id` IN (">>, <<"~w">>, <<")">>}, 2592000},
        {<<"SELECT * FROM `item_consume_log` WHERE `time` < ~w LIMIT 1000">>, {<<"REPLACE INTO `item_consume_log` (`id`, `role_id`, `item_id`, `operation`, `from`, `time`) VALUES ">>, <<"(~w, ~w, ~w, '~w', '~w', ~w)">>, <<";">>}, {<<"DELETE FROM `item_consume_log` WHERE `id` IN (">>, <<"~w">>, <<")">>}, 2592000},
        {<<"SELECT * FROM `shop_log` WHERE `time` < ~w LIMIT 1000">>, {<<"REPLACE INTO `shop_log` (`id`, `role_id`, `shop_id`, `number`, `time`) VALUES ">>, <<"(~w, ~w, ~w, ~w, ~w)">>, <<";">>}, {<<"DELETE FROM `shop_log` WHERE `id` IN (">>, <<"~w">>, <<")">>}, 2592000},
        {<<"SELECT * FROM `quest_log` WHERE `time` < ~w LIMIT 1000">>, {<<"REPLACE INTO `quest_log` (`id`, `role_id`, `quest_id`, `time`) VALUES ">>, <<"(~w, ~w, ~w, ~w)">>, <<";">>}, {<<"DELETE FROM `quest_log` WHERE `id` IN (">>, <<"~w">>, <<")">>}, 2592000},
        {<<"SELECT * FROM `title_log` WHERE `time` < ~w LIMIT 1000">>, {<<"REPLACE INTO `title_log` (`id`, `role_id`, `title_id`, `from`, `time`) VALUES ">>, <<"(~w, ~w, ~w, '~w', ~w)">>, <<";">>}, {<<"DELETE FROM `title_log` WHERE `id` IN (">>, <<"~w">>, <<")">>}, 2592000},
        {<<"SELECT * FROM `auction_log` WHERE `time` < ~w LIMIT 1000">>, {<<"REPLACE INTO `auction_log` (`id`, `auction_id`, `number`, `bid_number`, `price`, `role_id`, `role_name`, `server_id`, `time`) VALUES ">>, <<"(~w, ~w, ~w, ~w, ~w, ~w, '~s', ~w, ~w)">>, <<";">>}, {<<"DELETE FROM `auction_log` WHERE `id` IN (">>, <<"~w">>, <<")">>}, 2592000}
    ].
