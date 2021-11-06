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
        {<<"DELETE FROM `online_log` WHERE `time` < ~w LIMIT 1000 RETURNING *">>, {<<"REPLACE INTO `online_log` (`id`, `total`, `online`, `hosting`, `hour`, `time`) VALUES ">>, <<"(~w, ~w, ~w, ~w, ~w, ~w)">>, <<";">>}, 2592000},
        {<<"DELETE FROM `login_log` WHERE `time` < ~w LIMIT 1000 RETURNING *">>, {<<"REPLACE INTO `login_log` (`id`, `role_id`, `ip`, `device_id`, `login_time`, `online_time`, `logout_time`, `time`) VALUES ">>, <<"(~w, ~w, '~s', '~s', ~w, ~w, ~w, ~w)">>, <<";">>}, 2592000},
        {<<"DELETE FROM `role_log` WHERE `time` < ~w LIMIT 1000 RETURNING *">>, {<<"REPLACE INTO `role_log` (`id`, `role_id`, `exp`, `time`) VALUES ">>, <<"(~w, ~w, ~w, ~w)">>, <<";">>}, 2592000},
        {<<"DELETE FROM `item_produce_log` WHERE `time` < ~w LIMIT 1000 RETURNING *">>, {<<"REPLACE INTO `item_produce_log` (`id`, `role_id`, `item_id`, `operation`, `from`, `time`) VALUES ">>, <<"(~w, ~w, ~w, '~w', '~w', ~w)">>, <<";">>}, 2592000},
        {<<"DELETE FROM `item_consume_log` WHERE `time` < ~w LIMIT 1000 RETURNING *">>, {<<"REPLACE INTO `item_consume_log` (`id`, `role_id`, `item_id`, `operation`, `from`, `time`) VALUES ">>, <<"(~w, ~w, ~w, '~w', '~w', ~w)">>, <<";">>}, 2592000},
        {<<"DELETE FROM `shop_log` WHERE `time` < ~w LIMIT 1000 RETURNING *">>, {<<"REPLACE INTO `shop_log` (`id`, `role_id`, `shop_id`, `number`, `time`) VALUES ">>, <<"(~w, ~w, ~w, ~w, ~w)">>, <<";">>}, 2592000},
        {<<"DELETE FROM `task_log` WHERE `time` < ~w LIMIT 1000 RETURNING *">>, {<<"REPLACE INTO `task_log` (`id`, `role_id`, `task_id`, `time`) VALUES ">>, <<"(~w, ~w, ~w, ~w)">>, <<";">>}, 2592000},
        {<<"DELETE FROM `achievement_log` WHERE `time` < ~w LIMIT 1000 RETURNING *">>, {<<"REPLACE INTO `achievement_log` (`id`, `role_id`, `achievement_id`, `time`) VALUES ">>, <<"(~w, ~w, ~w, ~w)">>, <<";">>}, 2592000},
        {<<"DELETE FROM `fashion_log` WHERE `time` < ~w LIMIT 1000 RETURNING *">>, {<<"REPLACE INTO `fashion_log` (`id`, `role_id`, `fashion_id`, `from`, `time`) VALUES ">>, <<"(~w, ~w, ~w, '~w', ~w)">>, <<";">>}, 2592000},
        {<<"DELETE FROM `title_log` WHERE `time` < ~w LIMIT 1000 RETURNING *">>, {<<"REPLACE INTO `title_log` (`id`, `role_id`, `title_id`, `from`, `time`) VALUES ">>, <<"(~w, ~w, ~w, '~w', ~w)">>, <<";">>}, 2592000},
        {<<"DELETE FROM `bubble_log` WHERE `time` < ~w LIMIT 1000 RETURNING *">>, {<<"REPLACE INTO `bubble_log` (`id`, `role_id`, `bubble_id`, `from`, `time`) VALUES ">>, <<"(~w, ~w, ~w, '~w', ~w)">>, <<";">>}, 2592000},
        {<<"DELETE FROM `auction_log` WHERE `time` < ~w LIMIT 1000 RETURNING *">>, {<<"REPLACE INTO `auction_log` (`id`, `auction_id`, `number`, `bid_number`, `price`, `role_id`, `role_name`, `server_id`, `time`) VALUES ">>, <<"(~w, ~w, ~w, ~w, ~w, ~w, '~s', ~w, ~w)">>, <<";">>}, 2592000}
    ].
